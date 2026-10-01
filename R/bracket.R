#' Query a duckdt handle using data.table syntax
#'
#' Translates `i` (row filter), `j` (column select/compute), and `by`
#' (grouping) into a single SQL query executed in DuckDB, and returns the
#' result as a `data.table`. `j` may also be a `:=` call, in which case the
#' underlying DuckDB table is mutated instead (see the write-path notes in
#' the package README); this requires a materialized, writable table (see
#' [as.dbdt()]'s `copy` argument and [dbdt()]'s `writable` argument).
#'
#' Because this returns a `data.table`, the result has left the database --
#' it can no longer be used where a `"duckdt"` handle is expected (e.g. as an
#' argument to [dbdt_join()] or [dbdt_merge()]). Use [dbdt_temp()] to
#' run the same `i`/`j`/`by` query into a temporary table and get a handle
#' back instead, keeping the rows in DuckDB.
#'
#' Binary columns (`BLOB`/`BIT` on DuckDB, `varbinary`/`binary`/`image` on
#' MS SQL Server) are left out when `j` is missing: no driver hands them back
#' as an R vector, and asking for one fails the whole query rather than just
#' that column, so a single binary column would otherwise make the table
#' unreadable. Naming one in `j` (`d[, .(payload)]`) still selects it, and
#' nothing that stays in the database -- `:=`, [dbdt_temp()],
#' [dbdt_merge()] -- is affected.
#'
#' @param x A `"duckdt"` object.
#' @param i Optional row filter, e.g. `cyl == 6`, or `order(...)` to sort
#'   the rows (`-x` or `decreasing = TRUE` for descending; missing values
#'   last, as in R). Sorting can't be combined with `by`.
#' @param j Optional column selection/computation via `.(...)`/`list(...)`,
#'   a bare column name, or a `:=` assignment.
#' @param by Optional grouping: a bare column name, character vector, or
#'   `.(...)`/`list(...)`/`c(...)`.
#' @param ... Unused.
#'
#' @return A `data.table` (or, for `:=`, the input `x` invisibly).
#' @examples
#' d <- as.dbdt(datasets::mtcars)
#' d[cyl == 6]
#' d[cyl == 6, .(mpg, hp)]
#' d[, .(avg_mpg = mean(mpg), n = .N), by = cyl]
#' @export
`[.duckdt` <- function(x, i, j, by, ...) {
  has_i <- !missing(i)
  has_j <- !missing(j)
  has_by <- !missing(by)

  ie <- if (has_i) substitute(i)
  je <- if (has_j) substitute(j)
  bye <- if (has_by) substitute(by)

  env <- parent.frame()

  if (has_j && is.call(je) && identical(je[[1]], as.name(":="))) {
    return(duckdt_mutate(x, ie, je, has_i, has_by, duckdt_columns(x), env))
  }

  sql <- duckdt_select_sql(x, ie, je, bye, has_i, has_j, has_by, env)

  # `[]` after setDT() works around a well-known data.table quirk: setDT()
  # marks the object so its *next* auto-print at the top level is silently
  # skipped (the same mechanism `:=` uses). `[]` forces a normal print-able
  # copy so `d[cyl == 6]` at the console (or in a function returning this
  # value) actually prints instead of appearing to do nothing.
  data.table::setDT(duckdt_get_query(x$conn, sql))[]
}

# Internal: render an `i`/`j`/`by` query as a SELECT statement, without
# running it. Shared by `[.duckdt` (which executes it into a data.table) and
# duckdt_temp() (which executes it into a temporary table), so both spell the
# same data.table syntax the same way. They differ in one place: `to_r`
# expands a bare `*` to the columns R can actually receive (see
# binary-cols.R), which duckdt_temp() -- whose rows never leave the database
# -- turns off.
duckdt_select_sql <- function(x, ie, je, bye, has_i, has_j, has_by, env, to_r = TRUE) {
  conn <- x$conn
  cols <- duckdt_columns(x)
  dialect <- duckdt_dialect(conn)

  # `d[order(x)]` sorts rather than filters. Grouping would sort the groups
  # by where each first appears, which SQL has no way to say.
  order_sql <- NULL
  if (has_i && is.call(ie) && identical(ie[[1]], as.name("order"))) {
    if (has_by) {
      stop("duckdt: order() in `i` can't be combined with `by`; sort the result instead, ",
        "e.g. `d[, .(n = .N), by = g][order(-n)]`.", call. = FALSE)
    }
    order_sql <- translate_order(ie, cols, env, conn, dialect)
    has_i <- FALSE
  }
  where_sql <- if (has_i) translate_condition(ie, cols, env, conn, dialect) else NULL
  by_res <- if (has_by) translate_by(bye, cols, env, conn, dialect) else NULL

  # On SQL Server, median() in `j` is computed as a window function in a
  # subquery; translate_median() records each one in `tx$windows`.
  tx <- new.env(parent = emptyenv())
  tx$windows <- character(0)
  if (dialect == "mssql") {
    duckdt_env$tx <- tx
    on.exit(duckdt_env$tx <- NULL, add = TRUE)
  }
  sel_res <- if (has_j) translate_select(je, cols, env, conn, dialect) else NULL
  duckdt_env$tx <- NULL

  parts <- character(0)
  if (!is.null(by_res)) parts <- c(parts, by_res$parts)
  if (!is.null(sel_res)) {
    j_parts <- sel_res$parts
    if (!is.null(by_res)) {
      dup <- sel_res$aliases %in% by_res$aliases
      j_parts <- j_parts[!dup]
    }
    parts <- c(parts, j_parts)
  }
  if (length(parts) == 0) parts <- if (to_r) duckdt_star(x, cols) else "*"

  from <- duckdt_qtbl(x)
  if (length(tx$windows)) {
    # Each row carries its group's median, computed over the rows the WHERE
    # clause keeps; the outer query aggregates as usual and picks it up with
    # MAX(). The group expressions read the same columns either way.
    over <- if (is.null(by_res)) "" else paste0("PARTITION BY ", paste(by_res$group_parts, collapse = ", "))
    windows <- paste0(
      "PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", tx$windows, ") OVER (", over, ") AS ",
      DBI::dbQuoteIdentifier(conn, names(tx$windows))
    )
    inner <- paste0("SELECT *, ", paste(windows, collapse = ", "), " FROM ", from)
    if (!is.null(where_sql)) inner <- paste0(inner, " WHERE ", where_sql)
    from <- paste0("(", inner, ") AS duckdt_w")
    where_sql <- NULL
  }

  sql <- paste0("SELECT ", paste(parts, collapse = ", "), " FROM ", from)
  if (!is.null(where_sql)) sql <- paste0(sql, " WHERE ", where_sql)
  if (!is.null(by_res)) sql <- paste0(sql, " GROUP BY ", paste(by_res$group_parts, collapse = ", "))
  # A table has no row order, so a query run into one (dbdt_temp()) isn't
  # sorted -- and SQL Server rejects ORDER BY in the derived table it uses.
  if (!is.null(order_sql) && to_r) sql <- paste0(sql, " ORDER BY ", paste(order_sql, collapse = ", "))
  sql
}

#' Show the SQL a query would run
#'
#' Translates `d[i, j, by]` exactly as `[` would, and returns the SQL instead
#' of running it. Use it to see how an expression was translated, or to take
#' a slow query to a tool that shows its execution plan (SQL Server
#' Management Studio, DuckDB's `EXPLAIN ANALYZE`).
#'
#' To watch every statement duckdt sends, with how long each took, set
#' `options(duckdt.trace = TRUE)`.
#'
#' @param x A `"duckdt"` object.
#' @param i,j,by As in `[.duckdt`.
#'
#' @return The SQL, as a [DBI::SQL()] string.
#' @seealso [dbdt_temp()] to run the query into a temporary table instead.
#' @examples
#' d <- as.dbdt(datasets::mtcars)
#' dbdt_sql(d, cyl == 6 & hp > 100, .(n = .N, mpg = mean(mpg)), by = gear)
#' @export
duckdt_sql <- function(x, i, j, by) {
  if (!inherits(x, "duckdt")) {
    stop("duckdt_sql: `x` must be a duckdt object.", call. = FALSE)
  }
  has_i <- !missing(i)
  has_j <- !missing(j)
  has_by <- !missing(by)
  ie <- if (has_i) substitute(i)
  je <- if (has_j) substitute(j)
  bye <- if (has_by) substitute(by)
  if (has_j && is.call(je) && identical(je[[1]], as.name(":="))) {
    stop("duckdt_sql: `:=` writes rather than queries; there is no single SELECT to show.",
      call. = FALSE)
  }
  DBI::SQL(duckdt_select_sql(x, ie, je, bye, has_i, has_j, has_by, parent.frame()))
}
