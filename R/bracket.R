#' Query a duckdt handle using data.table syntax
#'
#' Translates `i` (row filter), `j` (column select/compute), and `by`
#' (grouping) into a single SQL query executed in DuckDB, and returns the
#' result as a `data.table`. `j` may also be a `:=` call, in which case the
#' underlying DuckDB table is mutated instead (see the write-path notes in
#' the package README); this requires a materialized table (see
#' [as.duckdt()]'s `copy` argument).
#'
#' @param x A `"duckdt"` object.
#' @param i Optional row filter, e.g. `cyl == 6`.
#' @param j Optional column selection/computation via `.(...)`/`list(...)`,
#'   a bare column name, or a `:=` assignment.
#' @param by Optional grouping: a bare column name, character vector, or
#'   `.(...)`/`list(...)`/`c(...)`.
#' @param ... Unused.
#'
#' @return A `data.table` (or, for `:=`, the input `x` invisibly).
#' @export
`[.duckdt` <- function(x, i, j, by, ...) {
  has_i <- !missing(i)
  has_j <- !missing(j)
  has_by <- !missing(by)

  ie <- if (has_i) substitute(i)
  je <- if (has_j) substitute(j)
  bye <- if (has_by) substitute(by)

  env <- parent.frame()
  conn <- x$conn
  cols <- duckdt_columns(x)
  dialect <- duckdt_dialect(conn)

  if (has_j && is.call(je) && identical(je[[1]], as.name(":="))) {
    return(duckdt_mutate(x, ie, je, has_i, has_by, cols, env))
  }

  where_sql <- if (has_i) translate_expr(ie, cols, env, conn, dialect) else NULL
  by_res <- if (has_by) translate_by(bye, cols, env, conn, dialect) else NULL
  sel_res <- if (has_j) translate_select(je, cols, env, conn, dialect) else NULL

  parts <- character(0)
  if (!is.null(by_res)) parts <- c(parts, by_res$parts)
  if (!is.null(sel_res)) {
    j_parts <- sel_res$parts
    if (!is.null(by_res)) {
      dup <- sel_res$aliases %in% by_res$aliases
      j_parts <- j_parts[!dup]
    }
    parts <- c(parts, j_parts)
  } else if (is.null(by_res)) {
    parts <- "*"
  }
  if (length(parts) == 0) parts <- "*"

  sql <- paste0("SELECT ", paste(parts, collapse = ", "), " FROM ", duckdt_qtbl(x))
  if (!is.null(where_sql)) sql <- paste0(sql, " WHERE ", where_sql)
  if (!is.null(by_res)) sql <- paste0(sql, " GROUP BY ", paste(by_res$parts, collapse = ", "))

  # `[]` after setDT() works around a well-known data.table quirk: setDT()
  # marks the object so its *next* auto-print at the top level is silently
  # skipped (the same mechanism `:=` uses). `[]` forces a normal print-able
  # copy so `d[cyl == 6]` at the console (or in a function returning this
  # value) actually prints instead of appearing to do nothing.
  data.table::setDT(DBI::dbGetQuery(conn, sql))[]
}
