#' Run a data.table-style query into a temporary table
#'
#' `d[i, j, by]` returns a `data.table` -- the rows come back to R. This runs
#' exactly the same query, but leaves the result in the database as a
#' temporary table and hands back a `"duckdt"` handle over it. Nothing
#' crosses into R, so the subset stays available to anything that takes a
#' handle: [dbdt_join()], [dbdt_merge()], another `dbdt_temp()`, or
#' plain `[` when you finally do want the rows.
#'
#' On DuckDB the result is a genuine `TEMP` table: it lives in the session's
#' `temp` schema and disappears when the connection closes. On MS SQL Server
#' it is created with `SELECT ... INTO` as an ordinary table on the current
#' schema (matching how [dbdt_merge()] stages there), so it outlives the
#' session -- call [dbdt_drop()] when you are done with it.
#'
#' The handle is materialized and writable, so `:=` and [dbdt_merge()] both
#' work against it. Writes land on the temporary copy and leave `x`'s table
#' untouched.
#'
#' @param x A `"duckdt"` object.
#' @param i,j,by As in `[.duckdt` -- row filter, column selection/computation,
#'   and grouping. All optional; `dbdt_temp(x)` copies the whole table.
#' @param name Name for the temporary table. Defaults to a generated name
#'   unique within the session.
#'
#' @return A `"duckdt"` object over the temporary table.
#' @seealso [dbdt_drop()] to drop it, [dbdt_join()] to join it.
#' @examples
#' d <- as.dbdt(datasets::mtcars)
#' sixes <- dbdt_temp(d, cyl == 6)
#' nrow(sixes)
#' dbdt_drop(sixes)
#' @export
duckdt_temp <- function(x, i, j, by, name = NULL) {
  if (!inherits(x, "duckdt")) {
    stop("duckdt_temp: `x` must be a duckdt object.", call. = FALSE)
  }
  has_i <- !missing(i)
  has_j <- !missing(j)
  has_by <- !missing(by)

  ie <- if (has_i) substitute(i)
  je <- if (has_j) substitute(j)
  bye <- if (has_by) substitute(by)

  if (has_j && is.call(je) && identical(je[[1]], as.name(":="))) {
    stop(
      "duckdt_temp: `:=` mutates a table in place and has no result to store. ",
      "Take the subset first, then write to it: `t <- duckdt_temp(x, i); t[, col := expr]`.",
      call. = FALSE
    )
  }

  sql <- duckdt_select_sql(x, ie, je, bye, has_i, has_j, has_by, parent.frame(), to_r = FALSE)
  if (is.null(name)) name <- duckdt_temp_name(x$tbl)
  duckdt_create_temp(x$conn, name, sql, duckdt_dialect(x$conn))
  duckdt_temp_handle(x$conn, name)
}

#' Drop a table created by dbdt_temp()
#'
#' Temporary tables are yours to manage: DuckDB drops them when the
#' connection closes, but a long-lived session that builds many subsets will
#' hold them all until then (and on MS SQL Server they are ordinary tables
#' that persist). This drops one.
#'
#' As a guard against dropping real data, this refuses handles that
#' [dbdt_temp()] did not create unless you pass `force = TRUE`.
#'
#' @param x A `"duckdt"` object from [dbdt_temp()].
#' @param force Drop the table even if `x` is not a `dbdt_temp()` handle?
#'   Default `FALSE`. This deletes a real table -- there is no undo.
#'
#' @return `NULL`, invisibly.
#' @examples
#' d <- as.dbdt(datasets::mtcars)
#' sixes <- dbdt_temp(d, cyl == 6)
#' dbdt_drop(sixes)
#' @export
duckdt_drop <- function(x, force = FALSE) {
  if (!inherits(x, "duckdt")) {
    stop("duckdt_drop: `x` must be a duckdt object.", call. = FALSE)
  }
  if (!isTRUE(x$temporary) && !isTRUE(force)) {
    stop(
      "duckdt: `duckdt_drop()` only drops tables created by `duckdt_temp()`. ",
      sprintf("'%s' is a real table -- pass `force = TRUE` if you really mean to drop it.", x$tbl),
      call. = FALSE
    )
  }
  duckdt_drop_table(x$conn, x$tbl, duckdt_dialect(x$conn))
  invisible(NULL)
}

# ---- internals -------------------------------------------------------------

# A duckdt_temp() table is a real, mutable table, so the handle is built
# directly rather than through duckdt(): on DuckDB a TEMP table reports
# information_schema table_type "LOCAL TEMPORARY", and on MS SQL Server the
# name may not be resolvable by dbExistsTable() at all.
duckdt_temp_handle <- function(conn, name) {
  structure(
    list(conn = conn, tbl = name, materialized = TRUE, writable = TRUE, temporary = TRUE),
    class = "duckdt"
  )
}

# Names are unique per session so repeated subsets don't silently overwrite
# each other (`CREATE OR REPLACE` would otherwise make that easy to do).
duckdt_temp_name <- function(base) {
  n <- if (is.null(duckdt_env$temp_n)) 1L else duckdt_env$temp_n + 1L
  duckdt_env$temp_n <- n
  paste0("duckdt_temp_", gsub("[^A-Za-z0-9]", "_", base), "_", n)
}

duckdt_create_temp <- function(conn, name, sql, dialect) {
  qname <- DBI::dbQuoteIdentifier(conn, name)
  if (dialect == "mssql") {
    # T-SQL has no CREATE TEMP TABLE ... AS; SELECT ... INTO is the
    # equivalent, and a derived table needs an alias.
    duckdt_drop_table(conn, name, dialect)
    DBI::dbExecute(conn, paste0("SELECT * INTO ", qname, " FROM (", sql, ") AS duckdt_src"))
  } else {
    DBI::dbExecute(conn, paste0("CREATE OR REPLACE TEMP TABLE ", qname, " AS ", sql))
  }
  invisible(NULL)
}

duckdt_drop_table <- function(conn, name, dialect) {
  qname <- DBI::dbQuoteIdentifier(conn, name)
  if (dialect == "mssql") {
    qname_str <- DBI::dbQuoteString(conn, name)
    DBI::dbExecute(conn, paste0("IF OBJECT_ID(", qname_str, ", 'U') IS NOT NULL DROP TABLE ", qname))
  } else {
    DBI::dbExecute(conn, paste0("DROP TABLE IF EXISTS ", qname))
  }
  invisible(NULL)
}
