#' Push a data.frame/data.table into DuckDB as a duckdt handle
#'
#' By default this is a **zero-copy** operation: [duckdb::duckdb_register()]
#' registers `x` as a virtual view directly over the R object, with no
#' serialization. Pass `copy = TRUE` to instead physically write the data
#' into DuckDB via [DBI::dbWriteTable()], which is required if you want to
#' use `:=` to mutate the result independently of `x`.
#'
#' @param x A `data.frame` or `data.table`.
#' @param conn A `DBI` connection to DuckDB. If `NULL` (the default), a new
#'   in-memory `duckdb::duckdb()` connection is created for you.
#' @param name Table/view name to use inside DuckDB. Defaults to a
#'   sanitized version of `deparse(substitute(x))`.
#' @param overwrite Overwrite an existing table/view of the same `name`.
#' @param copy If `TRUE`, physically copy `x` into DuckDB (materialized,
#'   writable). If `FALSE` (default), register `x` as a zero-copy view
#'   (read-only).
#'
#' @return A `"duckdt"` object.
#' @export
as.duckdt <- function(x, conn = NULL, name = NULL, overwrite = FALSE, copy = FALSE) {
  stopifnot(is.data.frame(x))
  if (is.null(name)) {
    name <- make.names(deparse(substitute(x)))
  }
  if (is.null(conn)) {
    conn <- DBI::dbConnect(duckdb::duckdb())
  }

  if (copy) {
    DBI::dbWriteTable(conn, name, x, overwrite = overwrite)
    materialized <- TRUE
  } else {
    if (duckdt_dialect(conn) != "duckdb") {
      stop(
        "duckdt: zero-copy registration (`copy = FALSE`) is only supported ",
        "for DuckDB connections. Pass `copy = TRUE` to physically write `x` ",
        "into this connection instead.", call. = FALSE
      )
    }
    if (overwrite) {
      try(duckdb::duckdb_unregister(conn, name), silent = TRUE)
    }
    duckdb::duckdb_register(conn, name, x)
    materialized <- FALSE
  }

  duckdt(conn, name, materialized = materialized)
}

#' Pull a duckdt handle fully into a data.table
#'
#' Runs `SELECT * FROM <table>` and materializes the result in R. This is
#' the counterpart to [as.duckdt()].
#'
#' @param x A `"duckdt"` object.
#' @param ... Unused.
#' @return A `data.table`.
#' @importFrom data.table as.data.table
#' @exportS3Method data.table::as.data.table
as.data.table.duckdt <- function(x, ...) {
  data.table::setDT(DBI::dbGetQuery(x$conn, paste0("SELECT * FROM ", duckdt_qtbl(x))))
}
