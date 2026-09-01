#' Push a data.frame/data.table into DuckDB as a duckdt handle
#'
#' By default this is a **zero-copy** operation: [duckdb::duckdb_register()]
#' registers `x` as a virtual view directly over the R object, with no
#' serialization. Pass `copy = TRUE` to instead physically write the data
#' into DuckDB, which is required if you want to use `:=` to mutate the
#' result independently of `x`.
#'
#' List-columns (e.g. `tags = list(c("a", "b"), "c")`) are supported against
#' DuckDB connections, both zero-copy and with `copy = TRUE`: they round-trip
#' to/from DuckDB's native `LIST` type, and come back as an R list-column
#' from [as.data.table()] with no extra steps. As with DuckDB's `LIST` type
#' itself, every element within one list-column must coerce to a single
#' atomic type. List-columns are not supported against MS SQL Server
#' connections.
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
#' @return A `"duckdt"` object, writable immediately (unlike [duckdt()]'s
#'   read-only-by-default handles -- you just created this table, so there's
#'   nothing accidental about writing to it).
#' @export
as.duckdt <- function(x, conn = NULL, name = NULL, overwrite = FALSE, copy = FALSE) {
  stopifnot(is.data.frame(x))
  if (is.null(name)) {
    name <- make.names(deparse(substitute(x)))
  }
  if (is.null(conn)) {
    conn <- DBI::dbConnect(duckdb::duckdb())
    duckdt_hint_erd()
  }

  if (copy) {
    if (duckdt_dialect(conn) == "duckdb") {
      src_name <- paste0(name, "_src")
      qsrc <- DBI::dbQuoteIdentifier(conn, src_name)
      duckdb::duckdb_register(conn, src_name, x)
      on.exit(try(duckdb::duckdb_unregister(conn, src_name), silent = TRUE), add = TRUE)
      create_sql <- if (overwrite) "CREATE OR REPLACE TABLE " else "CREATE TABLE "
      DBI::dbExecute(conn, paste0(
        create_sql, DBI::dbQuoteIdentifier(conn, name), " AS SELECT * FROM ", qsrc
      ))
    } else {
      DBI::dbWriteTable(conn, name, x, overwrite = overwrite)
    }
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

  duckdt(conn, name, materialized = materialized, writable = TRUE)
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
  # `[]` works around a data.table quirk where setDT() suppresses the next
  # top-level auto-print (see the note in bracket.R).
  data.table::setDT(DBI::dbGetQuery(x$conn, paste0("SELECT * FROM ", duckdt_qtbl(x))))[]
}
