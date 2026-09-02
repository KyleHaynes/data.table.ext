#' Wrap an existing DuckDB table or view as a duckdt handle
#'
#' `duckdt` objects are thin handles: `list(conn, tbl)`. No data is copied;
#' every operation on them (`[`, `print`, `dim`, ...) issues SQL against
#' `conn`. Most users will start from [as.duckdt()], [duckdt_csv()], or
#' [duckdt_parquet()] instead of calling this directly.
#'
#' Handles created here default to **read-only**: `:=` and [duckdt_merge()]
#' both refuse to run against them, even if `table` is a materialized base
#' table capable of being written to. This guards against accidentally
#' mutating a table you only meant to explore -- e.g. after reconnecting to
#' a persistent `.duckdb` file. Pass `writable = TRUE` once you actually mean
#' to write. (Handles from [as.duckdt()] are writable immediately, since you
#' just created that table in the same call.)
#'
#' @param conn A `DBI` connection to a DuckDB database, e.g. from
#'   [duckdb::duckdb()].
#' @param table Name of an existing table or view in `conn`.
#' @param materialized Is `table` a real, mutable base table (`TRUE`) as
#'   opposed to a read-only view (`FALSE`)? Only materialized tables support
#'   `:=` writes. `NA` (the default) auto-detects via `information_schema`.
#' @param writable Allow `:=` and [duckdt_merge()] to write through this
#'   handle? Default `FALSE` (see Details). Has no effect if `materialized`
#'   is/resolves to `FALSE` -- views are never writable regardless.
#'
#' @return An object of class `"duckdt"`.
#' @export
duckdt <- function(conn, table, materialized = NA, writable = FALSE) {
  if (!DBI::dbExistsTable(conn, table)) {
    stop(sprintf("duckdt: table/view '%s' does not exist on this connection.", table), call. = FALSE)
  }
  if (is.na(materialized)) {
    materialized <- duckdt_is_table(conn, table)
  }
  structure(list(conn = conn, tbl = table, materialized = materialized, writable = isTRUE(writable)), class = "duckdt")
}

# A DuckDB TEMP table reports "LOCAL TEMPORARY" rather than "BASE TABLE",
# but it is just as materialized and just as writable -- so handles over one
# (e.g. from duckdt_temp()) must not be mistaken for read-only views.
duckdt_is_table <- function(conn, table) {
  res <- DBI::dbGetQuery(conn, paste0(
    "SELECT table_type FROM information_schema.tables WHERE table_name = ",
    DBI::dbQuoteString(conn, table)
  ))
  nrow(res) > 0 && res$table_type[1] %in% c("BASE TABLE", "LOCAL TEMPORARY")
}

duckdt_columns <- function(x) DBI::dbListFields(x$conn, x$tbl)

duckdt_qtbl <- function(x) DBI::dbQuoteIdentifier(x$conn, x$tbl)

#' @export
print.duckdt <- function(x, n = 6L, ...) {
  cols <- duckdt_columns(x)
  nr <- DBI::dbGetQuery(x$conn, paste0("SELECT count(*) AS n FROM ", duckdt_qtbl(x)))$n
  status <- if (!isTRUE(x$materialized)) {
    " (view)"
  } else if (isTRUE(x$temporary)) {
    " (temp)"
  } else if (!isTRUE(x$writable)) {
    " (read-only)"
  } else {
    ""
  }
  cat(sprintf(
    "<duckdt> %s [%s x %d]%s\n",
    x$tbl, format(nr, big.mark = ","), length(cols), status
  ))
  cat("Columns:", paste(cols, collapse = ", "), "\n")
  preview_sql <- duckdt_limit_sql(duckdt_qtbl(x), n, duckdt_dialect(x$conn))
  preview <- DBI::dbGetQuery(x$conn, preview_sql)
  print(data.table::setDT(preview))
  invisible(x)
}

#' @export
dim.duckdt <- function(x) {
  cols <- duckdt_columns(x)
  nr <- DBI::dbGetQuery(x$conn, paste0("SELECT count(*) AS n FROM ", duckdt_qtbl(x)))$n
  c(nr, length(cols))
}

#' @exportS3Method base::nrow
nrow.duckdt <- function(x) dim(x)[1]

#' @exportS3Method base::ncol
ncol.duckdt <- function(x) dim(x)[2]

#' @export
names.duckdt <- function(x) duckdt_columns(x)

#' @exportS3Method base::colnames
colnames.duckdt <- function(x) duckdt_columns(x)
