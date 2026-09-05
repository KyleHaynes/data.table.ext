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
  if (!duckdt_exists(conn, table)) {
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

# DBI's dbExistsTable()/dbListFields() are the direct answers, but DuckDB
# implements both by probing with `SELECT * FROM t WHERE FALSE` -- which
# fails at prepare time on a column type it can't convert (BIT), so it
# reports "no such table" and "can't list columns" for exactly the tables
# binary-cols.R exists to keep readable. The catalogue knows better.
duckdt_exists <- function(conn, table) {
  DBI::dbExistsTable(conn, table) || nrow(duckdt_column_types(conn, table)) > 0
}

duckdt_columns <- function(x) {
  cols <- tryCatch(DBI::dbListFields(x$conn, x$tbl), error = function(e) NULL)
  if (!is.null(cols)) return(cols)
  types <- duckdt_column_types(x$conn, x$tbl)
  if (!nrow(types)) {
    stop(sprintf("duckdt: could not list the columns of '%s'.", x$tbl), call. = FALSE)
  }
  types$column
}

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
  # Naming the skipped columns here is the standing answer to "where did my
  # column go?", so print() reports them every time rather than leaving it to
  # the once-per-session note -- and claims that note so it isn't repeated.
  bin <- intersect(cols, duckdt_binary_cols(x))
  if (length(bin)) {
    duckdt_binary_notify(x$tbl, bin, announce = FALSE)
    cat("Not fetched (binary):", paste(bin, collapse = ", "), "\n")
  }
  keep <- setdiff(cols, bin)
  if (!length(keep)) {
    cat("No column can be fetched into R; reduce them in the database instead.\n")
    return(invisible(x))
  }
  preview_sql <- duckdt_limit_sql(
    duckdt_qtbl(x), n, duckdt_dialect(x$conn),
    if (length(bin)) duckdt_select_list(x$conn, keep, x$tbl) else "*"
  )
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
