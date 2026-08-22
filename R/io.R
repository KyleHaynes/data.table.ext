#' Open a CSV file as a duckdt view
#'
#' Creates a lazy DuckDB view over the file using `read_csv_auto` — the file
#' is not read into R; querying is pushed down and executed out-of-core in
#' DuckDB, `fread`-style ergonomics included.
#'
#' @param path Path to one or more CSV files.
#' @param conn A `DBI` connection to DuckDB; a new in-memory one is created
#'   if `NULL`.
#' @param name View name to use inside DuckDB. Defaults to the file's base
#'   name.
#' @param ... Currently unused.
#'
#' @return A `"duckdt"` object (a view, i.e. not writable via `:=`).
#' @export
duckdt_csv <- function(path, conn = NULL, name = NULL, ...) {
  duckdt_from_reader(path, conn, name, reader = "read_csv_auto")
}

#' Open Parquet file(s) as a duckdt view
#'
#' @inheritParams duckdt_csv
#' @return A `"duckdt"` object (a view, i.e. not writable via `:=`).
#' @export
duckdt_parquet <- function(path, conn = NULL, name = NULL, ...) {
  duckdt_from_reader(path, conn, name, reader = "read_parquet")
}

duckdt_from_reader <- function(path, conn, name, reader) {
  if (is.null(conn)) conn <- DBI::dbConnect(duckdb::duckdb())
  if (is.null(name)) name <- make.names(tools::file_path_sans_ext(basename(path[1])))
  qpaths <- paste(
    vapply(path, function(p) as.character(DBI::dbQuoteString(conn, p)), character(1)),
    collapse = ", "
  )
  sql <- sprintf(
    "CREATE OR REPLACE VIEW %s AS SELECT * FROM %s([%s])",
    DBI::dbQuoteIdentifier(conn, name), reader, qpaths
  )
  DBI::dbExecute(conn, sql)
  duckdt(conn, name, materialized = FALSE)
}
