#' Connect to a DuckDB database
#'
#' A thin, friendly wrapper around
#' `DBI::dbConnect(duckdb::duckdb(dbdir = ...))`. The only things it adds are
#' defaults worth having and a summary of what you just connected to: how
#' many tables are in there, and the two calls that get you moving --
#' [dbdt_erd()] to see the whole schema, [dbdt()] to query one table with
#' `data.table` syntax.
#'
#' You never have to use this: every function in duckdt takes a plain `DBI`
#' connection, so `DBI::dbConnect(duckdb::duckdb(dbdir = "x.duckdb"))` is
#' equally fine.
#'
#' @param dbdir Path to a DuckDB database file. Forward slashes work on
#'   Windows and save escaping backslashes. The default, `":memory:"`, is a
#'   scratch database that disappears when you disconnect. A file that
#'   doesn't exist yet is created.
#' @param read_only Open the file read-only. A good default for a database
#'   you only mean to explore, and required if another process has it open.
#' @param quiet Skip the connection summary.
#' @param ... Passed to [duckdb::duckdb()].
#'
#' @return A `DBI` connection to DuckDB. Close it with
#'   [dbdt_disconnect()] when you're done.
#' @seealso [dbdt_erd()] to visualise the database you just opened,
#'   [dbdt()] to wrap one of its tables.
#' @examples
#' con <- dbdt_connect()                       # in-memory scratch database
#' d <- as.dbdt(mtcars, conn = con, copy = TRUE)
#' d[cyl == 6, .(mpg, hp)]
#' dbdt_disconnect(con)
#'
#' \dontrun{
#' # A database file on disk, read-only because we're only looking:
#' con <- dbdt_connect("C:/data/warehouse.duckdb", read_only = TRUE)
#' dbdt_erd(con)
#' }
#' @export
duckdt_connect <- function(dbdir = ":memory:", read_only = FALSE, quiet = FALSE, ...) {
  conn <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir, read_only = read_only, ...))
  if (!quiet) duckdt_connect_message(conn, dbdir)
  conn
}

#' @param conn A `DBI` connection, or a `"duckdt"` object (its connection is
#'   closed).
#' @param shutdown Also shut the DuckDB database down, releasing the file.
#'   Default `TRUE`.
#' @return `dbdt_disconnect()` returns `TRUE` invisibly.
#' @rdname duckdt_connect
#' @export
duckdt_disconnect <- function(conn, shutdown = TRUE) {
  conn <- duckdt_unwrap_conn(conn)
  if (duckdt_dialect(conn) == "duckdb") {
    DBI::dbDisconnect(conn, shutdown = shutdown)
  } else {
    DBI::dbDisconnect(conn)
  }
  invisible(TRUE)
}

# ---- messages --------------------------------------------------------------

duckdt_connect_message <- function(conn, dbdir = NULL) {
  where <- if (is.null(dbdir) || identical(dbdir, ":memory:")) {
    "an in-memory database"
  } else {
    paste0("{.file ", dbdir, "}")
  }
  tabs <- tryCatch(as.data.frame(duckdt_tables(conn)), error = function(e) NULL)

  msg <- c("v" = paste("Connected to DuckDB:", where))
  if (!is.null(tabs) && nrow(tabs)) {
    names <- tabs$name
    shown <- utils::head(names, 6)
    msg <- c(msg, "i" = paste0(
      "{nrow(tabs)} table{?s}: {.val ", paste(shown, collapse = "}, {.val "), "}",
      if (length(names) > length(shown)) sprintf(" and %d more", length(names) - length(shown)) else ""
    ))
    msg <- c(msg,
      ">" = "{.code dbdt_erd(con)} to explore the tables and how they connect",
      ">" = paste0("{.code dbdt(con, \"", names[1], "\")} to query one with data.table syntax")
    )
  } else {
    msg <- c(msg,
      "i" = "No tables yet.",
      ">" = "{.code as.dbdt(mtcars, conn = con, copy = TRUE)} to put a data.frame in one",
      ">" = "{.code dbdt_csv(\"data.csv\", conn = con)} to read a file without loading it into R"
    )
  }
  cli::cli_inform(msg)
  invisible(NULL)
}

# Shown once per session when duckdt opens a connection on the user's behalf
# (as.duckdt(), duckdt_csv(), ...), where a full connection summary would be
# noise but the pointer to duckdt_erd() is still worth making once.
duckdt_hint_erd <- function() {
  if (!interactive() || isTRUE(duckdt_env$hinted_erd)) return(invisible(NULL))
  duckdt_env$hinted_erd <- TRUE
  cli::cli_inform(c(
    "i" = "duckdt opened an in-memory DuckDB database for this.",
    ">" = "{.fn dbdt_erd} on it (or any connection) draws the tables and how they connect."
  ))
  invisible(NULL)
}

# ---- connection description ------------------------------------------------

# The DuckDB file behind a connection, or NULL for in-memory/other backends.
duckdt_dbdir <- function(x) {
  if (is_duckdt_data_model(x)) return(NULL)
  conn <- duckdt_unwrap_conn(x)
  if (!duckdt_is_connection(conn) || duckdt_dialect(conn) != "duckdb") return(NULL)
  dbdir <- tryCatch(conn@driver@dbdir, error = function(e) NULL)
  if (is.null(dbdir) || !nzchar(dbdir) || identical(dbdir, ":memory:")) NULL else dbdir
}

# Short human description of what is being drawn, used as a page title.
duckdt_conn_label <- function(x) {
  if (is_duckdt_data_model(x)) return("Data model")
  conn <- duckdt_unwrap_conn(x)
  if (!duckdt_is_connection(conn)) return("Data model")
  dbdir <- duckdt_dbdir(conn)
  if (!is.null(dbdir)) return(basename(dbdir))
  if (duckdt_dialect(conn) != "duckdb") {
    return(tryCatch(
      paste0("SQL Server: ", DBI::dbGetInfo(conn)$dbname),
      error = function(e) "SQL Server database"
    ))
  }
  "In-memory DuckDB"
}
