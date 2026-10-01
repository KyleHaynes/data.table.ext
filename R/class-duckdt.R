#' Wrap an existing database table or view as a duckdt handle
#'
#' `duckdt` objects are thin handles: `list(conn, tbl)`. No data is copied;
#' every operation on them (`[`, `print`, `dim`, ...) issues SQL against
#' `conn`. Most users will start from [as.dbdt()], [dbdt_csv()], or
#' [dbdt_parquet()] instead of calling this directly.
#'
#' Handles created here default to **read-only**: `:=` and [dbdt_merge()]
#' both refuse to run against them, even if `table` is a materialized base
#' table capable of being written to. This guards against accidentally
#' mutating a table you only meant to explore -- e.g. after reconnecting to
#' a persistent `.duckdb` file. Pass `writable = TRUE` once you actually mean
#' to write. (Handles from [as.dbdt()] are writable immediately, since you
#' just created that table in the same call.)
#'
#' @param conn A `DBI` connection to DuckDB or Microsoft SQL Server.
#' @param table Name of an existing table or view in `conn`.
#' @param materialized Is `table` a real, mutable base table (`TRUE`) as
#'   opposed to a read-only view (`FALSE`)? Only materialized tables support
#'   `:=` writes. `NA` (the default) auto-detects it from the catalogue.
#' @param writable Allow `:=` and [dbdt_merge()] to write through this
#'   handle? Default `FALSE` (see Details). Has no effect if `materialized`
#'   is/resolves to `FALSE` -- views are never writable regardless.
#' @param schema The schema `table` is in, e.g. `"sales"` for
#'   `sales.orders`. `NULL` (the default) resolves the bare name the way the
#'   database does: the default schema (usually `dbo`) on SQL Server, the
#'   search path on DuckDB.
#'
#' @return An object of class `"duckdt"`.
#' @details
#' Use `dbdt()`, `as.dbdt()` and `dbdt_*()` for new code. The original
#' `duckdt` names remain available with identical behavior. The S3 classes
#' and `duckdt.*` options are retained for compatibility. [dbdt_connect()]
#' opens DuckDB; use a `DBI`/`odbc` connection for SQL Server.
#'
#' A handle reads the table's column names and types once, the first time it
#' needs them, and keeps them: on a large SQL Server database the catalogue
#' lookup can cost more than a small query does. `:=` refreshes them. If the
#' table's columns are changed some other way, create a new handle.
#' @examples
#' con <- dbdt_connect(quiet = TRUE)
#' DBI::dbExecute(con, "CREATE TABLE t (id INTEGER, x DOUBLE)")
#' DBI::dbExecute(con, "INSERT INTO t VALUES (1, 10), (2, 20)")
#' d <- dbdt(con, "t")
#' d
#'
#' # A table outside the default schema:
#' DBI::dbExecute(con, "CREATE SCHEMA sales")
#' DBI::dbExecute(con, "CREATE TABLE sales.orders (id INTEGER, total DOUBLE)")
#' dbdt(con, "orders", schema = "sales")
#' dbdt_disconnect(con)
#' @export
duckdt <- function(conn, table, materialized = NA, writable = FALSE, schema = NULL) {
  if (!is.character(table) || length(table) != 1L || is.na(table) || !nzchar(table)) {
    stop("duckdt: `table` must be a single table or view name.", call. = FALSE)
  }
  if (!is.null(schema) && (!is.character(schema) || length(schema) != 1L || is.na(schema))) {
    stop("duckdt: `schema` must be NULL or a single schema name.", call. = FALSE)
  }
  found <- duckdt_lookup(conn, table, schema)
  if (!found$exists) {
    stop(sprintf("duckdt: table/view '%s' does not exist on this connection.",
      duckdt_label(list(tbl = table, schema = schema))), call. = FALSE)
  }
  if (is.na(materialized)) materialized <- found$materialized
  duckdt_handle(conn, table, schema = schema, materialized = materialized,
    writable = isTRUE(writable))
}

# The one place handles are put together. `meta` is an environment so that
# what a handle learns about its table (see duckdt_meta()) is shared by every
# copy of it -- `[` receives the handle by value.
duckdt_handle <- function(conn, table, schema = NULL, materialized = TRUE,
                          writable = TRUE, temporary = FALSE) {
  x <- list(conn = conn, tbl = table, materialized = materialized, writable = writable)
  if (isTRUE(temporary)) x$temporary <- TRUE
  if (!is.null(schema)) x$schema <- schema
  x$meta <- new.env(parent = emptyenv())
  structure(x, class = "duckdt")
}

# "schema.table" for messages, or just the table.
duckdt_label <- function(x) if (is.null(x$schema)) x$tbl else paste0(x$schema, ".", x$tbl)

# Does the table exist, and is it a real table (rather than a view)?
#
# On SQL Server one catalogue lookup by OBJECT_ID() answers both. It resolves
# the name exactly as a query will, and it is a single indexed lookup, where
# DBI::dbExistsTable() goes through ODBC's SQLTables -- a pattern search of
# every schema, in which `_` in a name is a wildcard.
duckdt_lookup <- function(conn, table, schema = NULL) {
  if (duckdt_dialect(conn) == "mssql") {
    type <- tryCatch(
      duckdt_get_query(conn, paste0(
        "SELECT o.type FROM ", duckdt_mssql_sys(table), "objects o WHERE o.object_id = ",
        duckdt_mssql_object_id(conn, table, schema)
      ))$type,
      error = function(e) NULL
    )
    if (!is.null(type)) {
      type <- trimws(type)
      # A synonym (SN) can't be inspected through sys.columns, so let the
      # portable checks below make what they can of it.
      if (!identical(type, "SN")) {
        return(list(exists = length(type) > 0, materialized = identical(type[1], "U")))
      }
    }
  }
  id <- if (is.null(schema)) table else DBI::Id(schema = schema, table = table)
  exists <- isTRUE(tryCatch(DBI::dbExistsTable(conn, id), error = function(e) FALSE)) ||
    nrow(duckdt_column_types(conn, table, schema)) > 0
  list(exists = exists, materialized = exists && duckdt_is_table(conn, table, schema))
}

# A DuckDB TEMP table reports "LOCAL TEMPORARY" rather than "BASE TABLE",
# but it is just as materialized and just as writable -- so handles over one
# (e.g. from duckdt_temp()) must not be mistaken for read-only views.
duckdt_is_table <- function(conn, table, schema = NULL) {
  res <- duckdt_get_query(conn, paste0(
    "SELECT table_type FROM information_schema.tables WHERE table_name = ",
    DBI::dbQuoteString(conn, table),
    if (!is.null(schema)) paste0(" AND table_schema = ", DBI::dbQuoteString(conn, schema))
  ))
  nrow(res) > 0 && res$table_type[1] %in% c("BASE TABLE", "LOCAL TEMPORARY")
}

duckdt_columns <- function(x) {
  types <- duckdt_meta(x)
  if (nrow(types)) return(types$column)
  # DuckDB implements dbListFields() by probing with `SELECT * FROM t WHERE
  # FALSE`, which fails at prepare time on a type it can't convert (BIT), so
  # it is only the last resort.
  id <- if (is.null(x$schema)) x$tbl else DBI::Id(schema = x$schema, table = x$tbl)
  cols <- tryCatch(DBI::dbListFields(x$conn, id), error = function(e) NULL)
  if (!is.null(cols)) return(cols)
  stop(sprintf("duckdt: could not list the columns of '%s'.", duckdt_label(x)), call. = FALSE)
}

# The table's columns and their types, as data.frame(column, type), from a
# single catalogue query. Kept on the handle after the first call: every `[`
# needs the column names (to tell columns from R variables) and types (to
# leave binary columns out), and on a large SQL Server database looking them
# up costs more than many queries do. duckdt_meta_reset() forgets them, for
# `:=`, which can add a column.
duckdt_meta <- function(x) {
  cache <- x$meta
  if (is.environment(cache) && !is.null(cache$types)) return(cache$types)
  types <- duckdt_describe(x$conn, x$tbl, x$schema)
  if (is.environment(cache) && nrow(types)) cache$types <- types
  types
}

duckdt_meta_reset <- function(x) {
  if (is.environment(x$meta)) rm(list = ls(x$meta, all.names = TRUE), envir = x$meta)
  invisible(x)
}

# Ask the catalogue in the way that resolves the name exactly as a query
# will: sys.columns by OBJECT_ID() on SQL Server (one indexed lookup, rather
# than INFORMATION_SCHEMA or ODBC's SQLColumns, which are slow on a database
# with many tables), DESCRIBE on DuckDB (which also tells a TEMP table from a
# main one of the same name). information_schema, by name, is the fallback
# for anything those can't answer.
duckdt_describe <- function(conn, table, schema = NULL) {
  res <- if (duckdt_dialect(conn) == "mssql") {
    tryCatch(duckdt_get_query(conn, paste0(
      "SELECT c.name AS column_name, ",
      "ISNULL(TYPE_NAME(c.system_type_id), TYPE_NAME(c.user_type_id)) AS data_type ",
      "FROM ", duckdt_mssql_sys(table), "columns c WHERE c.object_id = ",
      duckdt_mssql_object_id(conn, table, schema), " ORDER BY c.column_id"
    )), error = function(e) NULL)
  } else {
    qname <- if (is.null(schema)) {
      DBI::dbQuoteIdentifier(conn, table)
    } else {
      DBI::dbQuoteIdentifier(conn, DBI::Id(schema = schema, table = table))
    }
    tryCatch(duckdt_get_query(conn, paste0("DESCRIBE ", qname)), error = function(e) NULL)
  }
  if (is.data.frame(res) && nrow(res) && ncol(res) >= 2) {
    return(data.frame(
      column = as.character(res[[1]]), type = as.character(res[[2]]),
      stringsAsFactors = FALSE
    ))
  }
  duckdt_column_types(conn, table, schema)
}

duckdt_qtbl <- function(x) {
  if (is.null(x$schema)) {
    DBI::dbQuoteIdentifier(x$conn, x$tbl)
  } else {
    DBI::dbQuoteIdentifier(x$conn, DBI::Id(schema = x$schema, table = x$tbl))
  }
}

# `OBJECT_ID(N'[schema].[table]')`, for SQL Server catalogue lookups. The
# name is bracket-quoted here rather than with dbQuoteIdentifier(), whose
# quote character depends on the driver; OBJECT_ID() parses brackets
# whatever the session's QUOTED_IDENTIFIER setting. A local temporary table
# (`#name`) lives in tempdb.
duckdt_mssql_object_id <- function(conn, table, schema = NULL) {
  br <- function(s) paste0("[", gsub("]", "]]", s, fixed = TRUE), "]")
  name <- if (startsWith(table, "#")) {
    paste0("tempdb..", br(table))
  } else if (is.null(schema)) {
    br(table)
  } else {
    paste0(br(schema), ".", br(table))
  }
  paste0("OBJECT_ID(N", DBI::dbQuoteString(conn, name), ")")
}

duckdt_mssql_sys <- function(table) if (startsWith(table, "#")) "tempdb.sys." else "sys."

# Rows in a SQL Server table according to its partition metadata: free to
# read, and exact unless rows are being written at that moment. NA for a
# view, for any other backend, or if the catalogue can't be read.
duckdt_approx_rows <- function(x) {
  if (duckdt_dialect(x$conn) != "mssql") return(NA_real_)
  n <- tryCatch(
    duckdt_get_query(x$conn, paste0(
      "SELECT SUM(p.rows) AS n FROM ", duckdt_mssql_sys(x$tbl), "partitions p ",
      "WHERE p.object_id = ", duckdt_mssql_object_id(x$conn, x$tbl, x$schema),
      " AND p.index_id IN (0, 1)"
    ))$n,
    error = function(e) NULL
  )
  if (length(n) == 1L && !is.na(n)) as.numeric(n) else NA_real_
}

#' @param x A `"duckdt"` database handle.
#' @param n Number of preview rows to print.
#' @param count Count all rows when printing? Defaults to `FALSE` on SQL
#'   Server to avoid scanning the table just to preview it. Uncounted, a SQL
#'   Server table shows the row total from its partition metadata, marked
#'   `~` (free to read, and exact unless rows are being written at that
#'   moment); a view, which has none, shows `?`. `nrow()` and `dim()` still
#'   count exactly.
#' @param ... Unused.
#' @rdname duckdt
#' @export
print.duckdt <- function(x, n = 6L, ..., count = duckdt_dialect(x$conn) != "mssql") {
  cols <- duckdt_columns(x)
  nr <- if (isTRUE(count)) {
    format(duckdt_get_query(x$conn, paste0("SELECT count(*) AS n FROM ", duckdt_qtbl(x)))$n,
      big.mark = ",")
  } else {
    approx <- duckdt_approx_rows(x)
    if (is.na(approx)) "?" else paste0("~", format(approx, big.mark = ",", scientific = FALSE))
  }
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
    duckdt_label(x), nr, length(cols), status
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
  preview <- duckdt_get_query(x$conn, preview_sql)
  print(data.table::setDT(preview))
  invisible(x)
}

#' @export
dim.duckdt <- function(x) {
  cols <- duckdt_columns(x)
  nr <- duckdt_get_query(x$conn, paste0("SELECT count(*) AS n FROM ", duckdt_qtbl(x)))$n
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
