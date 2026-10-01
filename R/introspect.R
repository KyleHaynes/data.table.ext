#' List the tables and views in a database
#'
#' Queries the portable `information_schema.tables` view, so this works
#' against both DuckDB and MS SQL Server connections. DuckDB's/Postgres's
#' internal `information_schema`/`pg_catalog` compatibility schemas are
#' excluded.
#'
#' @param conn A `DBI` connection, or a `"duckdt"` object (its `$conn` is
#'   used).
#'
#' @return A `data.table` with one row per table/view: `schema`, `name`,
#'   `type` (`"BASE TABLE"` or `"VIEW"`).
#' @examples
#' con <- dbdt_example()
#' dbdt_tables(con)
#' dbdt_disconnect(con)
#' @export
duckdt_tables <- function(conn) {
  conn <- duckdt_unwrap_conn(conn)
  if (duckdt_dialect(conn) == "mssql") {
    # The same objects as INFORMATION_SCHEMA.TABLES, less the ones SQL Server
    # ships itself (so that, like duckdt_schema(), this skips sysdiagrams).
    out <- tryCatch(duckdt_get_query(conn, "
      SELECT s.name AS table_schema, o.name AS table_name,
        CASE o.type WHEN 'V' THEN 'VIEW' ELSE 'BASE TABLE' END AS table_type
      FROM sys.objects o JOIN sys.schemas s ON s.schema_id = o.schema_id
      WHERE o.type IN ('U', 'V') AND o.is_ms_shipped = 0
      ORDER BY s.name, o.name
    "), error = function(e) NULL)
    if (is.data.frame(out)) {
      out <- data.table::setDT(out)
      data.table::setnames(out, c("schema", "name", "type"))
      return(out[])
    }
  }
  out <- data.table::setDT(duckdt_get_query(conn, "
    SELECT table_schema, table_name, table_type
    FROM information_schema.tables
    WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
    ORDER BY table_schema, table_name
  "))
  # Rename in R: SCHEMA is a reserved word on SQL Server.
  # Rename by position: drivers may return uppercase catalogue field names.
  data.table::setnames(out, c("schema", "name", "type"))
  out[]
}

#' List the columns of a database's tables
#'
#' Queries the portable `information_schema.columns`/`table_constraints`
#' views (works against both DuckDB and MS SQL Server connections) for every
#' table's columns, their SQL data type, and whether each is part of a
#' primary key.
#'
#' @param conn A `DBI` connection, or a `"duckdt"` object (its `$conn` is
#'   used).
#' @param table Optionally, restrict to a single table name (matched against
#'   `table_name` in any schema). `NULL` (the default) returns every table.
#'
#' @return A `data.table` with one row per column: `schema`, `table`,
#'   `column`, `type`, `ordinal_position`, `primary_key` (logical).
#' @examples
#' con <- dbdt_example()
#' dbdt_schema(con, table = "orders")
#' dbdt_disconnect(con)
#' @export
duckdt_schema <- function(conn, table = NULL) {
  conn <- duckdt_unwrap_conn(conn)
  if (!is.null(table) && (!is.character(table) || length(table) != 1L || is.na(table))) {
    stop("`table` must be NULL or a single table name.", call. = FALSE)
  }
  if (duckdt_dialect(conn) == "mssql") {
    out <- duckdt_schema_mssql(conn, table)
    if (!is.null(out)) return(out)
  }
  table_filter <- if (is.null(table)) "" else paste0(
    " AND table_name = ", DBI::dbQuoteString(conn, table)
  )

  cols <- data.table::setDT(duckdt_get_query(conn, paste0("
    SELECT
      table_schema, table_name, column_name, data_type, ordinal_position
    FROM information_schema.columns
    WHERE table_schema NOT IN ('information_schema', 'pg_catalog')", table_filter, "
    ORDER BY table_schema, table_name, ordinal_position
  ")))
  data.table::setnames(cols, c("schema", "table", "column", "type", "ordinal_position"))

  pk_filter <- if (is.null(table)) "" else paste0(
    " AND tc.table_name = ", DBI::dbQuoteString(conn, table)
  )
  pks <- data.table::setDT(duckdt_get_query(conn, paste0("
    SELECT tc.table_schema, tc.table_name, kcu.column_name
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu
      ON tc.constraint_name = kcu.constraint_name
     AND tc.table_schema = kcu.table_schema
     AND tc.table_catalog = kcu.table_catalog
     AND tc.table_name = kcu.table_name
    WHERE tc.constraint_type = 'PRIMARY KEY'", pk_filter
  )))
  data.table::setnames(pks, c("schema", "table", "column"))

  cols$primary_key <- duckdt_row_key(cols$schema, cols$table, cols$column) %in%
    duckdt_row_key(pks$schema, pks$table, pks$column)

  cols[]
}

# SQL Server's INFORMATION_SCHEMA.COLUMNS evaluates OBJECT_DEFINITION() and
# COLUMNPROPERTY() for every column, and KEY_COLUMN_USAGE is slower still, so
# on a database with thousands of tables the portable queries above take a
# long time to say what sys.columns says in one join. The type is reported
# the way INFORMATION_SCHEMA does: the base system type, or the user type's
# own name for a CLR type. NULL (so the portable queries run instead) if the
# catalogue views can't be read.
duckdt_schema_mssql <- function(conn, table = NULL) {
  res <- tryCatch(duckdt_get_query(conn, paste0("
    SELECT
      s.name AS table_schema, o.name AS table_name, c.name AS column_name,
      ISNULL(TYPE_NAME(c.system_type_id), TYPE_NAME(c.user_type_id)) AS data_type,
      c.column_id AS ordinal_position,
      CASE WHEN pk.column_id IS NULL THEN 0 ELSE 1 END AS primary_key
    FROM sys.columns c
    JOIN sys.objects o ON o.object_id = c.object_id
    JOIN sys.schemas s ON s.schema_id = o.schema_id
    LEFT JOIN (
      SELECT ic.object_id, ic.column_id
      FROM sys.indexes i
      JOIN sys.index_columns ic ON ic.object_id = i.object_id AND ic.index_id = i.index_id
      WHERE i.is_primary_key = 1
    ) pk ON pk.object_id = c.object_id AND pk.column_id = c.column_id
    WHERE o.type IN ('U', 'V') AND o.is_ms_shipped = 0",
    if (!is.null(table)) paste0(" AND o.name = ", DBI::dbQuoteString(conn, table)), "
    ORDER BY s.name, o.name, c.column_id
  ")), error = function(e) NULL)
  if (!is.data.frame(res)) return(NULL)
  out <- data.table::setDT(res)
  data.table::setnames(out, c("schema", "table", "column", "type", "ordinal_position", "primary_key"))
  data.table::set(out, j = "primary_key", value = as.logical(out$primary_key))
  out[]
}

#' List foreign-key relationships between tables in a database
#'
#' Queries `information_schema` on DuckDB and `sys.foreign_key_columns`
#' on MS SQL Server, pairing each referencing column with its target.
#' Only relationships the database actually declares as
#' constraints are returned — this does not guess relationships from
#' column-naming conventions. If the connected database/version doesn't
#' expose these views, returns a zero-row `data.table` rather than erroring.
#'
#' @param conn A `DBI` connection, or a `"duckdt"` object (its `$conn` is
#'   used).
#'
#' @return A `data.table` with one row per foreign-key column: `fk_schema`,
#'   `fk_table`, `fk_column` (the referencing side) and `pk_schema`,
#'   `pk_table`, `pk_column` (the referenced side).
#' @examples
#' con <- dbdt_example()
#' dbdt_relationships(con)
#' dbdt_disconnect(con)
#' @export
duckdt_relationships <- function(conn) {
  conn <- duckdt_unwrap_conn(conn)
  sql <- if (duckdt_dialect(conn) == "mssql") {
    # SQL Server's information_schema omits the referenced-column position.
    # Its catalog records exact column pairs, including compound keys.
    "
      SELECT
        fs.name AS fk_schema, ft.name AS fk_table, fc.name AS fk_column,
        ps.name AS pk_schema, pt.name AS pk_table, pc.name AS pk_column
      FROM sys.foreign_key_columns fkc
      JOIN sys.tables ft ON ft.object_id = fkc.parent_object_id
      JOIN sys.schemas fs ON fs.schema_id = ft.schema_id
      JOIN sys.columns fc ON fc.object_id = fkc.parent_object_id
                         AND fc.column_id = fkc.parent_column_id
      JOIN sys.tables pt ON pt.object_id = fkc.referenced_object_id
      JOIN sys.schemas ps ON ps.schema_id = pt.schema_id
      JOIN sys.columns pc ON pc.object_id = fkc.referenced_object_id
                         AND pc.column_id = fkc.referenced_column_id
      ORDER BY fs.name, ft.name, fkc.constraint_object_id, fkc.constraint_column_id
    "
  } else {
    "
      SELECT
        kcu1.table_schema AS fk_schema, kcu1.table_name AS fk_table, kcu1.column_name AS fk_column,
        kcu2.table_schema AS pk_schema, kcu2.table_name AS pk_table, kcu2.column_name AS pk_column
      FROM information_schema.referential_constraints rc
      JOIN information_schema.key_column_usage kcu1
        ON rc.constraint_name = kcu1.constraint_name
       AND rc.constraint_schema = kcu1.table_schema
       AND rc.constraint_catalog = kcu1.constraint_catalog
      JOIN information_schema.key_column_usage kcu2
        ON rc.unique_constraint_name = kcu2.constraint_name
       AND rc.unique_constraint_schema = kcu2.table_schema
       AND rc.unique_constraint_catalog = kcu2.constraint_catalog
       AND kcu1.position_in_unique_constraint = kcu2.ordinal_position
      ORDER BY kcu1.table_schema, kcu1.table_name, rc.constraint_name, kcu1.ordinal_position
    "
  }
  fks <- tryCatch(
    duckdt_get_query(conn, sql),
    error = function(e) data.frame(
      fk_schema = character(), fk_table = character(), fk_column = character(),
      pk_schema = character(), pk_table = character(), pk_column = character()
    )
  )
  data.table::setDT(fks)[]
}

duckdt_unwrap_conn <- function(conn) if (inherits(conn, "duckdt")) conn$conn else conn

duckdt_row_key <- function(...) do.call(paste, c(list(...), sep = ""))
