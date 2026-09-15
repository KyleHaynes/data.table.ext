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
#' @export
duckdt_tables <- function(conn) {
  conn <- duckdt_unwrap_conn(conn)
  data.table::setDT(DBI::dbGetQuery(conn, "
    SELECT table_schema AS schema, table_name AS name, table_type AS type
    FROM information_schema.tables
    WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
    ORDER BY table_schema, table_name
  "))[]
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
#' @export
duckdt_schema <- function(conn, table = NULL) {
  conn <- duckdt_unwrap_conn(conn)

  cols <- DBI::dbGetQuery(conn, "
    SELECT
      table_schema AS schema, table_name AS \"table\", column_name AS \"column\",
      data_type AS type, ordinal_position
    FROM information_schema.columns
    WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
    ORDER BY table_schema, table_name, ordinal_position
  ")

  pks <- DBI::dbGetQuery(conn, "
    SELECT tc.table_schema AS schema, tc.table_name AS \"table\", kcu.column_name AS \"column\"
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu
      ON tc.constraint_name = kcu.constraint_name
     AND tc.table_schema = kcu.table_schema
    WHERE tc.constraint_type = 'PRIMARY KEY'
  ")

  cols$primary_key <- duckdt_row_key(cols$schema, cols$table, cols$column) %in%
    duckdt_row_key(pks$schema, pks$table, pks$column)

  if (!is.null(table)) cols <- cols[cols$table == table, , drop = FALSE]

  data.table::setDT(cols)[]
}

#' List foreign-key relationships between tables in a database
#'
#' Queries the portable `information_schema.referential_constraints`/
#' `key_column_usage` views (works against both DuckDB and MS SQL Server
#' connections). Only relationships the database actually declares as
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
#' @export
duckdt_relationships <- function(conn) {
  conn <- duckdt_unwrap_conn(conn)
  fks <- tryCatch(
    DBI::dbGetQuery(conn, "
      SELECT
        kcu1.table_schema AS fk_schema, kcu1.table_name AS fk_table, kcu1.column_name AS fk_column,
        kcu2.table_schema AS pk_schema, kcu2.table_name AS pk_table, kcu2.column_name AS pk_column
      FROM information_schema.referential_constraints rc
      JOIN information_schema.key_column_usage kcu1
        ON rc.constraint_name = kcu1.constraint_name
       AND rc.constraint_schema = kcu1.table_schema
      JOIN information_schema.key_column_usage kcu2
        ON rc.unique_constraint_name = kcu2.constraint_name
       AND rc.unique_constraint_schema = kcu2.table_schema
    "),
    error = function(e) data.frame(
      fk_schema = character(), fk_table = character(), fk_column = character(),
      pk_schema = character(), pk_table = character(), pk_column = character()
    )
  )
  data.table::setDT(fks)[]
}

duckdt_unwrap_conn <- function(conn) if (inherits(conn, "duckdt")) conn$conn else conn

duckdt_row_key <- function(...) do.call(paste, c(list(...), sep = ""))
