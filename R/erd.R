#' Visualize a database's tables and foreign-key relationships
#'
#' Introspects `conn`'s tables, columns, primary keys, and foreign keys via
#' the portable `information_schema` views (works against both DuckDB and MS
#' SQL Server connections), renders them as a Mermaid `erDiagram`, and opens
#' the result as a self-contained HTML page in the system browser. Useful for
#' quickly seeing what tables are available in a database and how they
#' connect, without writing any SQL yourself.
#'
#' Foreign keys are only detected when the database actually declares them as
#' constraints — this does not guess relationships from column-naming
#' conventions.
#'
#' @param conn A `DBI` connection, or a `"duckdt"` object (its `$conn` is
#'   used).
#' @param include_row_counts Include a `SELECT count(*)` per table in the
#'   diagram. Default `FALSE`, since this can be slow on a large or remote
#'   database; a failure on any single table is skipped rather than failing
#'   the whole call.
#' @param open Open the generated HTML file with [utils::browseURL()].
#'   Default `TRUE`.
#'
#' @return Invisibly, the path to the generated HTML file. The Mermaid
#'   diagram source is attached as the `"mermaid"` attribute, so it can be
#'   dropped directly into an Rmd/Quarto ```` ```mermaid ```` code chunk.
#' @export
duckdt_erd <- function(conn, include_row_counts = FALSE, open = TRUE) {
  if (inherits(conn, "duckdt")) conn <- conn$conn

  tables <- DBI::dbGetQuery(conn, "
    SELECT table_schema, table_name, table_type
    FROM information_schema.tables
    WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
    ORDER BY table_schema, table_name
  ")
  if (nrow(tables) == 0) {
    stop("duckdt: no tables/views found on this connection.", call. = FALSE)
  }

  columns <- DBI::dbGetQuery(conn, "
    SELECT table_schema, table_name, column_name, data_type, ordinal_position
    FROM information_schema.columns
    WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
    ORDER BY table_schema, table_name, ordinal_position
  ")

  pks <- DBI::dbGetQuery(conn, "
    SELECT tc.table_schema, tc.table_name, kcu.column_name
    FROM information_schema.table_constraints tc
    JOIN information_schema.key_column_usage kcu
      ON tc.constraint_name = kcu.constraint_name
     AND tc.table_schema = kcu.table_schema
    WHERE tc.constraint_type = 'PRIMARY KEY'
  ")

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
    error = function(e) NULL
  )

  entity_id <- function(schema, table) gsub("[^A-Za-z0-9_]", "_", paste0(schema, "_", table))
  tables$entity <- entity_id(tables$table_schema, tables$table_name)
  tables$full_name <- paste0(tables$table_schema, ".", tables$table_name)

  row_counts <- NULL
  if (isTRUE(include_row_counts)) {
    row_counts <- stats::setNames(rep(NA_real_, nrow(tables)), tables$full_name)
    for (i in seq_len(nrow(tables))) {
      qname <- paste0(
        DBI::dbQuoteIdentifier(conn, tables$table_schema[i]), ".",
        DBI::dbQuoteIdentifier(conn, tables$table_name[i])
      )
      n <- tryCatch(
        DBI::dbGetQuery(conn, paste0("SELECT count(*) AS n FROM ", qname))$n[1],
        error = function(e) NA_real_
      )
      row_counts[tables$full_name[i]] <- n
    }
  }

  clean_type <- function(type) gsub("[^A-Za-z0-9_]+", "_", type)

  lines <- c("erDiagram")
  for (i in seq_len(nrow(tables))) {
    sch <- tables$table_schema[i]
    tbl <- tables$table_name[i]
    ent <- tables$entity[i]
    cols_i <- columns[columns$table_schema == sch & columns$table_name == tbl, , drop = FALSE]
    pk_cols <- pks$column_name[pks$table_schema == sch & pks$table_name == tbl]

    lines <- c(lines, sprintf("    %s {", ent))
    if (nrow(cols_i) > 0) {
      for (j in seq_len(nrow(cols_i))) {
        key_tag <- if (cols_i$column_name[j] %in% pk_cols) " PK" else ""
        lines <- c(lines, sprintf(
          "        %s %s%s",
          clean_type(cols_i$data_type[j]), cols_i$column_name[j], key_tag
        ))
      }
    }
    lines <- c(lines, "    }")
  }

  if (!is.null(fks) && nrow(fks) > 0) {
    fks$fk_entity <- entity_id(fks$fk_schema, fks$fk_table)
    fks$pk_entity <- entity_id(fks$pk_schema, fks$pk_table)
    known <- fks$fk_entity %in% tables$entity & fks$pk_entity %in% tables$entity
    fks <- fks[known, , drop = FALSE]
    for (i in seq_len(nrow(fks))) {
      lines <- c(lines, sprintf(
        '    %s ||--o{ %s : "%s"',
        fks$pk_entity[i], fks$fk_entity[i], fks$fk_column[i]
      ))
    }
  }

  mermaid <- paste(lines, collapse = "\n")

  summary_rows <- sprintf(
    "<tr><td>%s</td><td>%s</td>%s</tr>",
    tables$full_name, tables$table_type,
    if (is.null(row_counts)) "" else {
      paste0("<td>", ifelse(is.na(row_counts[tables$full_name]), "-",
        format(row_counts[tables$full_name], big.mark = ",")), "</td>")
    }
  )

  html <- sprintf('<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>duckdt: database schema</title>
<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
<style>
  body { font-family: sans-serif; margin: 2rem; }
  table { border-collapse: collapse; margin-bottom: 2rem; }
  td, th { border: 1px solid #ccc; padding: 4px 10px; text-align: left; }
  .mermaid { overflow-x: auto; }
</style>
</head>
<body>
<h1>Database schema</h1>
<table>
<tr><th>Table</th><th>Type</th>%s</tr>
%s
</table>
<pre class="mermaid">
%s
</pre>
<script>mermaid.initialize({ startOnLoad: true });</script>
</body>
</html>
', if (is.null(row_counts)) "" else "<th>Rows</th>",
    paste(summary_rows, collapse = "\n"), mermaid)

  path <- tempfile(fileext = ".html")
  writeLines(html, path)
  if (isTRUE(open)) utils::browseURL(path)

  attr(path, "mermaid") <- mermaid
  invisible(path)
}
