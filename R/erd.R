#' Visualize a database's tables and foreign-key relationships
#'
#' Builds on [duckdt_tables()], [duckdt_schema()], and [duckdt_relationships()]
#' to render a Mermaid `erDiagram` and open it as a self-contained HTML page
#' in the system browser. Useful for quickly seeing what tables are
#' available in a database and how they connect, without writing any SQL
#' yourself. Works against both DuckDB and MS SQL Server connections.
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
  conn <- duckdt_unwrap_conn(conn)

  tables <- duckdt_tables(conn)
  if (nrow(tables) == 0) {
    stop("duckdt: no tables/views found on this connection.", call. = FALSE)
  }
  cols <- duckdt_schema(conn)
  fks <- duckdt_relationships(conn)

  entity_id <- function(schema, table) gsub("[^A-Za-z0-9_]", "_", paste0(schema, "_", table))
  tables$entity <- entity_id(tables$schema, tables$name)
  tables$full_name <- paste0(tables$schema, ".", tables$name)

  row_counts <- NULL
  if (isTRUE(include_row_counts)) {
    row_counts <- stats::setNames(rep(NA_real_, nrow(tables)), tables$full_name)
    for (i in seq_len(nrow(tables))) {
      qname <- paste0(
        DBI::dbQuoteIdentifier(conn, tables$schema[i]), ".",
        DBI::dbQuoteIdentifier(conn, tables$name[i])
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
    sch <- tables$schema[i]
    tbl <- tables$name[i]
    ent <- tables$entity[i]
    cols_i <- cols[cols$schema == sch & cols$table == tbl, , drop = FALSE]

    lines <- c(lines, sprintf("    %s {", ent))
    if (nrow(cols_i) > 0) {
      for (j in seq_len(nrow(cols_i))) {
        key_tag <- if (isTRUE(cols_i$primary_key[j])) " PK" else ""
        lines <- c(lines, sprintf(
          "        %s %s%s",
          clean_type(cols_i$type[j]), cols_i$column[j], key_tag
        ))
      }
    }
    lines <- c(lines, "    }")
  }

  if (nrow(fks) > 0) {
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
    tables$full_name, tables$type,
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
