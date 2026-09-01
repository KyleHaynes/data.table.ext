#' Render a data model as a Mermaid ER diagram
#'
#' Mermaid needs no R packages and renders anywhere Markdown does, so this is
#' what [duckdt_erd()] draws and what to paste into an Rmd/Quarto
#' ```` ```mermaid ```` chunk or a GitHub comment.
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()], or anything
#'   [duckdt_data_model()] accepts (a connection, a `"duckdt"` handle, a
#'   named list of data frames).
#' @param view How much of each table to draw: `"all"` columns,
#'   `"keys_only"` (primary and foreign keys), or `"title_only"`.
#' @param types Show each column's SQL type. Default `TRUE`.
#'
#' @return A length-1 character vector of Mermaid `erDiagram` source.
#' @seealso [duckdt_dm_dot()] for the Graphviz rendering,
#'   [duckdt_erd()] for the interactive browser page.
#' @examples
#' dm <- duckdt_data_model(list(
#'   orders = data.frame(id = 1L, customer_id = 1L),
#'   customers = data.frame(id = 1L, name = "a")
#' ))
#' dm <- duckdt_dm_add_references(dm, orders$customer_id == customers$id)
#' cat(duckdt_dm_mermaid(dm))
#' @export
duckdt_dm_mermaid <- function(dm, view = c("all", "keys_only", "title_only"),
                              types = TRUE) {
  dm <- duckdt_as_data_model(dm)
  view <- match.arg(view)

  tabs <- as.data.frame(dm$tables)
  tabs <- tabs[!duckdt_dm_hidden(tabs$display), , drop = FALSE]
  if (!nrow(tabs)) stop("duckdt: nothing left to draw.", call. = FALSE)
  tabs$entity <- duckdt_dm_entities(dm)[tabs$table]

  cols <- duckdt_dm_visible_columns(dm, view)

  lines <- "erDiagram"
  for (i in seq_len(nrow(tabs))) {
    tab_cols <- cols[cols$table == tabs$table[i], , drop = FALSE]
    lines <- c(lines, sprintf("    %s {", tabs$entity[i]))
    for (j in seq_len(nrow(tab_cols))) {
      keys <- c(
        if (tab_cols$key[j] > 0) "PK",
        if (!is.na(tab_cols$ref[j])) "FK"
      )
      lines <- c(lines, sprintf(
        "        %s %s%s",
        if (types) duckdt_mermaid_safe(tab_cols$type[j]) else "col",
        duckdt_mermaid_safe(tab_cols$column[j]),
        if (length(keys)) paste0(" ", paste(keys, collapse = ",")) else ""
      ))
    }
    lines <- c(lines, "    }")
  }

  refs <- as.data.frame(dm$references)
  refs <- refs[refs$table %in% tabs$table & refs$ref %in% tabs$table, , drop = FALSE]
  entities <- stats::setNames(tabs$entity, tabs$table)
  for (id in unique(refs$ref_id)) {
    one <- refs[refs$ref_id == id, , drop = FALSE]
    lines <- c(lines, sprintf(
      '    %s ||--o{ %s : "%s"',
      entities[[one$ref[1]]], entities[[one$table[1]]],
      paste(one$column, collapse = ", ")
    ))
  }

  paste(lines, collapse = "\n")
}

# Mermaid entity names must be plain identifiers; keep the schema in them so
# two same-named tables in different schemas stay distinct.
duckdt_dm_entities <- function(dm) {
  tabs <- as.data.frame(dm$tables)
  raw <- ifelse(is.na(tabs$schema), tabs$name, paste0(tabs$schema, "_", tabs$name))
  ids <- make.unique(gsub("[^A-Za-z0-9_]", "_", raw), sep = "_")
  stats::setNames(ids, tabs$table)
}

duckdt_mermaid_safe <- function(x) {
  x[is.na(x)] <- "unknown"
  gsub("[^A-Za-z0-9_]+", "_", x)
}
