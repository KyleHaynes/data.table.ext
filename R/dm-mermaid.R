#' Render a data model as a Mermaid ER diagram
#'
#' Mermaid needs no R packages and renders anywhere Markdown does, so this is
#' what [dbdt_erd()] draws and what to paste into an Rmd/Quarto
#' ```` ```mermaid ```` chunk or a GitHub comment.
#'
#' @param dm A `"duckdt_data_model"` from [dbdt_data_model()], or anything
#'   [dbdt_data_model()] accepts (a connection, a `"duckdt"` handle, a
#'   named list of data frames).
#' @param view How much of each table to draw: `"all"` columns,
#'   `"keys_only"` (primary and foreign keys), or `"title_only"`.
#' @param types Show each column's SQL type. Default `TRUE`.
#'
#' @return A length-1 character vector of Mermaid `erDiagram` source.
#' @seealso [dbdt_dm_dot()] for the Graphviz rendering,
#'   [dbdt_erd()] for the interactive browser page.
#' @examples
#' dm <- dbdt_data_model(list(
#'   orders = data.frame(id = 1L, customer_id = 1L),
#'   customers = data.frame(id = 1L, name = "a")
#' ))
#' dm <- dbdt_dm_add_references(dm, orders$customer_id == customers$id)
#' cat(dbdt_dm_mermaid(dm))
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
  cols <- cols[cols$table %in% tabs$table, , drop = FALSE]

  # Every line is built in one vectorised pass and then grouped by table:
  # growing the output a line at a time is quadratic, which a schema with
  # tens of thousands of columns makes very noticeable.
  is_pk <- cols$key > 0
  is_fk <- !is.na(cols$ref)
  keys <- ifelse(is_pk & is_fk, " PK,FK", ifelse(is_pk, " PK", ifelse(is_fk, " FK", "")))
  col_lines <- sprintf(
    "        %s %s%s",
    if (types) duckdt_mermaid_safe(cols$type) else rep("col", nrow(cols)),
    duckdt_mermaid_safe(cols$column),
    keys
  )
  by_table <- split(col_lines, factor(cols$table, levels = tabs$table))
  blocks <- unlist(Map(
    function(entity, body) c(sprintf("    %s {", entity), body, "    }"),
    tabs$entity, by_table
  ), use.names = FALSE)

  refs <- as.data.frame(dm$references)
  refs <- refs[refs$table %in% tabs$table & refs$ref %in% tabs$table, , drop = FALSE]
  entities <- stats::setNames(tabs$entity, tabs$table)
  first <- refs[!duplicated(refs$ref_id), , drop = FALSE]
  labels <- vapply(
    split(refs$column, factor(refs$ref_id, levels = first$ref_id)),
    paste, character(1), collapse = ", "
  )
  ref_lines <- sprintf(
    '    %s ||--o{ %s : "%s"',
    entities[first$ref], entities[first$table], labels
  )

  paste(c("erDiagram", blocks, if (nrow(first)) ref_lines), collapse = "\n")
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
