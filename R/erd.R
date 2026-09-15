#' Explore a database's tables and relationships in your browser
#'
#' Builds a data model ([duckdt_data_model()]) and writes it to a
#' self-contained HTML page: an ER diagram of the tables you tick, a
#' searchable list of every table and column, and -- as you tick columns --
#' the `duckdt`/SQL code that selects exactly those, ready to copy. Useful
#' for finding your way around a database you didn't build, without writing
#' any SQL first. Works against both DuckDB and MS SQL Server connections,
#' and needs no packages beyond duckdt itself.
#'
#' Only foreign keys the database actually declares as constraints are drawn.
#' DuckDB files built by loading CSV/Parquet usually declare none, in which
#' case pass `infer_references = TRUE` to guess them from column names (see
#' [duckdt_dm_infer_references()] for exactly what that guesses), or state
#' them yourself with [duckdt_dm_add_references()] and pass the resulting
#' model straight to this function.
#'
#' The page renders its diagram with [Mermaid](https://mermaid.js.org/)
#' loaded from a CDN. With no internet connection everything still works --
#' the table/column browser, the generated code -- except the drawing itself,
#' which is replaced by its source.
#'
#' @param x A `DBI` connection, a `"duckdt"` object, or a
#'   `"duckdt_data_model"` (from [duckdt_data_model()], so you can filter or
#'   annotate it first).
#' @param tables Optionally, a character vector of tables to model. Others
#'   are left out entirely.
#' @param include_row_counts Run a `SELECT count(*)` per table and show it
#'   next to each table. Default `FALSE`, since this can be slow on a large
#'   or remote database; a failure on any single table shows as no count
#'   rather than failing the whole call.
#' @param open Open the generated page with [utils::browseURL()]. Default
#'   `TRUE`.
#' @param infer_references Guess undeclared foreign keys from column naming
#'   conventions, via [duckdt_dm_infer_references()]. Default `FALSE`.
#' @param select Tables to start with ticked. Defaults to all of them when
#'   there are 12 or fewer, otherwise none.
#' @param view Initial level of detail: `"all"` columns, `"keys_only"` or
#'   `"title_only"`.
#' @param title Page title. Defaults to a description of the connection.
#' @param file Where to write the page. Defaults to a temporary file.
#'
#' @return Invisibly, the path to the generated HTML file, with the Mermaid
#'   diagram source attached as its `"mermaid"` attribute (so it can be
#'   dropped straight into an Rmd/Quarto ```` ```mermaid ```` chunk) and the
#'   data model as its `"data_model"` attribute.
#' @seealso [duckdt_explorer()] for the Shiny version, which also previews
#'   and runs the query; [duckdt_dm_mermaid()] and [duckdt_dm_dot()] for the
#'   diagram sources on their own.
#' @examples
#' con <- duckdt_connect(quiet = TRUE)
#' DBI::dbExecute(con, "CREATE TABLE customers (id INTEGER PRIMARY KEY, name VARCHAR)")
#' DBI::dbExecute(con, "CREATE TABLE orders (
#'   id INTEGER PRIMARY KEY, customer_id INTEGER REFERENCES customers(id), total DOUBLE)")
#'
#' path <- duckdt_erd(con, open = FALSE)
#' cat(attr(path, "mermaid"))
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' @export
duckdt_erd <- function(x, tables = NULL, include_row_counts = FALSE, open = TRUE,
                       infer_references = FALSE, select = NULL,
                       view = c("all", "keys_only", "title_only"),
                       title = NULL, file = NULL) {
  view <- match.arg(view)
  dm <- if (is_duckdt_data_model(x)) {
    x
  } else {
    duckdt_data_model(
      x, tables = tables, infer_references = infer_references,
      row_counts = include_row_counts
    )
  }
  if (is.null(title)) title <- duckdt_conn_label(x)
  if (is.null(file)) file <- tempfile(pattern = "duckdt-erd-", fileext = ".html")

  mermaid <- duckdt_dm_mermaid(dm, view = view)

  tabs <- as.data.frame(dm$tables)
  tabs$entity <- unname(duckdt_dm_entities(dm)[tabs$table])
  model_json <- duckdt_to_json(list(
    tables = tabs[, c("table", "schema", "name", "type", "n_rows", "segment",
                      "display", "entity")],
    columns = as.data.frame(dm$columns),
    references = as.data.frame(dm$references),
    selected = if (is.null(select)) list() else as.list(select),
    dbdir = duckdt_dbdir(x),
    title = title
  ))

  html <- duckdt_fill_template(
    duckdt_asset("erd", "erd.html"),
    TITLE = duckdt_html_escape(title),
    CSS = duckdt_asset_text("erd", "erd.css"),
    JS = duckdt_asset_text("erd", "erd.js"),
    MODEL = model_json,
    MERMAID_SRC = getOption("duckdt.mermaid_src", duckdt_mermaid_cdn)
  )
  writeLines(html, file, useBytes = TRUE)

  if (isTRUE(open)) utils::browseURL(file)

  attr(file, "mermaid") <- mermaid
  attr(file, "data_model") <- dm
  invisible(file)
}

duckdt_mermaid_cdn <- "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js"

# ---- template plumbing -----------------------------------------------------

duckdt_asset <- function(...) {
  path <- system.file(..., package = "data.table.ext")
  if (!nzchar(path)) {
    stop("duckdt: could not find the packaged file ", file.path(...),
      ". Is the package installed correctly?", call. = FALSE)
  }
  path
}

duckdt_asset_text <- function(...) {
  paste(readLines(duckdt_asset(...), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Replace {{NAME}} placeholders. Values are inserted literally (they are
# already-escaped HTML, CSS, JS or JSON), so `fixed = TRUE` on both sides.
duckdt_fill_template <- function(path, ...) {
  values <- list(...)
  out <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (nm in names(values)) {
    out <- gsub(paste0("{{", nm, "}}"), values[[nm]], out, fixed = TRUE)
  }
  out
}
