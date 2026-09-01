#' Explore a database in a Shiny app
#'
#' The interactive version of [duckdt_erd()]: the same tick-a-table,
#' tick-a-column browsing, but with a live diagram, the generated
#' `duckdt`/SQL code, and -- since it has a connection rather than a static
#' page -- a preview of the rows the query actually returns.
#'
#' Needs the `shiny` package. The diagram is drawn with Graphviz via
#' `DiagrammeR` if that's installed, and falls back to showing the DOT
#' source if not; the row preview uses `DT` if that's installed. Nothing
#' beyond duckdt itself is needed for [duckdt_erd()], which covers the same
#' ground in a plain HTML page.
#'
#' @param x A `DBI` connection, a `"duckdt"` object, or a
#'   `"duckdt_data_model"`. Given a model rather than a connection, the app
#'   browses and builds queries but cannot run them.
#' @param tables Optionally, a character vector of tables to model.
#' @param infer_references Guess undeclared foreign keys from column naming
#'   conventions, via [duckdt_dm_infer_references()].
#' @param row_counts Show each table's row count (a `count(*)` per table).
#' @param ... Passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object. Printed at the console (or returned from a
#'   `.R` file passed to [shiny::runApp()]) it starts the app.
#' @seealso [duckdt_erd()] for the dependency-free browser version.
#' @examples
#' \dontrun{
#' con <- duckdt_connect("C:/data/warehouse.duckdb", read_only = TRUE)
#' duckdt_explorer(con, infer_references = TRUE)
#' }
#' @export
duckdt_explorer <- function(x, tables = NULL, infer_references = FALSE,
                            row_counts = FALSE, ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("duckdt: duckdt_explorer() needs the shiny package. ",
      "Install it, or use duckdt_erd() which needs nothing extra.", call. = FALSE)
  }
  dm <- if (is_duckdt_data_model(x)) {
    x
  } else {
    duckdt_data_model(x, tables = tables, infer_references = infer_references,
      row_counts = row_counts)
  }
  conn <- if (is_duckdt_data_model(x)) NULL else duckdt_unwrap_conn(x)

  shiny::shinyApp(
    ui = duckdt_explorer_ui(dm, conn),
    server = duckdt_explorer_server(dm, conn),
    ...
  )
}

duckdt_explorer_ui <- function(dm, conn) {
  tabs <- as.data.frame(dm$tables)
  choices <- stats::setNames(
    tabs$table,
    ifelse(is.na(tabs$n_rows), tabs$table,
      sprintf("%s (%s rows)", tabs$table, format(tabs$n_rows, big.mark = ",", trim = TRUE)))
  )

  shiny::fluidPage(
    title = "duckdt explorer",
    shiny::tags$style(shiny::HTML(
      ".duckdt-title{font-weight:700;letter-spacing:-.02em}
       .duckdt-hint{color:#6b7280;font-size:12px}
       pre.duckdt-code{background:#1e2430;color:#e6e9ef;padding:12px;border-radius:6px}"
    )),
    shiny::titlePanel(shiny::div(
      shiny::span("duckdt explorer", class = "duckdt-title"),
      shiny::span(duckdt_conn_label(if (is.null(conn)) dm else conn), class = "duckdt-hint")
    )),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectizeInput(
          "tables", "Tables", choices = choices, multiple = TRUE,
          options = list(placeholder = "Pick one or more tables")
        ),
        shiny::uiOutput("column_pickers"),
        shiny::hr(),
        shiny::textInput("where", "WHERE (SQL)", placeholder = "total > 100"),
        shiny::numericInput("limit", "Preview rows", value = 100, min = 1, step = 50),
        if (!is.null(conn)) shiny::actionButton("run", "Run query", class = "btn-primary"),
        shiny::hr(),
        shiny::selectInput("view", "Detail",
          c("All columns" = "all", "Keys only" = "keys_only", "Tables only" = "title_only")),
        shiny::selectInput("rankdir", "Layout",
          c("Bottom to top" = "BT", "Left to right" = "LR", "Top to bottom" = "TB")),
        shiny::checkboxInput("column_arrows", "Arrows between columns", FALSE)
      ),
      shiny::mainPanel(
        width = 9,
        shiny::tabsetPanel(
          id = "tab",
          shiny::tabPanel("Diagram", shiny::br(), shiny::uiOutput("diagram")),
          shiny::tabPanel("Rows", shiny::br(), shiny::uiOutput("rows")),
          shiny::tabPanel("Code", shiny::br(),
            shiny::h5("R"), shiny::verbatimTextOutput("r_code"),
            shiny::h5("SQL"), shiny::verbatimTextOutput("sql_code")
          ),
          shiny::tabPanel("Columns", shiny::br(), shiny::uiOutput("schema"))
        )
      )
    )
  )
}

duckdt_explorer_server <- function(dm, conn) {
  all_cols <- as.data.frame(dm$columns)

  function(input, output, session) {
    selected_columns <- shiny::reactive({
      picks <- lapply(input$tables, function(t) {
        chosen <- input[[paste0("cols_", duckdt_input_id(t))]]
        if (is.null(chosen)) all_cols$column[all_cols$table == t] else chosen
      })
      stats::setNames(picks, input$tables)
    })

    model <- shiny::reactive({
      shiny::req(length(input$tables) > 0)
      duckdt_dm_filter(dm, input$tables, columns = selected_columns())
    })

    sql <- shiny::reactive({
      shiny::req(length(input$tables) > 0)
      duckdt_dm_query(
        dm, input$tables, columns = selected_columns(),
        where = if (nzchar(input$where)) input$where else NULL,
        limit = input$limit
      )
    })

    output$column_pickers <- shiny::renderUI({
      shiny::req(length(input$tables) > 0)
      lapply(input$tables, function(t) {
        cols <- all_cols$column[all_cols$table == t]
        shiny::selectizeInput(
          paste0("cols_", duckdt_input_id(t)),
          shiny::HTML(paste0("Columns in <code>", t, "</code>")),
          choices = cols, selected = cols, multiple = TRUE
        )
      })
    })

    output$diagram <- shiny::renderUI({
      if (!length(input$tables)) {
        return(shiny::helpText("Pick a table on the left to draw it."))
      }
      dot <- duckdt_dm_dot(
        model(), view = input$view, rankdir = input$rankdir,
        column_arrows = input$column_arrows
      )
      if (requireNamespace("DiagrammeR", quietly = TRUE)) {
        DiagrammeR::grViz(unclass(dot), allow_subst = FALSE)
      } else {
        shiny::tagList(
          shiny::helpText("Install the DiagrammeR package to draw this. The Graphviz source:"),
          shiny::tags$pre(class = "duckdt-code", unclass(dot))
        )
      }
    })

    preview <- shiny::eventReactive(input$run, {
      shiny::req(conn, length(input$tables) > 0)
      tryCatch(
        data.table::setDT(DBI::dbGetQuery(conn, sql()))[],
        error = function(e) conditionMessage(e)
      )
    })

    output$rows <- shiny::renderUI({
      if (is.null(conn)) {
        return(shiny::helpText(
          "This explorer was opened on a data model rather than a connection, ",
          "so there is nothing to run the query against."
        ))
      }
      if (!isTRUE(input$run > 0)) {
        return(shiny::helpText("Press 'Run query' to preview the rows."))
      }
      res <- preview()
      if (is.character(res)) {
        return(shiny::div(shiny::strong("Query failed: "), res))
      }
      if (requireNamespace("DT", quietly = TRUE)) {
        DT::datatable(res, options = list(pageLength = 25, scrollX = TRUE),
          rownames = FALSE)
      } else {
        shiny::renderTable(utils::head(res, 25))()
      }
    })

    output$sql_code <- shiny::renderText({
      if (!length(input$tables)) "-- Pick a table on the left." else unclass(sql())
    })

    output$r_code <- shiny::renderText({
      if (!length(input$tables)) return("# Pick a table on the left.")
      duckdt_explorer_r_code(dm, input$tables, selected_columns(), unclass(sql()))
    })

    output$schema <- shiny::renderUI({
      cols <- all_cols
      if (length(input$tables)) cols <- cols[cols$table %in% input$tables, , drop = FALSE]
      cols$key <- ifelse(cols$key > 0, "PK", "")
      cols$ref <- ifelse(is.na(cols$ref), "", paste0(cols$ref, ".", cols$ref_col))
      cols <- cols[, c("table", "column", "type", "key", "ref")]
      names(cols) <- c("Table", "Column", "Type", "Key", "References")
      if (requireNamespace("DT", quietly = TRUE)) {
        DT::datatable(cols, options = list(pageLength = 25), rownames = FALSE,
          filter = "top")
      } else {
        shiny::renderTable(cols)()
      }
    })
  }
}

# Shiny input ids can't carry the dots and quotes a schema-qualified table
# name might.
duckdt_input_id <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

duckdt_explorer_r_code <- function(dm, tables, columns, sql) {
  tabs <- as.data.frame(dm$tables)
  if (length(tables) == 1) {
    i <- match(tables, tabs$table)
    picked <- columns[[tables]]
    plain <- is.na(tabs$schema[i]) || tabs$schema[i] %in% c("main", "dbo")
    all_of <- setdiff(as.data.frame(dm$columns)$column[dm$columns$table == tables], picked)
    if (plain) {
      code <- sprintf('d <- duckdt(con, "%s")', tabs$name[i])
      if (!length(all_of)) {
        return(paste(code, "d[]", sep = "\n"))
      }
      if (all(grepl("^[A-Za-z.][A-Za-z0-9._]*$", picked))) {
        return(paste(code, sprintf("d[, .(%s)]", paste(picked, collapse = ", ")), sep = "\n"))
      }
      return(paste(code, sprintf('d[, .SD, .SDcols = c("%s")]',
        paste(picked, collapse = '", "')), sep = "\n"))
    }
  }
  paste0("res <- DBI::dbGetQuery(con, '\n", sql, "\n')\ndata.table::setDT(res)[]")
}
