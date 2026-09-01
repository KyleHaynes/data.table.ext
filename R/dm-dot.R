#' Render a data model as Graphviz DOT
#'
#' A port of datamodelr's `dm_create_graph()`: each table becomes a
#' `plaintext` node whose label is an HTML table of its columns, primary keys
#' underlined, and each reference becomes an edge. Segments
#' ([duckdt_dm_set_segment()]) become labelled clusters, and displays
#' ([duckdt_dm_set_display()]) pick the colours.
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()], or anything
#'   [duckdt_data_model()] accepts (a connection, a `"duckdt"` handle, a
#'   named list of data frames).
#' @param view How much of each table to draw: `"all"` columns,
#'   `"keys_only"` (primary and foreign keys), or `"title_only"` (just the
#'   table names).
#' @param col_attr Which column attributes to show in each row. `"column"`
#'   (the name) by default; `c("column", "type")` also shows SQL types.
#' @param rankdir Graphviz layout direction, e.g. `"BT"` (bottom to top,
#'   the default), `"LR"`, `"TB"`, `"RL"`.
#' @param column_arrows Draw edges between the referencing and referenced
#'   *columns* rather than between the table boxes.
#' @param graph_name Graph tooltip/name.
#' @param graph_attrs,node_attrs,edge_attrs Extra Graphviz attribute strings,
#'   appended to the defaults.
#'
#' @return A length-1 character vector of DOT source, with class
#'   `"duckdt_dot"` so printing it shows the diagram (via `DiagrammeR`, if
#'   installed) rather than the source text. Use [cat()] to see the source.
#' @seealso [duckdt_dm_render()] to get a widget object directly,
#'   [duckdt_dm_export()] to write an image file, [duckdt_erd()] for the
#'   dependency-free browser version.
#' @examples
#' dm <- duckdt_data_model(list(
#'   orders = data.frame(id = 1L, customer_id = 1L),
#'   customers = data.frame(id = 1L, name = "a")
#' ))
#' dm <- duckdt_dm_add_references(dm, orders$customer_id == customers$id)
#' cat(substr(duckdt_dm_dot(dm), 1, 60))
#' @export
duckdt_dm_dot <- function(dm, view = c("all", "keys_only", "title_only"),
                          col_attr = "column", rankdir = "BT",
                          column_arrows = FALSE, graph_name = "Data model",
                          graph_attrs = "", node_attrs = "", edge_attrs = "") {
  dm <- duckdt_as_data_model(dm)
  view <- match.arg(view)

  cols <- duckdt_dm_visible_columns(dm, view)
  tabs <- as.data.frame(dm$tables)
  tabs <- tabs[!duckdt_dm_hidden(tabs$display), , drop = FALSE]
  cols <- cols[cols$table %in% tabs$table, , drop = FALSE]
  if (!all(col_attr %in% names(cols))) {
    stop("duckdt: `col_attr` must name columns of `dm$columns`.", call. = FALSE)
  }
  if (!nrow(tabs)) stop("duckdt: nothing left to draw.", call. = FALSE)

  segments <- unique(stats::na.omit(tabs$segment))
  seg_id <- stats::setNames(seq_along(segments), segments)

  nodes <- vapply(seq_len(nrow(tabs)), function(i) {
    tab <- tabs$table[i]
    label <- duckdt_dot_label(
      cols[cols$table == tab, , drop = FALSE],
      title = tab,
      palette_id = tabs$display[i],
      col_attr = col_attr,
      column_arrows = column_arrows
    )
    node <- sprintf("  '%s' [label = %s, shape = 'plaintext']\n", tab, label)
    if (!is.na(tabs$segment[i])) {
      node <- sprintf(
        "subgraph cluster_%s {\nlabel='%s'\ncolor=\"#DDDDDD\"\n%s}\n",
        seg_id[[tabs$segment[i]]], tabs$segment[i], node
      )
    }
    node
  }, character(1))

  refs <- as.data.frame(dm$references)
  refs <- refs[refs$table %in% tabs$table & refs$ref %in% tabs$table, , drop = FALSE]
  edges <- if (!nrow(refs)) {
    ""
  } else if (column_arrows) {
    paste(sprintf("'%s':'%s'->'%s':'%s'",
      refs$table, refs$column, refs$ref, refs$ref_col), collapse = "\n")
  } else {
    one <- refs[refs$ref_col_num == 1, , drop = FALSE]
    paste(sprintf("'%s'->'%s'", one$table, one$ref), collapse = "\n")
  }

  dot <- sprintf(
    "#duckdt_data_model\ndigraph {\ngraph [rankdir=%s tooltip=\"%s\" %s]\n\nnode [margin=0 fontcolor=\"#444444\" %s]\n\nedge [color=\"#555555\", arrowsize=1 %s]\n\n%s\n%s\n}",
    rankdir, graph_name, graph_attrs, node_attrs, edge_attrs,
    paste(nodes, collapse = "\n"), edges
  )
  structure(dot, class = c("duckdt_dot", "character"))
}

#' @export
print.duckdt_dot <- function(x, ...) {
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    print(DiagrammeR::grViz(unclass(x), allow_subst = FALSE), ...)
  } else {
    cli::cli_inform(c(
      "i" = "Install {.pkg DiagrammeR} to render this diagram, or use {.fn cat} to see the DOT source.",
      "i" = "{.fn duckdt_erd} draws the same model in a browser with no extra packages."
    ))
    cat(unclass(x), "\n")
  }
  invisible(x)
}

#' Render a data model with DiagrammeR/Graphviz
#'
#' Turns the model into an `htmlwidget` that draws in the RStudio viewer, an
#' Rmd/Quarto document or a Shiny app. Requires the `DiagrammeR` package;
#' [duckdt_erd()] needs nothing extra and is interactive, so prefer it for
#' exploring, and this for embedding.
#'
#' @param dm A `"duckdt_data_model"`, or anything [duckdt_data_model()]
#'   accepts.
#' @param width,height Optional widget size, in pixels.
#' @param ... Passed to [duckdt_dm_dot()], e.g. `view`, `rankdir`,
#'   `col_attr`.
#'
#' @return A `DiagrammeR` `grViz` htmlwidget.
#' @examples
#' \dontrun{
#' con <- duckdt_connect("my_database.duckdb")
#' duckdt_dm_render(con, view = "keys_only", rankdir = "LR")
#' }
#' @export
duckdt_dm_render <- function(dm, width = NULL, height = NULL, ...) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("duckdt: duckdt_dm_render() needs the DiagrammeR package. ",
      "Install it, or use duckdt_erd() which needs nothing extra.", call. = FALSE)
  }
  DiagrammeR::grViz(unclass(duckdt_dm_dot(dm, ...)), allow_subst = FALSE,
    width = width, height = height)
}

#' Export a data model diagram to an image file
#'
#' Writes the Graphviz rendering to SVG, PNG, PDF or PostScript. Needs the
#' `DiagrammeR`, `DiagrammeRsvg` and (for anything but SVG) `rsvg` packages.
#'
#' @param dm A `"duckdt_data_model"`, or anything [duckdt_data_model()]
#'   accepts.
#' @param file Output file. The extension picks the format unless `type` is
#'   given.
#' @param type One of `"svg"`, `"png"`, `"pdf"`, `"ps"`.
#' @param width,height Output size in pixels (not used for SVG).
#' @param ... Passed to [duckdt_dm_dot()].
#'
#' @return Invisibly, the path written.
#' @examples
#' \dontrun{
#' duckdt_dm_export(con, "schema.png", view = "keys_only")
#' }
#' @export
duckdt_dm_export <- function(dm, file, type = NULL, width = NULL, height = NULL, ...) {
  for (pkg in c("DiagrammeR", "DiagrammeRsvg")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("duckdt: duckdt_dm_export() needs the %s package.", pkg), call. = FALSE)
    }
  }
  if (is.null(type)) type <- tolower(tools::file_ext(file))
  if (!type %in% c("svg", "png", "pdf", "ps")) {
    stop("duckdt: `type` must be one of svg, png, pdf, ps.", call. = FALSE)
  }
  svg <- DiagrammeRsvg::export_svg(duckdt_dm_render(dm, ...))
  if (type == "svg") {
    writeLines(svg, file)
    return(invisible(file))
  }
  if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("duckdt: writing a ", type, " file needs the rsvg package (SVG works without it).",
      call. = FALSE)
  }
  writer <- switch(type, png = rsvg::rsvg_png, pdf = rsvg::rsvg_pdf, ps = rsvg::rsvg_ps)
  writer(charToRaw(svg), file = file, width = width, height = height)
  invisible(file)
}

# ---- colour scheme ---------------------------------------------------------

#' Colour schemes for data model diagrams
#'
#' Ported from datamodelr. A palette is four colours; a scheme is a named
#' list of palettes, and [duckdt_dm_set_display()] picks one per table by
#' name. The built-in scheme provides `"default"` plus `"accent1"` ...
#' `"accent7"` and border-less `"accent1nb"` ... `"accent7nb"`.
#'
#' @param line_color Border colour (`NULL` for no border).
#' @param header_bgcolor Table header background colour.
#' @param header_font Table header font colour.
#' @param bgcolor Table body background colour.
#' @param ... Named palettes, for `duckdt_dm_color_scheme()`.
#' @param color_scheme A scheme from `duckdt_dm_color_scheme()`.
#'
#' @return `duckdt_dm_palette()` a palette; `duckdt_dm_color_scheme()` a
#'   scheme; `duckdt_dm_get_color_scheme()` the scheme currently in use;
#'   `duckdt_dm_add_colors()` and `duckdt_dm_set_color_scheme()` invisibly
#'   the new scheme (they set the `duckdt.dm_scheme` option).
#' @examples
#' duckdt_dm_add_colors(duckdt_dm_color_scheme(
#'   fresh = duckdt_dm_palette(
#'     line_color = "#1a7f64", header_bgcolor = "#22a37f",
#'     header_font = "#FFFFFF", bgcolor = "#E6F5F0"
#'   )
#' ))
#' names(duckdt_dm_get_color_scheme())
#' @export
duckdt_dm_palette <- function(line_color = NULL, header_bgcolor, header_font, bgcolor) {
  list(
    line_color = line_color, header_bgcolor = header_bgcolor,
    header_font = header_font, bgcolor = bgcolor
  )
}

#' @rdname duckdt_dm_palette
#' @export
duckdt_dm_color_scheme <- function(...) list(...)

#' @rdname duckdt_dm_palette
#' @export
duckdt_dm_add_colors <- function(color_scheme) {
  old <- duckdt_dm_get_color_scheme()
  old[names(color_scheme)] <- NULL
  duckdt_dm_set_color_scheme(c(old, color_scheme))
}

#' @rdname duckdt_dm_palette
#' @export
duckdt_dm_get_color_scheme <- function() getOption("duckdt.dm_scheme")

#' @rdname duckdt_dm_palette
#' @export
duckdt_dm_set_color_scheme <- function(color_scheme) {
  options(duckdt.dm_scheme = color_scheme)
  invisible(color_scheme)
}

duckdt_dm_color <- function(palette_id, what) {
  scheme <- duckdt_dm_get_color_scheme()
  if (is.null(palette_id) || is.na(palette_id) || is.null(scheme[[palette_id]])) {
    palette_id <- "default"
  }
  scheme[[palette_id]][[what]]
}

duckdt_dm_default_scheme <- function() {
  accent <- function(line, header, bg) {
    duckdt_dm_palette(line, header_bgcolor = header, header_font = "#FFFFFF", bgcolor = bg)
  }
  base <- list(
    accent1 = c("#41719C", "#5B9BD5", "#D6E1F1"),
    accent2 = c("#AE5A21", "#ED7D31", "#F9DBD2"),
    accent3 = c("#BC8C00", "#FFC000", "#FFEAD0"),
    accent4 = c("#507E32", "#70AD47", "#D9E6D4"),
    accent5 = c("#2F528F", "#4472C4", "#D4D9EC"),
    accent6 = c("#787878", "#A5A5A5", "#E4E4E4"),
    accent7 = c("#000000", "#787878", "#D8D8D8")
  )
  scheme <- list(default = duckdt_dm_palette(
    line_color = "#555555", header_bgcolor = "#EFEBDD",
    header_font = "#000000", bgcolor = "#FFFFFF"
  ))
  for (nm in names(base)) {
    scheme[[nm]] <- accent(base[[nm]][1], base[[nm]][2], base[[nm]][3])
    scheme[[paste0(nm, "nb")]] <- accent(NULL, base[[nm]][2], base[[nm]][3])
  }
  scheme
}

# ---- DOT HTML labels -------------------------------------------------------

duckdt_html_tag <- function(x, tag, indent = 0, nl = TRUE, attrs = NULL, collapse = "") {
  if (length(x) > 1 && !is.null(collapse)) x <- paste(x, collapse = collapse)
  space <- strrep("  ", indent)
  attrs <- paste(sprintf('%s="%s"', names(attrs), unlist(attrs)), collapse = " ")
  if (nzchar(attrs)) attrs <- paste0(" ", attrs)
  out <- if (nl) {
    sprintf("%s<%s%s>\n%s%s</%s>\n", space, tag, attrs, x, space, tag)
  } else {
    sprintf("%s<%s%s>%s</%s>\n", space, tag, attrs, x, tag)
  }
  paste(out, collapse = "")
}

duckdt_dot_label <- function(x, title, palette_id = "default", col_attr = "column",
                             column_arrows = FALSE) {
  border <- if (is.null(duckdt_dm_color(palette_id, "line_color"))) 0 else 1
  attr_table <- list(ALIGN = "LEFT", BORDER = border, CELLBORDER = 0, CELLSPACING = 0)
  line_color <- duckdt_dm_color(palette_id, "line_color")
  if (!is.null(line_color)) attr_table$COLOR <- line_color

  cells <- if (column_arrows) col_attr else c("ref", col_attr)
  header <- duckdt_html_tag(
    duckdt_html_tag(
      duckdt_html_escape(title), tag = "FONT", nl = FALSE,
      attrs = list(COLOR = duckdt_dm_color(palette_id, "header_font"))
    ),
    tag = "TD", indent = 3, nl = FALSE, collapse = NULL,
    attrs = list(
      COLSPAN = length(cells),
      BGCOLOR = duckdt_dm_color(palette_id, "header_bgcolor"),
      BORDER = 0
    )
  )

  rows <- vapply(seq_len(nrow(x)), function(r) {
    tds <- vapply(cells, function(cell) {
      value <- if (cell == "ref") {
        if (is.na(x$ref[r])) "" else "~"
      } else {
        v <- x[[cell]][r]
        if (is.na(v)) "" else duckdt_html_escape(as.character(v))
      }
      if (cell == "column" && x$key[r] > 0) value <- sprintf("<U>%s</U>", value)
      td_attrs <- list(ALIGN = "LEFT", BGCOLOR = duckdt_dm_color(palette_id, "bgcolor"))
      if (cell == "column" && column_arrows && (x$key[r] > 0 || !is.na(x$ref[r]))) {
        td_attrs$PORT <- x$column[r]
      }
      duckdt_html_tag(value, tag = "TD", indent = 3, nl = FALSE, attrs = td_attrs)
    }, character(1))
    duckdt_html_tag(tds, tag = "TR", indent = 2)
  }, character(1))

  table <- duckdt_html_tag(
    c(duckdt_html_tag(header, tag = "TR", indent = 2), rows),
    tag = "TABLE", indent = 1, attrs = attr_table
  )
  sprintf("<%s>", trimws(table))
}

# Graphviz HTML-like labels are parsed as XML, so table/column names carrying
# an & or < would otherwise produce an unparseable graph.
duckdt_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# ---- shared helpers --------------------------------------------------------

duckdt_as_data_model <- function(dm) {
  if (is_duckdt_data_model(dm)) dm else duckdt_data_model(dm)
}

# Columns to draw for a given view type.
duckdt_dm_visible_columns <- function(dm, view = "all") {
  cols <- as.data.frame(dm$columns)
  switch(view,
    all = cols,
    keys_only = cols[cols$key > 0 | !is.na(cols$ref), , drop = FALSE],
    title_only = cols[0L, , drop = FALSE],
    stop("duckdt: unknown view type '", view, "'.", call. = FALSE)
  )
}

# Tables tagged `display = "hide"` are left out of every rendering.
duckdt_dm_hidden <- function(display) !is.na(display) & display == "hide"
