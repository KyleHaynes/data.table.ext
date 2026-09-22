# The exported functions, grouped by what they are for. This is the single
# place that grouping lives: the startup banner (zzz.R) is drawn from it, and
# tests/testthat/test-function-index.R fails when an export is missing from it,
# so the banner cannot quietly fall behind the package again.
#
# Per theme, `fns` is the complete set of exports (checked against NAMESPACE)
# and `show` is what the banner prints for it: literal names, `prefix_*`
# shorthand standing in for several of `fns`, or a non-function token such as
# `d[i, j, by]`.
ext_function_index <- function() {
  index <- list(
    list(
      family = "Display, sampling & readability",
      themes = list(
        list(
          theme = "Session",
          fns = c(
            "turn_everyone_on", "switch_col",
            "enable_dt_print_thousands", "disable_dt_print_thousands",
            "enable_dt_str_mask", "disable_dt_str_mask",
            "enable_dt_dput_mask", "disable_dt_dput_mask"
          ),
          show = c("turn_everyone_on()", "switch_col()", "enable_dt_*()", "disable_dt_*()")
        ),
        list(
          theme = "Sample",
          fns = c("sample_dt", "cdt"),
          show = c("sample_dt()", "cdt()")
        ),
        list(
          theme = "Spot",
          fns = c("highlight_dt", "dupe_dt", "outlier_dt"),
          show = c("highlight_dt()", "dupe_dt()", "outlier_dt()")
        ),
        list(
          theme = "Profile",
          fns = c("schema_dt", "na_dt", "freq_dt", "spark_dt", "key_dt"),
          show = c("schema_dt()", "na_dt()", "freq_dt()", "spark_dt()", "key_dt()")
        ),
        list(
          theme = "Columns",
          fns = c("e", "rename_dt", "set_null"),
          show = c("e()", "rename_dt()", "set_null()")
        ),
        list(
          theme = "Share",
          fns = c("md_dt", "copy_dt"),
          show = c("md_dt()", "copy_dt()")
        )
      )
    ),
    list(
      family = "dbdt: data.table syntax on DuckDB and SQL Server",
      themes = list(
        list(
          theme = "Connect",
          fns = c("duckdt_connect", "duckdt_disconnect", "duckdt_example"),
          show = c("duckdt_connect()", "duckdt_disconnect()", "duckdt_example()")
        ),
        list(
          theme = "Load",
          fns = c("as.duckdt", "duckdt", "duckdt_csv", "duckdt_parquet"),
          show = c("as.duckdt()", "duckdt()", "duckdt_csv()", "duckdt_parquet()")
        ),
        list(
          theme = "Query",
          fns = "duckdt_sample",
          show = c("d[i, j, by]", "head()", "tail()", "duckdt_sample()")
        ),
        list(
          theme = "Join/write",
          fns = c("duckdt_join", "duckdt_merge", "duckdt_temp", "duckdt_drop"),
          show = c("duckdt_join()", "duckdt_merge()", "duckdt_temp()", "duckdt_drop()", ":=")
        ),
        list(
          theme = "Explore",
          fns = c(
            "duckdt_tables", "duckdt_schema", "duckdt_relationships",
            "duckdt_erd", "duckdt_explorer"
          ),
          show = c(
            "duckdt_tables()", "duckdt_schema()", "duckdt_relationships()",
            "duckdt_erd()", "duckdt_explorer()"
          )
        ),
        list(
          theme = "Model",
          fns = c(
            "duckdt_data_model", "is_duckdt_data_model",
            "duckdt_dm_set_key", "duckdt_dm_add_references", "duckdt_dm_add_reference",
            "duckdt_dm_infer_references", "duckdt_dm_set_segment", "duckdt_dm_set_display",
            "duckdt_dm_filter", "duckdt_dm_query"
          ),
          show = c(
            "duckdt_data_model()", "duckdt_dm_infer_references()",
            "duckdt_dm_query()", "duckdt_dm_*()"
          )
        ),
        list(
          theme = "Draw",
          fns = c(
            "duckdt_dm_mermaid", "duckdt_dm_dot", "duckdt_dm_render", "duckdt_dm_export",
            "duckdt_dm_palette", "duckdt_dm_color_scheme", "duckdt_dm_add_colors",
            "duckdt_dm_get_color_scheme", "duckdt_dm_set_color_scheme"
          ),
          show = c(
            "duckdt_dm_mermaid()", "duckdt_dm_dot()", "duckdt_dm_render()",
            "duckdt_dm_export()", "duckdt_dm_palette()"
          )
        )
      )
    )
  )
  index[[2L]]$themes <- lapply(index[[2L]]$themes, function(theme) {
    theme$fns <- c(gsub("duckdt", "dbdt", theme$fns, fixed = TRUE), theme$fns)
    theme$show <- gsub("duckdt", "dbdt", theme$show, fixed = TRUE)
    theme
  })
  index
}

# The startup banner as a character vector of lines: the index above drawn as
# a tree (family -> theme -> functions), wrapped to `width`. Pure -- it only
# builds text -- so .onAttach() decides whether and how to show it.
ext_banner_lines <- function(version, width = 80L, unicode = TRUE,
                             index = ext_function_index()) {
  g <- if (unicode) {
    list(rule = intToUtf8(0x2500), tick = intToUtf8(0x2714),
         branch = intToUtf8(c(0x251c, 0x2500, 0x20)), last = intToUtf8(c(0x2514, 0x2500, 0x20)),
         bar = intToUtf8(c(0x2502, 0x20, 0x20)), dot = intToUtf8(0xb7))
  } else {
    list(rule = "-", tick = "v", branch = "|- ", last = "`- ", bar = "|  ", dot = "-")
  }
  bold <- cli::style_bold
  faint <- cli::col_grey
  code <- cli::col_cyan

  title <- paste("data.table.ext", version)
  header <- paste0(
    faint(strrep(g$rule, 2L)), " ", bold(title), " ",
    faint(strrep(g$rule, max(3L, width - nchar(title) - 4L)))
  )

  labels <- unlist(lapply(index, function(f) vapply(f$themes, `[[`, "", "theme")))
  label_w <- max(nchar(labels))
  indent_w <- nchar(g$branch)
  room <- max(20L, width - indent_w - label_w - 2L)

  tree <- function(family) {
    n <- length(family$themes)
    out <- bold(family$family)
    for (i in seq_len(n)) {
      theme <- family$themes[[i]]
      elbow <- if (i == n) g$last else g$branch
      cont <- if (i == n) strrep(" ", indent_w) else g$bar
      rows <- ext_wrap_tokens(theme$show, room)
      label <- formatC(theme$theme, width = -label_w)
      for (r in seq_along(rows)) {
        lead <- if (r == 1L) {
          paste0(faint(elbow), bold(label))
        } else {
          paste0(faint(cont), strrep(" ", label_w))
        }
        out <- c(out, paste0(lead, "  ", paste(code(rows[[r]]), collapse = "  ")))
      }
    }
    out
  }

  c(
    header,
    paste(cli::col_green(g$tick),
      "Masks on: data.table print (thousands, colour), str() and dput()."),
    paste0("  ", faint(paste0(
      "Undo with disable_dt_*()  ", g$dot, "  hide this with options(duckdt.quiet = TRUE)"
    ))),
    "",
    tree(index[[1L]]),
    "",
    tree(index[[2L]]),
    "",
    paste0(bold("Try  "), code("DT <- data.table::as.data.table(iris)")),
    paste0("     ", code("sample_dt(DT, n = 2, group = Species)")),
    paste0("     ", code("con <- dbdt_example(); dbdt_erd(con)")),
    paste0(bold("All  "), code("help(package = \"data.table.ext\")"))
  )
}

# Greedy word-wrap of `tokens` into rows no wider than `room` (two spaces
# between tokens), so a long theme wraps under its own label.
ext_wrap_tokens <- function(tokens, room) {
  rows <- list()
  current <- character()
  used <- 0L
  for (tok in tokens) {
    w <- nchar(tok) + if (length(current)) 2L else 0L
    if (length(current) && used + w > room) {
      rows[[length(rows) + 1L]] <- current
      current <- character()
      used <- 0L
      w <- nchar(tok)
    }
    current <- c(current, tok)
    used <- used + w
  }
  rows[[length(rows) + 1L]] <- current
  rows
}
