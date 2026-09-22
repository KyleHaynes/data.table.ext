# Shared by every page: each .qmd opens with a hidden chunk that sources this.

options(
  width = 84,
  cli.num_colors = 256, # let cli and the print mask emit colour; converted to HTML below
  cli.unicode = TRUE,
  duckdt.quiet = TRUE   # no startup banner from the hidden library() call
)

suppressPackageStartupMessages({
  library(data.table)
  library(data.table.ext)
})
set.seed(2024)

# duckdb announces where it stores extensions (naming your home directory)
# every time a database opens, until a storage choice has been made. Make one
# up front, with a throwaway home, so the note stays out of the rendered pages.
suppressMessages(
  DBI::dbDisconnect(DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)), shutdown = TRUE)
)

# Chunk output carrying ANSI colour escapes (the print mask, cli messages)
# becomes coloured HTML instead of raw escape characters.
local({
  ansi_hook <- function(name, css_class) {
    base <- knitr::knit_hooks$get(name)
    hooks <- list()
    hooks[[name]] <- function(x, options) {
      if (!any(grepl("\033[", x, fixed = TRUE))) return(base(x, options))
      html <- sub("\n+$", "", cli::ansi_html(paste(x, collapse = "\n")))
      paste0(
        "\n::: {.cell-output ", css_class, "}\n```{=html}\n",
        "<pre class=\"ansi-output\"><code>", html, "</code></pre>\n```\n:::\n"
      )
    }
    knitr::knit_hooks$set(hooks)
  }
  ansi_hook("output", ".cell-output-stdout")
  ansi_hook("message", ".cell-output-stderr")
  ansi_hook("warning", ".cell-output-stderr")
  ansi_hook("error", ".cell-output-stderr")
})
