#' @keywords internal
"_PACKAGE"

# Session-scoped flags (e.g. "have we already hinted about duckdt_erd()").
duckdt_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("duckdt.dm_scheme"))) {
    duckdt_dm_set_color_scheme(duckdt_dm_default_scheme())
  }
  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (isTRUE(getOption("duckdt.quiet"))) return(invisible())
  # cli collapses runs of spaces, so these read as sentences rather than a
  # column-aligned table.
  msg <- cli::format_message(c(
    "{.pkg duckdt} {utils::packageVersion('duckdt')} -- DuckDB with data.table syntax.",
    "*" = "Open a database with {.code con <- duckdt_connect(\"my_data.duckdb\")}",
    "*" = "See what is in it with {.code duckdt_erd(con)}",
    "*" = "Query a table with {.code d <- duckdt(con, \"my_table\")}, then {.code d[x > 1, .N, by = y]}",
    "*" = "Or start from an R data.frame: {.code d <- as.duckdt(mtcars)}",
    "*" = "New here? {.code con <- duckdt_example()} builds a small database to try on.",
    " " = "Silence this with {.code options(duckdt.quiet = TRUE)}."
  ))
  packageStartupMessage(paste(msg, collapse = "\n"))
}
