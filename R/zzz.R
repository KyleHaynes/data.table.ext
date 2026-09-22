#' @keywords internal
"_PACKAGE"

# Columns referenced via data.table's NSE (`:=`, `by =`) inside package
# functions, not actual free variables.
utils::globalVariables(c("N", "pct", ".dupe_group", ".dupe_n", "outlier_cols", "n_cols"))

# Session-scoped flags (e.g. "have we already hinted about duckdt_erd()").
duckdt_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
    if (is.null(getOption("duckdt.dm_scheme"))) {
        duckdt_dm_set_color_scheme(duckdt_dm_default_scheme())
    }
    invisible()
}

.onAttach <- function(libname, pkgname) {
    turn_everyone_on()

    if (isTRUE(getOption("duckdt.quiet"))) return(invisible())

    # One packageStartupMessage() so suppressPackageStartupMessages() silences
    # all of it (cli's own cli_*() output is a different kind of message).
    packageStartupMessage(paste(
        ext_banner_lines(
            version = as.character(utils::packageVersion(pkgname)),
            width = min(cli::console_width(), 100L),
            unicode = cli::is_utf8_output()
        ),
        collapse = "\n"
    ))
}
