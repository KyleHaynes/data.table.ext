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

    if (requireNamespace("cli", quietly = TRUE)) {
        example_code <- c(
            "DT <- data.table::as.data.table(iris)",
            "sample_dt(DT, n = 2, group = \"Species\")",
            "str(DT)",
            "dput(DT[1:2])",
            "# Demo thousand comma separation:",
            "data.table(x = 1:1E6)"
        )

        cli::cli_h1("data.table.ext")
        cli::cli_alert_success("Auto-enabled: print mask, str() mask, dput() mask, and default sample coloring.")
        cli::cli_alert_info("What this does: cleaner data.table printing, smarter grouped sampling, friendlier str()/dput().")
        cli::cli_text("Run these iris examples for a demo:")
        cli::cli_code(example_code, language = "R")

        # cli collapses runs of spaces, so these read as sentences rather than a
        # column-aligned table.
        msg <- cli::format_message(c(
            "Also included: {.pkg duckdt} -- query DuckDB with data.table syntax.",
            "*" = "Open a database with {.code con <- duckdt_connect(\"my_data.duckdb\")}",
            "*" = "See what is in it with {.code duckdt_erd(con)}",
            "*" = "Query a table with {.code d <- duckdt(con, \"my_table\")}, then {.code d[x > 1, .N, by = y]}",
            "*" = "Or start from an R data.frame: {.code d <- as.duckdt(mtcars)}",
            "*" = "New here? {.code con <- duckdt_example()} builds a small database to try on.",
            " " = "Silence this with {.code options(duckdt.quiet = TRUE)}."
        ))
        packageStartupMessage(paste(msg, collapse = "\n"))
    } else {
        packageStartupMessage(
            paste(
                "data.table.ext: auto-enabled print mask, str() mask, dput() mask, and sample coloring.",
                "Try with iris:",
                "DT <- data.table::as.data.table(iris); sample_dt(DT, n = 2, group = \"Species\"); str(DT); dput(DT[1:2])",
                "Also included: duckdt -- query DuckDB with data.table syntax. See ?duckdt_connect to get started."
            )
        )
    }
}
