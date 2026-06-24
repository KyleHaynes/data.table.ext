.onAttach <- function(libname, pkgname) {
    turn_everyone_on()

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
    } else {
        packageStartupMessage(
            paste(
                "data.table.ext: auto-enabled print mask, str() mask, dput() mask, and sample coloring.",
                "Try with iris:",
                "DT <- data.table::as.data.table(iris); sample_dt(DT, n = 2, group = \"Species\"); str(DT); dput(DT[1:2])"
            )
        )
    }
}

