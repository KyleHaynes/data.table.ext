namespace_exports <- function() {
    ns_file <- system.file("NAMESPACE", package = "data.table.ext")
    lines <- readLines(ns_file, warn = FALSE)
    exports <- grep("^export\\(", lines, value = TRUE)
    gsub("\"", "", sub("^export\\((.*)\\)$", "\\1", exports))
}

index_functions <- function(index) {
    unlist(lapply(index, function(f) lapply(f$themes, `[[`, "fns")), use.names = FALSE)
}

test_that("the function index lists every export exactly once", {
    fns <- index_functions(ext_function_index())

    expect_false(anyDuplicated(fns) > 0)
    expect_setequal(fns, namespace_exports())
})

test_that("every banner token names a real function or a known shorthand", {
    # S3 methods on "duckdt" that the banner shows but the package does not export.
    methods_shown <- c("head", "tail")

    for (family in ext_function_index()) {
        for (theme in family$themes) {
            for (tok in theme$show) {
                name <- sub("\\(\\)$", "", tok)
                info <- sprintf("%s / %s", theme$theme, tok)
                if (grepl("*", name, fixed = TRUE)) {
                    expect_true(any(grepl(utils::glob2rx(name), theme$fns)), info = info)
                } else if (grepl("\\(\\)$", tok)) {
                    expect_true(name %in% c(theme$fns, methods_shown), info = info)
                }
            }
        }
    }
})

test_that("the banner draws every family and theme, within the requested width", {
    index <- ext_function_index()
    lines <- cli::ansi_strip(ext_banner_lines("9.9.9", width = 80L, unicode = TRUE, index = index))
    text <- paste(lines, collapse = "\n")

    expect_match(text, "data.table.ext 9.9.9", fixed = TRUE)
    for (family in index) {
        expect_match(text, family$family, fixed = TRUE)
        for (theme in family$themes) {
            expect_match(text, theme$theme, fixed = TRUE)
            for (tok in theme$show) expect_match(text, tok, fixed = TRUE)
        }
    }
    expect_lte(max(cli::ansi_nchar(lines, type = "width")), 80L)
})

test_that("the tree wraps long themes under their own label on a narrow console", {
    lines <- cli::ansi_strip(ext_banner_lines("9.9.9", width = 60L, unicode = TRUE))
    tree_lines <- grep(paste0("^[", intToUtf8(c(0x251c, 0x2514, 0x2502)), "]"), lines, value = TRUE)

    expect_gt(length(tree_lines), 0L)
    expect_lte(max(cli::ansi_nchar(tree_lines, type = "width")), 60L)
})

test_that("the banner falls back to plain ASCII", {
    lines <- cli::ansi_strip(ext_banner_lines("9.9.9", width = 80L, unicode = FALSE))
    expect_true(all(grepl("^[ -~]*$", lines)))
})

test_that("wrapping keeps a token whole and starts a new row when the line is full", {
    rows <- ext_wrap_tokens(c("aaaa()", "bbbb()", "cccc()"), room = 14L)
    expect_equal(rows, list(c("aaaa()", "bbbb()"), "cccc()"))
    expect_equal(ext_wrap_tokens("a_very_long_token()", room = 5L), list("a_very_long_token()"))
})

test_that("attaching prints the banner as one suppressible startup message", {
    on_attach <- get(".onAttach", envir = asNamespace("data.table.ext"))
    had <- vapply(c("print.data.table", "str", "dput"), exists, logical(1L),
        envir = .GlobalEnv, inherits = FALSE)
    old <- options(duckdt.quiet = NULL)
    on.exit({
        options(old)
        if (!had[["print.data.table"]]) disable_dt_print_thousands()
        if (!had[["str"]]) disable_dt_str_mask()
        if (!had[["dput"]]) disable_dt_dput_mask()
    }, add = TRUE)

    msgs <- character()
    withCallingHandlers(
        on_attach("", "data.table.ext"),
        packageStartupMessage = function(m) {
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
        }
    )
    expect_length(msgs, 1L)
    expect_match(msgs, "Display, sampling & readability", fixed = TRUE)

    expect_silent(suppressPackageStartupMessages(on_attach("", "data.table.ext")))

    options(duckdt.quiet = TRUE)
    expect_silent(on_attach("", "data.table.ext"))
})
