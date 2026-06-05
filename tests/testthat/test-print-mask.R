library(data.table)

# ── enable / disable roundtrip ─────────────────────────────────────────────────

test_that("enable installs mask and disable removes it", {
    disable_dt_print_thousands()
    expect_false(exists("print.data.table", envir = .GlobalEnv, inherits = FALSE))

    enable_dt_print_thousands(color = FALSE)
    expect_true(exists("print.data.table", envir = .GlobalEnv, inherits = FALSE))

    disable_dt_print_thousands()
    expect_false(exists("print.data.table", envir = .GlobalEnv, inherits = FALSE))
})

# ── row index formatting ───────────────────────────────────────────────────────

test_that(".align_dt_row_indices adds thousands separator", {
    lines <- c("   1: foo", "1000: bar")
    out <- data.table.ext:::.align_dt_row_indices(lines, big.mark = ",")
    expect_true(grepl("1,000", out[2]))
    expect_true(grepl("^\\s*1:", out[1]))
})

test_that(".format_dt_row_index detects non-row lines", {
    p <- data.table.ext:::.format_dt_row_index("   col1  col2  col3")
    expect_false(isTRUE(p$is_row))
})

test_that(".colorize_dt_class_rows ignores unknown class tokens", {
    skip_if_not_installed("cli")

    lines <- "      <char> <POSct> <mystery>"
    expect_no_error({
        out <- data.table.ext:::.colorize_dt_class_rows(
            lines,
            class_colors = data.table.ext:::.default_dt_class_colors()
        )
    })
    expect_length(out, 1L)
    expect_match(out, "<mystery>", fixed = TRUE)
})

test_that(".colorize_group_value_rows skips wrapped continuation lines", {
    lines <- c(
        "1: alpha 100",
        "1: GAQLD123 <NA>",
        "2: beta 200",
        "2: GAQLD456 <NA>"
    )
    x <- data.table(
        grp = c("g1", "g2"),
        left = c("alpha", "beta"),
        right = c("100", "200")
    )
    similarity_maps <- list(
        g1 = list(
            left = list(values = "alpha", colors = 22),
            right = list(values = "100", colors = 22)
        ),
        g2 = list(
            left = list(values = "beta", colors = 23),
            right = list(values = "200", colors = 23)
        )
    )

    out <- data.table.ext:::.colorize_group_value_rows(lines, x, "grp", similarity_maps)
    expect_true(grepl("\033[38;5;22malpha\033[39m", out[1], fixed = TRUE))
    expect_true(grepl("\033[38;5;23mbeta\033[39m", out[3], fixed = TRUE))
    expect_false(grepl("\\033\\[38;5;", out[2]))
    expect_false(grepl("\\033\\[38;5;", out[4]))
})

# ── group separators ───────────────────────────────────────────────────────────

test_that(".insert_group_separators inserts separator at group boundaries", {
    dt <- data.table(grp = c("a", "a", "b"), val = 1:3)
    lines <- c("   1: a 1", "   2: a 2", "   3: b 3")
    out <- data.table.ext:::.insert_group_separators(lines, dt, "grp")
    sep_lines <- grep("Group:", out, value = TRUE)
    expect_equal(length(sep_lines), 2L)
})

test_that(".insert_group_separators does not repeat separators for wrapped rows", {
    dt <- data.table(grp = c("a"), left = "x", right = "y")
    lines <- c(
        "   left",
        " <char>",
        "1: x",
        "   right",
        "  <char>",
        "1: y"
    )
    out <- data.table.ext:::.insert_group_separators(lines, dt, "grp")
    sep_lines <- grep("Group:", out, value = TRUE)
    expect_equal(length(sep_lines), 1L)
})

test_that(".insert_group_separators does not repeat separators for wrapped multi-row blocks", {
    dt <- data.table(grp = c("a", "b"), left = c("x1", "x2"), right = c("y1", "y2"))
    lines <- c(
        "   left",
        " <char>",
        "1: x1",
        "2: x2",
        "   right",
        "  <char>",
        "1: y1",
        "2: y2"
    )
    out <- data.table.ext:::.insert_group_separators(lines, dt, "grp")
    sep_lines <- grep("Group:", out, value = TRUE)
    expect_equal(length(sep_lines), 2L)
})

test_that(".insert_group_separators respects custom sep_fmt", {
    dt <- data.table(grp = c("x"), val = 1L)
    lines <- c("1: x 1")
    out <- data.table.ext:::.insert_group_separators(lines, dt, "grp", sep_fmt = "=== %s ===")
    expect_true(any(grepl("=== x ===", out)))
})

# ── similarity clusters ────────────────────────────────────────────────────────

test_that(".similarity_clusters returns single cluster for empty input", {
    expect_equal(data.table.ext:::.similarity_clusters(character(0L)), integer(0L))
})

test_that(".similarity_clusters assigns same cluster to identical values", {
    ids <- data.table.ext:::.similarity_clusters(c("cat", "cat", "dog"))
    expect_equal(ids[1], ids[2])
    expect_false(ids[1] == ids[3])
})

test_that(".similarity_clusters merges values within edit distance", {
    ids <- data.table.ext:::.similarity_clusters(c("colour", "color"), max_distance = 2L)
    expect_equal(ids[1], ids[2])
})

test_that(".similarity_clusters keeps distant values separate", {
    ids <- data.table.ext:::.similarity_clusters(c("apple", "zebra"), max_distance = 1L, max_relative = 0.1)
    expect_false(ids[1] == ids[2])
})
