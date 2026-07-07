library(data.table)

dt <- data.table(
    grp = rep(c("a", "b", "c"), each = 4),
    val = 1:12,
    lab = rep(letters[1:4], 3)
)

# ── sample_dt ──────────────────────────────────────────────────────────────────

test_that("sample_dt returns n rows when no group", {
    set.seed(1)
    out <- sample_dt(dt, n = 5, color = FALSE)
    expect_s3_class(out, "data.table")
    expect_equal(nrow(out), 5L)
})

test_that("sample_dt clamps n to nrow", {
    out <- sample_dt(dt, n = 999, color = FALSE)
    expect_equal(nrow(out), nrow(dt))
})

test_that("sample_dt returns empty table for empty input", {
    empty <- data.table(x = integer(0), g = character(0))
    out <- sample_dt(empty, color = FALSE)
    expect_equal(nrow(out), 0L)
})

test_that("sample_dt group sampling returns all rows for selected groups", {
    set.seed(1)
    out <- sample_dt(dt, n = 2, group = grp, color = FALSE)
    expect_s3_class(out, "data.table")
    expect_lte(data.table::uniqueN(out$grp), 2L)
    for (g in unique(out$grp)) {
        expect_equal(nrow(out[grp == g]), nrow(dt[grp == g]))
    }
})

test_that("sample_dt accepts character group name", {
    set.seed(1)
    out <- sample_dt(dt, n = 1, group = "grp", color = FALSE)
    expect_equal(data.table::uniqueN(out$grp), 1L)
})

test_that("sample_dt errors on non-data.table", {
    expect_error(sample_dt(data.frame(x = 1)), "'dt' must be a data.table")
})

test_that("sample_dt errors on invalid n", {
    expect_error(sample_dt(dt, n = 0, color = FALSE), "'n' must be a positive integer")
    expect_error(sample_dt(dt, n = -1, color = FALSE), "'n' must be a positive integer")
})

test_that("sample_dt errors on missing group column", {
    expect_error(sample_dt(dt, group = "zzz", color = FALSE), "not found")
})

test_that("sample_dt color_threshold disables color on large groups", {
    out <- sample_dt(dt, n = 3, group = grp, color = TRUE, color_threshold = 2L)
    expect_true(isTRUE(attr(out, ".group_print_disable_color")))
})

# ── set_null ───────────────────────────────────────────────────────────────────

test_that("set_null removes columns by reference", {
    d <- data.table(x = 1, y = 2, z = 3)
    set_null(d, c("y", "z"))
    expect_equal(names(d), "x")
})

test_that("set_null errors on non-data.table", {
    expect_error(set_null(data.frame(x = 1), "x"), "'x' must be a data.table")
})

test_that("set_null errors on non-character cols", {
    d <- data.table(x = 1)
    expect_error(set_null(d, 1L), "'cols' must be a character vector")
})

# ── switch_col ─────────────────────────────────────────────────────────────────

test_that("switch_col sets and retrieves option", {
    switch_col(TRUE)
    expect_true(getOption("foam.sample_dt.color"))
    switch_col(FALSE)
    expect_false(getOption("foam.sample_dt.color"))
    switch_col(TRUE)
})

test_that("switch_col errors on non-logical", {
    expect_error(switch_col("yes"), "'on' must be TRUE or FALSE")
})

# ── highlight_dt ───────────────────────────────────────────────────────────────

test_that("highlight_dt tags matching rows", {
    out <- highlight_dt(dt, val > 10)
    expect_equal(attr(out, ".highlight_print_rows"), which(dt$val > 10))
    expect_equal(attr(out, ".highlight_print_color"), "col_red")
})

test_that("highlight_dt accepts a pre-computed logical vector", {
    cond <- dt$val > 10
    out <- highlight_dt(dt, cond)
    expect_equal(attr(out, ".highlight_print_rows"), which(cond))
})

test_that("highlight_dt respects custom color", {
    out <- highlight_dt(dt, val > 10, color = "col_yellow")
    expect_equal(attr(out, ".highlight_print_color"), "col_yellow")
})

test_that("highlight_dt errors on non-data.table", {
    expect_error(highlight_dt(data.frame(x = 1), x > 0), "'dt' must be a data.table")
})

test_that("highlight_dt errors on wrong-length condition", {
    expect_error(highlight_dt(dt, c(TRUE, FALSE)), "same length")
})

test_that("highlight_dt colors matching rows when printed", {
    skip_if_not_installed("cli")
    d <- data.table(x = 1:3)
    out <- highlight_dt(d, x == 2)
    printed <- capture.output(print(out))
    expect_true(any(grepl("\033\\[3[0-9]m", printed)))
})

# ── dupe_dt ────────────────────────────────────────────────────────────────────

test_that("dupe_dt returns only rows that participate in a duplicate", {
    d <- data.table(a = c(1, 1, 2, 3, 3, 3), b = c("x", "x", "y", "z", "z", "z"))
    out <- dupe_dt(d, color = FALSE)
    expect_equal(nrow(out), 5L)
    expect_true(all(c(".dupe_group") %in% names(out)))
    expect_equal(data.table::uniqueN(out$.dupe_group), 2L)
})

test_that("dupe_dt returns zero rows when there are no duplicates", {
    d <- data.table(a = 1:3, b = c("x", "y", "z"))
    out <- dupe_dt(d, color = FALSE)
    expect_equal(nrow(out), 0L)
})

test_that("dupe_dt respects a 'by' subset of columns", {
    d <- data.table(a = c(1, 1, 2), b = c("x", "y", "z"))
    out <- dupe_dt(d, by = "a", color = FALSE)
    expect_equal(nrow(out), 2L)
})

test_that("dupe_dt errors on non-data.table", {
    expect_error(dupe_dt(data.frame(x = 1)), "'dt' must be a data.table")
})

test_that("dupe_dt errors on missing by column", {
    d <- data.table(a = 1)
    expect_error(dupe_dt(d, by = "zzz"), "not found")
})
