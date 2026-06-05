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
