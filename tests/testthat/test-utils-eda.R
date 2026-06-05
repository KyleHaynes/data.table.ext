library(data.table)

dt <- data.table(
    a = c(1, 2, NA, 4),
    b = c("x", "y", "x", NA),
    c = c(TRUE, FALSE, TRUE, TRUE)
)

# ── na_dt ──────────────────────────────────────────────────────────────────────

test_that("na_dt returns correct counts and percentages", {
    out <- na_dt(dt)
    expect_s3_class(out, "data.table")
    expect_equal(out$col, c("a", "b", "c"))
    expect_equal(out$n_na, c(1L, 1L, 0L))
    expect_equal(out$pct_na, c(0.25, 0.25, 0.0))
})

test_that("na_dt returns zero pct_na for empty table", {
    empty <- data.table(x = integer(0))
    out <- na_dt(empty)
    expect_true(is.na(out$pct_na))
})

test_that("na_dt errors on non-data.table", {
    expect_error(na_dt(data.frame(x = 1)), "'dt' must be a data.table")
})

# ── freq_dt ────────────────────────────────────────────────────────────────────

test_that("freq_dt returns sorted frequency table", {
    out <- freq_dt(dt, b)
    expect_s3_class(out, "data.table")
    expect_true("n" %in% names(out))
    expect_true("pct" %in% names(out))
    expect_equal(out$b[1], "x")
    expect_equal(out$n[1], 2L)
})

test_that("freq_dt accepts character column name", {
    out <- freq_dt(dt, "b")
    expect_equal(nrow(out), 3L)
})

test_that("freq_dt respects n limit", {
    big <- data.table(v = letters)
    out <- freq_dt(big, v, n = 5)
    expect_equal(nrow(out), 5L)
})

test_that("freq_dt errors on missing column", {
    expect_error(freq_dt(dt, "zzz"), "not found")
})

# ── schema_dt ──────────────────────────────────────────────────────────────────

test_that("schema_dt returns one row per column", {
    out <- schema_dt(dt)
    expect_s3_class(out, "data.table")
    expect_equal(nrow(out), ncol(dt))
    expect_equal(out$col, names(dt))
    expect_true(all(c("col", "class", "n_distinct", "n_na") %in% names(out)))
})

test_that("schema_dt n_na matches na_dt", {
    expect_equal(schema_dt(dt)$n_na, na_dt(dt)$n_na)
})

test_that("schema_dt errors on non-data.table", {
    expect_error(schema_dt(list(x = 1)), "'dt' must be a data.table")
})

# ── rename_dt ──────────────────────────────────────────────────────────────────

test_that("rename_dt renames columns by reference", {
    d <- data.table(old_name = 1:3, value = 4:6)
    rename_dt(d, c(new_name = "old_name", v2 = "value"))
    expect_equal(names(d), c("new_name", "v2"))
})

test_that("rename_dt errors on missing source column", {
    d <- data.table(x = 1)
    expect_error(rename_dt(d, c(y = "z")), "not found")
})

test_that("rename_dt errors on unnamed renames vector", {
    d <- data.table(x = 1)
    expect_error(rename_dt(d, c("x")), "named character vector")
})

test_that("rename_dt errors on non-data.table", {
    expect_error(rename_dt(data.frame(x = 1), c(y = "x")), "'dt' must be a data.table")
})
