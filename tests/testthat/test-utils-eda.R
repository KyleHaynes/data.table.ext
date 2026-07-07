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

# ── spark_dt ───────────────────────────────────────────────────────────────────

test_that("spark_dt returns a sparkline with min/median/max", {
    d <- data.table(v = 1:100)
    out <- capture.output(txt <- spark_dt(d, v))
    expect_match(out, "min 1")
    expect_match(out, "max 100")
    expect_true(nzchar(txt))
})

test_that("spark_dt accepts a character column name", {
    d <- data.table(v = c(1, 2, 3))
    expect_no_error(capture.output(spark_dt(d, "v")))
})

test_that("spark_dt handles all-NA columns", {
    d <- data.table(v = c(NA_real_, NA_real_))
    out <- capture.output(txt <- spark_dt(d, v))
    expect_match(out, "no non-NA values")
})

test_that("spark_dt errors on non-numeric column", {
    d <- data.table(v = c("a", "b"))
    expect_error(spark_dt(d, v), "must be numeric")
})

test_that("spark_dt errors on missing column", {
    d <- data.table(v = 1)
    expect_error(spark_dt(d, "zzz"), "not found")
})

test_that("spark_dt errors on non-data.table", {
    expect_error(spark_dt(data.frame(x = 1), x), "'dt' must be a data.table")
})

# ── outlier_dt ─────────────────────────────────────────────────────────────────

test_that("outlier_dt flags iqr outliers", {
    d <- data.table(v = c(rep(10, 20), 1000))
    out <- outlier_dt(d)
    expect_equal(nrow(out), 1L)
    expect_equal(out$v, 1000)
    expect_equal(out$outlier_cols, "v")
})

test_that("outlier_dt flags zscore outliers", {
    set.seed(1)
    d <- data.table(v = c(rnorm(100), 100))
    out <- outlier_dt(d, method = "zscore")
    expect_true(100 %in% out$v)
})

test_that("outlier_dt returns zero rows when nothing is flagged", {
    d <- data.table(v = rep(10, 10))
    out <- outlier_dt(d)
    expect_equal(nrow(out), 0L)
    expect_true("outlier_cols" %in% names(out))
})

test_that("outlier_dt restricts to requested numeric cols", {
    d <- data.table(v = c(rep(10, 20), 1000), label = "x")
    expect_error(outlier_dt(d, cols = "label"), "not numeric")
    expect_error(outlier_dt(d, cols = "zzz"), "not found")
})

test_that("outlier_dt errors on non-data.table", {
    expect_error(outlier_dt(data.frame(x = 1)), "'dt' must be a data.table")
})

# ── key_dt ─────────────────────────────────────────────────────────────────────

test_that("key_dt finds a single-column key", {
    d <- data.table(id = 1:5, grp = c("a", "a", "b", "b", "c"))
    out <- key_dt(d)
    expect_true("id" %in% out$cols)
    expect_equal(out$n_cols[out$cols == "id"], 1L)
})

test_that("key_dt finds a minimal multi-column key and skips supersets", {
    d <- data.table(a = c(1, 1, 2, 2), b = c(1, 2, 1, 2))
    out <- key_dt(d)
    expect_true("a, b" %in% out$cols)
    expect_false(any(out$cols == "a, b" & out$n_cols != 2L))
})

test_that("key_dt returns zero rows when nothing under max_size uniquely identifies rows", {
    d <- data.table(a = c(1, 1), b = c(1, 1))
    out <- key_dt(d, max_size = 1L)
    expect_equal(nrow(out), 0L)
})

test_that("key_dt errors on non-data.table", {
    expect_error(key_dt(data.frame(x = 1)), "'dt' must be a data.table")
})

test_that("key_dt errors on missing columns", {
    d <- data.table(x = 1)
    expect_error(key_dt(d, cols = "zzz"), "not found")
})
