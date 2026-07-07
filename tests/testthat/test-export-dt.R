library(data.table)

dt <- data.table(
    a = 1:3,
    b = c("x", "y", NA_character_)
)

# ── md_dt ──────────────────────────────────────────────────────────────────────

test_that("md_dt renders a markdown pipe table", {
    out <- capture.output(txt <- md_dt(dt))
    expect_equal(out[1], "| a | b |")
    expect_equal(out[2], "| --- | --- |")
    expect_equal(out[3], "| 1 | x |")
    expect_equal(out[4], "| 2 | y |")
    expect_equal(out[5], "| 3 |  |")
    expect_equal(txt, paste(out, collapse = "\n"))
})

test_that("md_dt respects the n limit", {
    out <- capture.output(md_dt(dt, n = 2))
    expect_equal(length(out), 4L)
})

test_that("md_dt handles zero-row tables", {
    empty <- data.table(a = integer(0), b = character(0))
    out <- capture.output(md_dt(empty))
    expect_equal(out, c("| a | b |", "| --- | --- |"))
})

test_that("md_dt escapes pipe characters in cell values", {
    d <- data.table(x = "a|b")
    out <- capture.output(md_dt(d))
    expect_true(grepl("a\\|b", out[3], fixed = TRUE))
})

test_that("md_dt errors on non-data.table", {
    expect_error(md_dt(data.frame(x = 1)), "'dt' must be a data.table")
})

# ── copy_dt ────────────────────────────────────────────────────────────────────

test_that("copy_dt returns the same markdown text as md_dt", {
    md_out <- capture.output(md_txt <- md_dt(dt))
    copy_txt <- copy_dt(dt, clip_fun = function(text) invisible(TRUE))
    expect_equal(copy_txt, md_txt)
})

test_that("copy_dt passes the rendered text to clip_fun", {
    captured <- NULL
    copy_dt(dt, clip_fun = function(text) captured <<- text)
    expect_equal(captured, .render_md_table(dt))
})

test_that("copy_dt errors on non-data.table", {
    expect_error(copy_dt(data.frame(x = 1)), "'dt' must be a data.table")
})
