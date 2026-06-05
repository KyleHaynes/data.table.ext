library(data.table)

# ── str mask ───────────────────────────────────────────────────────────────────

test_that("enable/disable str mask roundtrips cleanly", {
    disable_dt_str_mask()
    expect_false(exists("str", envir = .GlobalEnv, inherits = FALSE))

    enable_dt_str_mask()
    expect_true(exists("str", envir = .GlobalEnv, inherits = FALSE))

    disable_dt_str_mask()
    expect_false(exists("str", envir = .GlobalEnv, inherits = FALSE))
})

test_that("masked str removes .internal.selfref line", {
    enable_dt_str_mask()
    dt <- data.table(x = 1:3, y = letters[1:3])
    out <- capture.output(str(dt))
    expect_false(any(grepl(".internal.selfref", out, fixed = TRUE)))
    disable_dt_str_mask()
})

test_that("masked str rewrites header to 'A data.table' format", {
    enable_dt_str_mask()
    dt <- data.table(x = 1:5)
    out <- capture.output(str(dt))
    expect_true(any(grepl("data\\.table", out)))
    disable_dt_str_mask()
})

# ── dput mask ──────────────────────────────────────────────────────────────────

test_that("enable/disable dput mask roundtrips cleanly", {
    disable_dt_dput_mask()
    expect_false(exists("dput", envir = .GlobalEnv, inherits = FALSE))

    enable_dt_dput_mask()
    expect_true(exists("dput", envir = .GlobalEnv, inherits = FALSE))

    disable_dt_dput_mask()
    expect_false(exists("dput", envir = .GlobalEnv, inherits = FALSE))
})

test_that("masked dput removes .internal.selfref attribute", {
    enable_dt_dput_mask()
    dt <- data.table(x = 1:2)
    out <- capture.output(dput(dt))
    expect_false(any(grepl(".internal.selfref", out, fixed = TRUE)))
    disable_dt_dput_mask()
})
