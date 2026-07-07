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
    # See the note on the dput test below: invoke the mask directly via
    # .GlobalEnv rather than a bare `str(dt)` call, for the same reason.
    enable_dt_str_mask()
    dt <- data.table(x = 1:3, y = letters[1:3])
    masked_str <- get("str", envir = .GlobalEnv, inherits = FALSE)
    out <- capture.output(masked_str(dt))
    expect_false(any(grepl(".internal.selfref", out, fixed = TRUE)))
    disable_dt_str_mask()
})

test_that("masked str rewrites header to 'A data.table' format", {
    enable_dt_str_mask()
    dt <- data.table(x = 1:5)
    masked_str <- get("str", envir = .GlobalEnv, inherits = FALSE)
    out <- capture.output(masked_str(dt))
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
    # Invoke the installed mask directly via .GlobalEnv rather than a bare
    # `dput(dt)` call: under R CMD check / test_dir(), sourced test code
    # resolves the bare `dput` symbol straight to base::dput regardless of
    # a .GlobalEnv override (a testthat/R sourcing quirk, not a mask bug),
    # so a bare call here would not actually exercise the masked closure.
    enable_dt_dput_mask()
    dt <- data.table(x = 1:2)
    masked_dput <- get("dput", envir = .GlobalEnv, inherits = FALSE)
    out <- capture.output(masked_dput(dt))
    expect_false(any(grepl(".internal.selfref", out, fixed = TRUE)))
    disable_dt_dput_mask()
})
