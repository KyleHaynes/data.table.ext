test_that("frequency limits accept Inf and large finite values", {
  dt <- data.table(x = c("a", "a", "b"))
  for (n in c(Inf, .Machine$integer.max + 1)) {
    out <- expect_no_warning(freq_dt(dt, x, n = n))
    expect_equal(out$x, c("a", "b"))
    expect_identical(out$n, c(2L, 1L))
    expect_equal(out$pct, c(2 / 3, 1 / 3))
    expect_equal(nrow(freq_dt(dt[0L], x, n = n)), 0L)
  }
  expect_error(freq_dt(dt, x, n = numeric()), "positive")
})

test_that("duplicate detection preserves user columns and the caller's table", {
  dt <- data.table(x = c(1L, 1L, 2L), .dupe_n = c("a", "b", "c"))
  before <- copy(dt)
  out <- dupe_dt(dt, by = "x", color = FALSE)
  expect_identical(out$.dupe_n, c("a", "b"))
  expect_identical(dt, before)
  expect_identical(dupe_dt(dt[0L], by = "x", color = FALSE)$.dupe_n, character())
})

test_that("masked dput round-trips attributes without mutating its input", {
  enable_dt_dput_mask()
  on.exit(disable_dt_dput_mask())
  masked_dput <- get("dput", envir = .GlobalEnv, inherits = FALSE)
  dt <- data.table(x = 1:2)
  setattr(dt, "note", "keep this attribute")
  before <- copy(dt)
  text <- capture.output(returned <- withVisible(masked_dput(dt)))
  expect_false(any(grepl(".internal.selfref", text, fixed = TRUE)))
  restored <- eval(parse(text = text))
  expect_identical(restored$x, dt$x)
  expect_identical(attr(restored, "note"), attr(dt, "note"))
  expect_identical(dt, before)
  expect_identical(returned$value, dt)
  expect_false(returned$visible)

  path <- tempfile(fileext = ".R")
  on.exit(unlink(path), add = TRUE)
  masked_dput(dt, file = path)
  expect_identical(dget(path)$x, dt$x)
})
