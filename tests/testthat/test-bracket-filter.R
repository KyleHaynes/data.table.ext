test_that("simple equality filter matches data.table", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(d[cyl == 6], ref[cyl == 6])
})

test_that("comparison and boolean operators translate correctly", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(d[cyl >= 6 & mpg < 20], ref[cyl >= 6 & mpg < 20])
  expect_same_rows(d[cyl == 4 | cyl == 8], ref[cyl == 4 | cyl == 8])
  expect_same_rows(d[!(cyl == 6)], ref[!(cyl == 6)])
})

test_that("%in% and %between% translate correctly", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(d[cyl %in% c(4, 6)], ref[cyl %in% c(4, 6)])
  expect_same_rows(d[mpg %between% c(15, 20)], ref[mpg %between% c(15, 20)])
})

test_that("filtering against a value from the calling environment works", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()
  min_mpg <- 20

  expect_same_rows(d[mpg > min_mpg], ref[mpg > min_mpg])
})

test_that("is.na() translates to IS NULL", {
  d <- mtcars_dt(copy = TRUE)
  invisible(d[, opt := NA_real_])
  invisible(d[cyl == 6, opt := hp])

  out <- d[is.na(opt)]
  expect_true(all(out$cyl != 6))
  expect_equal(nrow(out) + nrow(d[!is.na(opt)]), nrow(d))
})

test_that("%like%/%ilike%/%flike%/%plike%/%chin% translate correctly", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(d[car %like% "^Merc"], ref[car %like% "^Merc"])
  expect_same_rows(d[car %ilike% "^merc"], ref[car %ilike% "^merc"])
  expect_same_rows(d[car %flike% "Merc 450"], ref[car %like% "Merc 450"])
  expect_same_rows(d[car %plike% "^Merc"], ref[car %like% "^Merc"])
  expect_same_rows(
    d[car %chin% c("Valiant", "Duster 360")],
    ref[car %chin% c("Valiant", "Duster 360")]
  )
})

test_that("string equality filter works", {
  d <- mtcars_dt()
  out <- d[car == "Datsun 710"]
  expect_equal(nrow(out), 1L)
  expect_equal(out$car, "Datsun 710")
})
