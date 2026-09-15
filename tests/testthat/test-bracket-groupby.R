test_that("by + aggregate matches data.table", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(
    d[, .(avg_mpg = mean(mpg), n = .N), by = cyl],
    ref[, .(avg_mpg = mean(mpg), n = .N), by = cyl],
    by = "cyl"
  )
})

test_that("by accepts a character vector and multiple grouping columns", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(
    d[, .(total = sum(hp)), by = c("cyl", "gear")],
    ref[, .(total = sum(hp)), by = c("cyl", "gear")],
    by = c("cyl", "gear")
  )
})

test_that("bare .N with by works and is named N", {
  d <- mtcars_dt()
  out <- d[, .N, by = cyl]
  expect_setequal(names(out), c("cyl", "N"))
  expect_equal(sum(out$N), 32L)
})

test_that("filter + group by combine", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(
    d[hp > 100, .(max_mpg = max(mpg)), by = cyl],
    ref[hp > 100, .(max_mpg = max(mpg)), by = cyl],
    by = "cyl"
  )
})
