test_that("j missing returns all columns", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(d[], ref)
})

test_that(".() selects and renames columns", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(d[, .(mpg, hp)], ref[, .(mpg, hp)])
  expect_same_rows(d[, .(mpg, horsepower = hp)], ref[, .(mpg, horsepower = hp)])
})

test_that("computed columns in j work", {
  d <- mtcars_dt()
  out <- d[, .(car, kw = hp * 0.7457)]
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")[, .(car, kw = hp * 0.7457)]
  expect_same_rows(out, ref)
})

test_that("a bare column symbol selects a single column", {
  d <- mtcars_dt()
  out <- d[, mpg]
  expect_equal(names(out), "mpg")
  expect_equal(nrow(out), 32L)
})

test_that("i and j combine", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_same_rows(d[cyl == 6, .(car, mpg)], ref[cyl == 6, .(car, mpg)])
})
