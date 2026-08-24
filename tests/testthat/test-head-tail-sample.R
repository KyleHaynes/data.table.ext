test_that("head() and tail() return the right number of rows", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_equal(nrow(head(d, 5)), 5L)
  expect_equal(nrow(tail(d, 5)), 5L)
  expect_equal(names(head(d, 5)), names(ref))
  expect_equal(names(tail(d, 5)), names(ref))
})

test_that("head() and tail() support negative n like utils::head/tail", {
  d <- mtcars_dt()
  nr <- nrow(d)

  expect_equal(nrow(head(d, -2)), nr - 2L)
  expect_equal(nrow(tail(d, -2)), nr - 2L)
})

test_that("head() and tail() clamp n to the row count", {
  d <- mtcars_dt()
  nr <- nrow(d)

  expect_equal(nrow(head(d, nr + 10)), nr)
  expect_equal(nrow(tail(d, nr + 10)), nr)
})

test_that("duckdt_sample() returns n rows drawn from the table", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  out <- duckdt_sample(d, 5)
  expect_equal(nrow(out), 5L)
  expect_true(all(out$car %in% ref$car))
})

test_that("duckdt_sample() clamps n to the row count", {
  d <- mtcars_dt()
  nr <- nrow(d)

  expect_equal(nrow(duckdt_sample(d, nr + 10)), nr)
})
