test_that("as.duckdt round-trips a data.table", {
  ref <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  d <- mtcars_dt()

  expect_s3_class(d, "duckdt")
  expect_equal(dim(d), dim(ref))
  expect_equal(nrow(d), nrow(ref))
  expect_equal(ncol(d), ncol(ref))
  expect_setequal(names(d), names(ref))

  back <- as.data.table(d)
  expect_s3_class(back, "data.table")
  expect_same_rows(back, ref)
})

test_that("copy = TRUE materializes a real, mutable table", {
  d <- mtcars_dt(copy = TRUE)
  expect_true(d$materialized)
})

test_that("copy = FALSE (default) registers a zero-copy, read-only view", {
  d <- mtcars_dt(copy = FALSE)
  expect_false(d$materialized)
})
