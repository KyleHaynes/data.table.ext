test_that("duckdt_temp() lands an `i` subset in the database and returns a handle", {
  d <- mtcars_dt()
  sub <- duckdt_temp(d, cyl == 6)

  expect_s3_class(sub, "duckdt")
  expect_equal(nrow(sub), sum(datasets::mtcars$cyl == 6))
  expect_setequal(names(sub), names(d))
  # the subset is a real table on the same connection, not an R copy
  expect_identical(sub$conn, d$conn)
  expect_true(DBI::dbExistsTable(d$conn, sub$tbl))
})

test_that("duckdt_temp() honours `j` and `by` the same way `[` does", {
  d <- mtcars_dt()
  agg <- duckdt_temp(d, , .(mean_mpg = mean(mpg)), by = cyl)

  expect_setequal(names(agg), c("cyl", "mean_mpg"))
  expect_same_rows(as.data.table(agg), d[, .(mean_mpg = mean(mpg)), by = cyl], by = "cyl")
})

test_that("duckdt_temp() with no query copies the whole table", {
  d <- mtcars_dt()
  cp <- duckdt_temp(d)
  expect_equal(nrow(cp), nrow(d))
})

test_that("duckdt_temp() handles are materialized and writable, and writes don't touch the source", {
  d <- mtcars_dt()
  sub <- duckdt_temp(d, cyl == 6)

  expect_true(sub$materialized)
  expect_true(sub$writable)

  sub[, hp := 0]
  expect_true(all(as.data.table(sub)$hp == 0))
  # the original table is untouched
  expect_true(all(d[cyl == 6]$hp > 0))
})

test_that("duckdt_temp() names are unique, so two subsets don't clobber each other", {
  d <- mtcars_dt()
  a <- duckdt_temp(d, cyl == 6)
  b <- duckdt_temp(d, cyl == 4)

  expect_false(identical(a$tbl, b$tbl))
  expect_equal(nrow(a), sum(datasets::mtcars$cyl == 6))
  expect_equal(nrow(b), sum(datasets::mtcars$cyl == 4))
})

test_that("duckdt_temp() rejects `:=`, which has no result to store", {
  d <- mtcars_dt(copy = TRUE)
  expect_error(duckdt_temp(d, , hp2 := hp * 2), "no result to store")
})

test_that("duckdt_temp() rejects a non-duckdt x", {
  expect_error(duckdt_temp(datasets::mtcars, cyl == 6), "must be a duckdt object")
})

test_that("print.duckdt() marks a duckdt_temp() handle as temp", {
  d <- mtcars_dt()
  sub <- duckdt_temp(d, cyl == 6)
  expect_output(print(sub), "\\(temp\\)")
})

test_that("duckdt_drop() removes the temporary table", {
  d <- mtcars_dt()
  sub <- duckdt_temp(d, cyl == 6)
  nm <- sub$tbl

  duckdt_drop(sub)
  expect_false(DBI::dbExistsTable(d$conn, nm))
})

test_that("duckdt_drop() refuses a real table unless forced", {
  d <- mtcars_dt(copy = TRUE)
  expect_error(duckdt_drop(d), "duckdt_temp")
  expect_true(DBI::dbExistsTable(d$conn, d$tbl))

  duckdt_drop(d, force = TRUE)
  expect_false(DBI::dbExistsTable(d$conn, d$tbl))
})

test_that("duckdt() detects a TEMP table as materialized, not as a view", {
  d <- mtcars_dt()
  sub <- duckdt_temp(d, cyl == 6)
  wrapped <- duckdt(d$conn, sub$tbl)
  expect_true(wrapped$materialized)
})
