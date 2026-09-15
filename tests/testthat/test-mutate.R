test_that(":= adds a new column on a materialized table", {
  d <- mtcars_dt(copy = TRUE)
  invisible(d[, kw := hp * 0.7457])

  out <- as.data.table(d)
  expect_true("kw" %in% names(out))
  expect_equal(out$kw, out$hp * 0.7457, tolerance = 1e-8)
})

test_that(":= with i only updates matching rows", {
  d <- mtcars_dt(copy = TRUE)
  invisible(d[, flag := "no"])
  invisible(d[cyl == 6, flag := "yes"])

  out <- as.data.table(d)
  expect_equal(sort(unique(out$flag)), c("no", "yes"))
  expect_true(all(out$flag[out$cyl == 6] == "yes"))
  expect_true(all(out$flag[out$cyl != 6] == "no"))
})

test_that(":= updates an existing column in place", {
  d <- mtcars_dt(copy = TRUE)
  before <- as.data.table(d)
  invisible(d[, hp := hp * 2])

  out <- as.data.table(d)
  expect_equal(out$hp, before$hp * 2)
})

test_that(":= errors on a read-only (registered) view", {
  d <- mtcars_dt(copy = FALSE)
  expect_error(d[, kw := hp * 0.7457], "materialized")
})

test_that(":= errors when combined with by", {
  d <- mtcars_dt(copy = TRUE)
  expect_error(d[, avg := mean(hp), by = cyl], "not supported")
})

test_that(":= errors on a duckdt(conn, table) handle by default (read-only guard)", {
  d <- mtcars_dt(copy = TRUE)
  wrapped <- duckdt(d$conn, d$tbl)
  expect_false(wrapped$writable)
  expect_error(wrapped[, kw := hp * 0.7457], "writable")
})

test_that(":= works on a duckdt(conn, table) handle with writable = TRUE", {
  d <- mtcars_dt(copy = TRUE)
  wrapped <- duckdt(d$conn, d$tbl, writable = TRUE)
  invisible(wrapped[, kw := hp * 0.7457])

  out <- as.data.table(wrapped)
  expect_true("kw" %in% names(out))
})
