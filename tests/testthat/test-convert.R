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

test_that("as.duckdt() handles are writable immediately", {
  expect_true(mtcars_dt(copy = TRUE)$writable)
  expect_true(mtcars_dt(copy = FALSE)$writable)
})

test_that("duckdt(conn, table) defaults to a read-only handle", {
  d <- mtcars_dt(copy = TRUE)
  wrapped <- duckdt(d$conn, d$tbl)
  expect_false(wrapped$writable)
  expect_true(wrapped$materialized)
})

test_that("duckdt(conn, table, writable = TRUE) opts into a writable handle", {
  d <- mtcars_dt(copy = TRUE)
  wrapped <- duckdt(d$conn, d$tbl, writable = TRUE)
  expect_true(wrapped$writable)
})

test_that("print.duckdt flags a materialized-but-read-only handle", {
  d <- mtcars_dt(copy = TRUE)
  wrapped <- duckdt(d$conn, d$tbl)
  expect_output(print(wrapped), "\\(read-only\\)")
  expect_false(any(grepl("(read-only)", capture.output(print(d)), fixed = TRUE)))
})

test_that("copy = TRUE round-trips list-columns via DuckDB's native LIST type", {
  x <- data.table::data.table(id = 1:3, tags = list(c("a", "b"), "c", character(0)))
  d <- as.duckdt(x, name = "list_col_copy_test", overwrite = TRUE, copy = TRUE)
  expect_true(d$materialized)

  back <- as.data.table(d)
  back <- back[order(back$id), ]
  expect_equal(back$tags, x$tags)
})

test_that("copy = FALSE (zero-copy) round-trips list-columns via DuckDB's native LIST type", {
  x <- data.table::data.table(id = 1:3, tags = list(c("a", "b"), "c", character(0)))
  d <- as.duckdt(x, name = "list_col_view_test", overwrite = TRUE, copy = FALSE)
  expect_false(d$materialized)

  back <- as.data.table(d)
  back <- back[order(back$id), ]
  expect_equal(back$tags, x$tags)
})
