labels_df <- function() {
  data.frame(
    cyl = c(4, 6, 8),
    label = c("four", "six", "eight"),
    stringsAsFactors = FALSE
  )
}

test_that("duckdt_join() joins a duckdt to an R data.frame", {
  d <- mtcars_dt()
  out <- duckdt_join(d, labels_df(), by = "cyl")

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), nrow(datasets::mtcars))
  expect_true("label" %in% names(out))
  expect_equal(unique(out$label[out$cyl == 6]), "six")
})

test_that("duckdt_join() joins an R data.frame to a duckdt (x is the R side)", {
  d <- mtcars_dt()
  out <- duckdt_join(labels_df(), d, by = "cyl")

  expect_equal(nrow(out), nrow(datasets::mtcars))
  # x's columns come first, after the key
  expect_equal(names(out)[1:2], c("cyl", "label"))
})

test_that("duckdt_join() joins two duckdt handles on the same connection", {
  d <- mtcars_dt()
  lab <- as.duckdt(labels_df(), conn = d$conn, name = "cyl_labels")
  out <- duckdt_join(d, lab, by = "cyl")

  expect_equal(nrow(out), nrow(datasets::mtcars))
  expect_true("label" %in% names(out))
})

test_that("duckdt_join() matches base merge()'s result", {
  d <- mtcars_dt()
  lab <- labels_df()
  expect_same_rows(
    duckdt_join(d, lab, by = "cyl"),
    merge(data.table::as.data.table(datasets::mtcars, keep.rownames = "car"), lab, by = "cyl"),
    by = c("car")
  )
})

test_that("duckdt_join() drops non-matching rows by default (inner join)", {
  d <- mtcars_dt()
  partial <- data.frame(cyl = 6, label = "six", stringsAsFactors = FALSE)
  out <- duckdt_join(d, partial, by = "cyl")
  expect_equal(nrow(out), sum(datasets::mtcars$cyl == 6))
})

test_that("duckdt_join(all.x = TRUE) keeps unmatched x rows with NA fill", {
  d <- mtcars_dt()
  partial <- data.frame(cyl = 6, label = "six", stringsAsFactors = FALSE)
  out <- duckdt_join(d, partial, by = "cyl", all.x = TRUE)

  expect_equal(nrow(out), nrow(datasets::mtcars))
  expect_true(all(is.na(out$label[out$cyl != 6])))
})

test_that("duckdt_join(all.y = TRUE) keeps unmatched y rows and coalesces the key", {
  d <- mtcars_dt()
  extra <- data.frame(cyl = c(6, 12), label = c("six", "twelve"), stringsAsFactors = FALSE)
  out <- duckdt_join(d, extra, by = "cyl", all.y = TRUE)

  expect_true(12 %in% out$cyl)
  # the y-only row has no x-side key, so the coalesced key must still be filled
  expect_false(any(is.na(out$cyl)))
  expect_true(is.na(out$mpg[out$cyl == 12]))
})

test_that("duckdt_join(all = TRUE) is a full join", {
  d <- mtcars_dt()
  extra <- data.frame(cyl = c(6, 12), label = c("six", "twelve"), stringsAsFactors = FALSE)
  out <- duckdt_join(d, extra, by = "cyl", all = TRUE)

  # every mtcars row, plus the unmatched cyl 4 and 8 rows, plus the y-only 12
  expect_equal(nrow(out), nrow(datasets::mtcars) + 1L)
  expect_true(12 %in% out$cyl)
  expect_false(any(is.na(out$cyl)))
})

test_that("duckdt_join() suffixes columns both sides share", {
  d <- mtcars_dt()
  y <- data.frame(cyl = c(4, 6, 8), hp = c(1, 2, 3))
  out <- duckdt_join(d, y, by = "cyl")

  expect_true(all(c("hp.x", "hp.y") %in% names(out)))
  expect_false("hp" %in% names(out))
  expect_equal(unique(out$hp.y[out$cyl == 6]), 2)
})

test_that("duckdt_join() honours a custom `suffixes`", {
  d <- mtcars_dt()
  y <- data.frame(cyl = c(4, 6, 8), hp = c(1, 2, 3))
  out <- duckdt_join(d, y, by = "cyl", suffixes = c("_left", "_right"))
  expect_true(all(c("hp_left", "hp_right") %in% names(out)))
})

test_that("duckdt_join() supports differently-named keys via by.x/by.y", {
  d <- mtcars_dt()
  y <- data.frame(cylinders = c(4, 6, 8), label = c("four", "six", "eight"),
                  stringsAsFactors = FALSE)
  out <- duckdt_join(d, y, by.x = "cyl", by.y = "cylinders")

  expect_true("cyl" %in% names(out))
  expect_false("cylinders" %in% names(out))
  expect_equal(nrow(out), nrow(datasets::mtcars))
})

test_that("duckdt_join() infers `by` from the shared columns", {
  d <- mtcars_dt()
  out <- duckdt_join(d, labels_df())
  expect_equal(nrow(out), nrow(datasets::mtcars))
})

test_that("duckdt_join() sorts by the key by default", {
  d <- mtcars_dt()
  out <- duckdt_join(d, labels_df(), by = "cyl")
  expect_false(is.unsorted(out$cyl))
})

test_that("duckdt_join() joins multiple key columns", {
  d <- mtcars_dt()
  y <- unique(data.table::as.data.table(datasets::mtcars)[, .(cyl, gear)])
  y$tag <- paste(y$cyl, y$gear)
  out <- duckdt_join(d, as.data.frame(y), by = c("cyl", "gear"))

  expect_equal(nrow(out), nrow(datasets::mtcars))
  expect_equal(out$tag, paste(out$cyl, out$gear))
})

test_that("duckdt_join() drops the tables it staged for R-side inputs", {
  d <- mtcars_dt()
  before <- DBI::dbListTables(d$conn)
  invisible(duckdt_join(d, labels_df(), by = "cyl"))
  expect_equal(sort(DBI::dbListTables(d$conn)), sort(before))
})

test_that("duckdt_join() cleans up its staging table even when the join errors", {
  d <- mtcars_dt()
  before <- DBI::dbListTables(d$conn)
  # a key column that exists on both sides but holds incomparable types
  y <- data.frame(cyl = "not a number", label = "x", stringsAsFactors = FALSE)
  try(duckdt_join(d, y, by = "cyl"), silent = TRUE)
  expect_equal(sort(DBI::dbListTables(d$conn)), sort(before))
})

test_that("duckdt_join() leaves both inputs untouched", {
  d <- mtcars_dt(copy = TRUE)
  before <- as.data.table(d)
  invisible(duckdt_join(d, labels_df(), by = "cyl"))
  expect_same_rows(as.data.table(d), before)
})

test_that("merge() on a duckdt dispatches to duckdt_join()", {
  d <- mtcars_dt()
  expect_same_rows(
    merge(d, labels_df(), by = "cyl"),
    duckdt_join(d, labels_df(), by = "cyl")
  )
})

test_that("duckdt_join() composes with duckdt_temp(), keeping the subset in the database", {
  d <- mtcars_dt()
  sub <- duckdt_temp(d, cyl == 6)
  out <- duckdt_join(sub, labels_df(), by = "cyl")

  expect_equal(nrow(out), sum(datasets::mtcars$cyl == 6))
  expect_equal(unique(out$label), "six")
})

test_that("duckdt_join() errors when neither side is a duckdt", {
  expect_error(
    duckdt_join(labels_df(), labels_df(), by = "cyl"),
    "at least one of"
  )
})

test_that("duckdt_join() errors on inputs that are neither duckdt nor data.frame", {
  d <- mtcars_dt()
  expect_error(duckdt_join(d, 1:3, by = "cyl"), "`y` must be")
  expect_error(duckdt_join(1:3, d, by = "cyl"), "`x` must be")
})

test_that("duckdt_join() errors when the two handles are on different connections", {
  a <- mtcars_dt()
  b <- mtcars_dt()
  expect_error(duckdt_join(a, b, by = "cyl"), "same connection")
})

test_that("duckdt_join() errors on unknown or unshared join columns", {
  d <- mtcars_dt()
  expect_error(duckdt_join(d, labels_df(), by = "nope"), "not found in `x`")
  expect_error(duckdt_join(d, data.frame(a = 1, b = 2), by = "cyl"), "not found in `y`")
  expect_error(duckdt_join(d, data.frame(a = 1, b = 2)), "share no columns")
})

test_that("duckdt_join() errors when only one of by.x/by.y is given, or lengths differ", {
  d <- mtcars_dt()
  y <- data.frame(cylinders = 6, label = "six", stringsAsFactors = FALSE)
  expect_error(duckdt_join(d, y, by.x = "cyl"), "both `by.x` and `by.y`")
  expect_error(duckdt_join(d, y, by.x = c("cyl", "gear"), by.y = "cylinders"), "same length")
})

test_that("duckdt_join() errors on a malformed `suffixes`", {
  d <- mtcars_dt()
  expect_error(duckdt_join(d, labels_df(), by = "cyl", suffixes = ".x"), "length 2")
})
