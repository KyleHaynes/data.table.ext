suppressPackageStartupMessages(library(data.table))

mtcars_dt <- function(copy = FALSE) {
  x <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")
  as.duckdt(x, name = "mtcars_test", overwrite = TRUE, copy = copy)
}

# Compare ignoring row order and data.table/data.frame attribute noise.
expect_same_rows <- function(actual, expected, by = NULL) {
  a <- as.data.frame(actual)
  e <- as.data.frame(expected)
  if (is.null(by)) by <- names(a)
  a <- a[do.call(order, a[by]), , drop = FALSE]
  e <- e[do.call(order, e[by]), , drop = FALSE]
  rownames(a) <- NULL
  rownames(e) <- NULL
  testthat::expect_equal(a[names(e)], e, ignore_attr = TRUE)
}
