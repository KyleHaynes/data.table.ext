test_that("duckdt_merge() updates matches and inserts new rows from a data.frame by default", {
  d <- mtcars_dt(copy = TRUE)
  before <- as.data.table(d)

  patch <- data.frame(
    car = c("Mazda RX4", "New Car"),
    hp = c(999, 111),
    cyl = c(6, 4),
    stringsAsFactors = FALSE
  )
  invisible(duckdt_merge(d, patch, by = "car"))

  out <- as.data.table(d)
  expect_equal(nrow(out), nrow(before) + 1L)
  expect_equal(out$hp[out$car == "Mazda RX4"], 999)
  expect_equal(out$cyl[out$car == "Mazda RX4"], 6)
  expect_true("New Car" %in% out$car)
  expect_equal(out$hp[out$car == "New Car"], 111)
  # untouched rows are unaffected
  expect_equal(out$hp[out$car == "Datsun 710"], before$hp[before$car == "Datsun 710"])
})

test_that("duckdt_merge() infers `by` as the shared column(s) when not specified", {
  # Only "car" overlaps between x and this patch, so it becomes the (sole)
  # match key -- unlike the `by = "car"` tests above, every shared column
  # would become part of the key with no explicit `by` (base merge()-style
  # natural join), so this case is deliberately single-column to stay
  # unambiguous.
  d <- mtcars_dt(copy = TRUE)
  patch <- data.frame(car = "Brand New Car", stringsAsFactors = FALSE)
  invisible(duckdt_merge(d, patch))
  out <- as.data.table(d)
  expect_true("Brand New Car" %in% out$car)
  expect_true(is.na(out$hp[out$car == "Brand New Car"]))
})

test_that("duckdt_merge() accepts another duckdt object as `y`", {
  d <- mtcars_dt(copy = TRUE)
  src <- as.duckdt(
    data.table::data.table(car = "Mazda RX4", hp = 42),
    conn = d$conn, name = "src_patch", copy = TRUE
  )
  invisible(duckdt_merge(d, src, by = "car"))
  out <- as.data.table(d)
  expect_equal(out$hp[out$car == "Mazda RX4"], 42)
})

test_that("duckdt_merge(insert = FALSE) skips rows not already present", {
  d <- mtcars_dt(copy = TRUE)
  patch <- data.frame(car = c("Mazda RX4", "New Car"), hp = c(999, 111))
  invisible(duckdt_merge(d, patch, by = "car", insert = FALSE))
  out <- as.data.table(d)
  expect_false("New Car" %in% out$car)
  expect_equal(out$hp[out$car == "Mazda RX4"], 999)
})

test_that("duckdt_merge(update = FALSE) skips updating existing rows", {
  d <- mtcars_dt(copy = TRUE)
  before <- as.data.table(d)
  patch <- data.frame(car = c("Mazda RX4", "New Car"), hp = c(999, 111))
  invisible(duckdt_merge(d, patch, by = "car", update = FALSE))
  out <- as.data.table(d)
  expect_equal(out$hp[out$car == "Mazda RX4"], before$hp[before$car == "Mazda RX4"])
  expect_true("New Car" %in% out$car)
})

test_that("duckdt_merge(delete = TRUE) removes rows absent from y (full subset replace)", {
  d <- mtcars_dt(copy = TRUE)
  keep <- data.table::as.data.table(datasets::mtcars, keep.rownames = "car")[1:3]
  invisible(duckdt_merge(d, keep, by = "car", delete = TRUE))
  out <- as.data.table(d)
  expect_equal(nrow(out), 3L)
  expect_setequal(out$car, keep$car)
})

test_that("duckdt_merge() drops its staging table after running", {
  d <- mtcars_dt(copy = TRUE)
  patch <- data.frame(car = "Mazda RX4", hp = 1)
  invisible(duckdt_merge(d, patch, by = "car"))
  tbls <- duckdt_tables(d)
  expect_false(any(grepl("__duckdt_merge_tmp", tbls$name)))
})

test_that("duckdt_merge() errors on a read-only (registered) view", {
  d <- mtcars_dt(copy = FALSE)
  expect_error(duckdt_merge(d, data.frame(car = "Mazda RX4", hp = 1), by = "car"), "materialized")
})

test_that("duckdt_merge() errors when update/insert/delete are all FALSE", {
  d <- mtcars_dt(copy = TRUE)
  expect_error(
    duckdt_merge(d, data.frame(car = "Mazda RX4", hp = 1), by = "car", update = FALSE, insert = FALSE),
    "at least one of"
  )
})

test_that("duckdt_merge() errors on unknown `by` columns", {
  d <- mtcars_dt(copy = TRUE)
  expect_error(duckdt_merge(d, data.frame(car = "Mazda RX4"), by = "nope"), "not found in `x`")
  expect_error(duckdt_merge(d, data.frame(nope = "x"), by = "car"), "not found in `y`")
})

test_that("duckdt_merge() errors when x and y share no columns and `by` is unspecified", {
  d <- mtcars_dt(copy = TRUE)
  expect_error(duckdt_merge(d, data.frame(totally_unrelated = 1)), "share no columns")
})

test_that("duckdt_merge() ignores y-only columns not present in x", {
  d <- mtcars_dt(copy = TRUE)
  patch <- data.frame(car = "Mazda RX4", hp = 999, made_up_col = "ignored")
  invisible(duckdt_merge(d, patch, by = "car"))
  out <- as.data.table(d)
  expect_false("made_up_col" %in% names(out))
  expect_equal(out$hp[out$car == "Mazda RX4"], 999)
})
