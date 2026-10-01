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

test_that(":= keeps the table's primary key and works on a referenced table", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE parent (id INTEGER PRIMARY KEY, name VARCHAR NOT NULL)")
  DBI::dbExecute(con, "CREATE TABLE child (id INTEGER PRIMARY KEY, parent_id INTEGER REFERENCES parent(id))")
  DBI::dbExecute(con, "INSERT INTO parent VALUES (1, 'a'), (2, 'b')")
  DBI::dbExecute(con, "INSERT INTO child VALUES (10, 1)")

  p <- dbdt(con, "parent", writable = TRUE)
  invisible(p[id == 2, name := "B"])
  expect_equal(p[id == 2]$name, "B")
  expect_equal(nrow(dbdt_schema(con, "parent")[primary_key == TRUE]), 1L)
  # The key is still enforced.
  expect_error(DBI::dbExecute(con, "INSERT INTO parent VALUES (1, 'dupe')"))

  ch <- dbdt(con, "child", writable = TRUE)
  invisible(ch[, note := "x"])
  expect_equal(ch[]$note, "x")
  expect_equal(nrow(dbdt_relationships(con)), 1L)
})

test_that(":= with i keeps the column's type; a whole-column := replaces it", {
  d <- as.dbdt(data.frame(id = 1:3, n = c(1L, 2L, 3L)), name = "typed", copy = TRUE)
  invisible(d[id == 1, n := 10L])
  expect_type(d[]$n, "integer")
  invisible(d[, n := n / 2])
  expect_type(d[]$n, "double")
  expect_equal(sort(d[]$n), c(1, 1.5, 5))
})

test_that("literal values give new columns R's types", {
  d <- as.dbdt(data.frame(id = 1:3), name = "lits", copy = TRUE)
  invisible(d[, rate := 0.5])
  invisible(d[id == 2, rate := 12.25])
  invisible(d[, flag := "no"])
  invisible(d[id == 3, flag := "a much longer value"])
  invisible(d[, ok := NA])
  out <- d[][order(id)]
  expect_equal(out$rate, c(0.5, 12.25, 0.5))
  expect_equal(out$flag, c("no", "no", "a much longer value"))
  expect_type(out$ok, "logical")
})

test_that(":= NULL drops a column", {
  d <- mtcars_dt(copy = TRUE)
  invisible(d[, carb := NULL])
  expect_false("carb" %in% names(d))
  expect_false("carb" %in% names(as.data.table(d)))
})

test_that("a multi-column := is all or nothing", {
  d <- mtcars_dt(copy = TRUE)
  before <- as.data.table(d)
  # Translation fails before anything is written.
  expect_error(d[, `:=`(a = hp + 1, b = no_such_variable)], "could not resolve")
  expect_identical(names(d), names(before))
  # So does an error from the database partway through.
  expect_error(d[, `:=`(a = hp + 1, b = no_such_fn(hp))])
  expect_false("a" %in% names(as.data.table(d)))
  expect_equal(as.data.table(d), before)
})

test_that("later assignments in one := see earlier ones", {
  d <- mtcars_dt(copy = TRUE)
  invisible(d[, `:=`(kw = hp * 0.7457, kw2 = kw * 2)])
  out <- as.data.table(d)
  expect_equal(out$kw2, out$hp * 0.7457 * 2, tolerance = 1e-8)
})
