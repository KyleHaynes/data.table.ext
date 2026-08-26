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

# mssql SQL-text generation: no real SQL Server is available in this
# environment, so these check the generated SQL string shape directly
# rather than executing it (T-SQL's TOP/OFFSET-FETCH/NEWID() syntax isn't
# valid against a duckdb connection).

test_that("duckdt_limit_sql() emits TOP for mssql, LIMIT for duckdb", {
  expect_equal(duckdt_limit_sql('"t"', 5, "mssql"), 'SELECT TOP (5) * FROM "t"')
  expect_equal(duckdt_limit_sql('"t"', 5, "duckdb"), 'SELECT * FROM "t" LIMIT 5')
})

test_that("duckdt_tail_sql() emits OFFSET/FETCH with a constant ORDER BY for mssql", {
  expect_equal(
    duckdt_tail_sql('"t"', 5, 27, "mssql"),
    'SELECT * FROM "t" ORDER BY (SELECT NULL) OFFSET 27 ROWS FETCH NEXT 5 ROWS ONLY'
  )
  expect_equal(duckdt_tail_sql('"t"', 5, 27, "duckdb"), 'SELECT * FROM "t" LIMIT 5 OFFSET 27')
})

test_that("duckdt_sample_sql() emits TOP/ORDER BY NEWID() for mssql", {
  expect_equal(duckdt_sample_sql('"t"', 5, "mssql"), 'SELECT TOP (5) * FROM "t" ORDER BY NEWID()')
  expect_equal(duckdt_sample_sql('"t"', 5, "duckdb"), 'SELECT * FROM "t" USING SAMPLE reservoir(5 ROWS)')
})
