erd_db <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "CREATE TABLE customers (id INTEGER PRIMARY KEY, name VARCHAR)")
  DBI::dbExecute(con, "
    CREATE TABLE orders (
      id INTEGER PRIMARY KEY,
      customer_id INTEGER,
      total DOUBLE,
      FOREIGN KEY (customer_id) REFERENCES customers(id)
    )
  ")
  con
}

test_that("duckdt_data_model() reverse-engineers tables, keys and references", {
  con <- erd_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  dm <- duckdt_data_model(con)
  expect_true(is_duckdt_data_model(dm))
  expect_setequal(dm$tables$table, c("customers", "orders"))
  expect_equal(dm$tables$schema, c("main", "main"))

  expect_equal(dm$columns[table == "customers" & column == "id"]$key, 1L)
  expect_equal(dm$columns[table == "orders" & column == "total"]$key, 0L)

  expect_equal(nrow(dm$references), 1L)
  expect_equal(dm$references$table, "orders")
  expect_equal(dm$references$column, "customer_id")
  expect_equal(dm$references$ref, "customers")
  expect_equal(dm$references$ref_col, "id")
  expect_equal(dm$references$ref_id, 1L)
})

test_that("duckdt_data_model() accepts a duckdt handle and can count rows", {
  d <- mtcars_dt(copy = TRUE)
  dm <- duckdt_data_model(d, tables = "mtcars_test", row_counts = TRUE)

  expect_equal(dm$tables$table, "mtcars_test")
  expect_equal(dm$tables$n_rows, 32)
  expect_equal(nrow(dm$columns), 12L)
  expect_equal(nrow(dm$references), 0L)
})

test_that("duckdt_data_model() restricts to `tables` and rejects unknown ones", {
  con <- erd_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  dm <- duckdt_data_model(con, tables = "orders")
  expect_equal(dm$tables$table, "orders")
  expect_true(all(dm$columns$table == "orders"))
  # The referenced table is gone, so there is nothing left to draw an edge to.
  expect_equal(nrow(dm$references), 0L)

  expect_error(duckdt_data_model(con, tables = "nope"), "none of")
})

test_that("duckdt_data_model() builds from a named list of data frames", {
  dm <- duckdt_data_model(list(
    people = data.frame(id = 1L, name = "a"),
    pets = data.frame(id = 1L, owner = 1L)
  ))
  expect_setequal(dm$tables$table, c("people", "pets"))
  expect_equal(dm$columns[table == "people" & column == "id"]$type, "integer")
  expect_equal(nrow(dm$references), 0L)

  expect_error(duckdt_data_model(list(data.frame(a = 1))), "must be named")
  expect_error(duckdt_data_model(list(a = 1)), "must be a data frame")
})

test_that("duckdt_data_model() builds from a column-info data frame", {
  dm <- duckdt_data_model(data.frame(
    table = c("a", "a", "b"),
    column = c("id", "b_id", "id"),
    key = c(TRUE, FALSE, TRUE),
    ref = c(NA, "b", NA),
    stringsAsFactors = FALSE
  ))
  expect_equal(nrow(dm$references), 1L)
  expect_equal(dm$references$ref_col, "id")   # filled in from b's key

  expect_error(duckdt_data_model(data.frame(x = 1)), "table.*column")
})

test_that("duckdt_data_model() errors clearly on an empty or unsupported input", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_error(duckdt_data_model(con), "no tables")
  expect_error(duckdt_data_model(1:3), "don't know how")
})

test_that("two same-named tables in different schemas stay distinct", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE SCHEMA other")
  DBI::dbExecute(con, "CREATE TABLE items (id INTEGER PRIMARY KEY)")
  DBI::dbExecute(con, "CREATE TABLE other.items (id INTEGER PRIMARY KEY, note VARCHAR)")

  dm <- duckdt_data_model(con)
  expect_setequal(dm$tables$table, c("main.items", "other.items"))
  expect_equal(nrow(dm$columns[table == "other.items"]), 2L)
})

test_that("print() summarises the model", {
  con <- erd_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  out <- capture.output(print(duckdt_data_model(con)))
  expect_match(out[1], "2 tables, 5 columns, 1 references")
  expect_match(paste(out, collapse = "\n"), "PK: id")
  expect_match(paste(out, collapse = "\n"), "1 FK")
})
