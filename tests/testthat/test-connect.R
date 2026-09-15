test_that("duckdt_connect() opens an in-memory database and reports it", {
  expect_message(con <- duckdt_connect(), "in-memory database")
  on.exit(duckdt_disconnect(con))

  expect_true(DBI::dbIsValid(con))
  expect_equal(duckdt_dialect(con), "duckdb")
  expect_null(duckdt_dbdir(con))
  expect_equal(duckdt_conn_label(con), "In-memory DuckDB")
})

test_that("duckdt_connect() summarises an existing file's tables", {
  path <- file.path(tempdir(), "duckdt-connect-test.duckdb")
  unlink(path)
  on.exit(unlink(path), add = TRUE)

  con <- duckdt_connect(path, quiet = TRUE)
  DBI::dbExecute(con, "CREATE TABLE orders (id INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE customers (id INTEGER)")
  duckdt_disconnect(con)

  expect_message(con <- duckdt_connect(path), "2 tables")
  on.exit(duckdt_disconnect(con), add = TRUE)
  expect_equal(normalizePath(duckdt_dbdir(con)), normalizePath(path))
  expect_equal(duckdt_conn_label(con), basename(path))
})

test_that("duckdt_connect(quiet = TRUE) says nothing", {
  # duckdb itself may emit one-off messages about its extension directory, so
  # this checks only that duckdt's own summary is absent.
  said <- character()
  withCallingHandlers(
    con <- duckdt_connect(quiet = TRUE),
    message = function(m) said <<- c(said, conditionMessage(m))
  )
  duckdt_disconnect(con)
  expect_false(any(grepl("Connected to DuckDB", said)))
})

test_that("duckdt_disconnect() accepts a duckdt handle", {
  con <- duckdt_connect(quiet = TRUE)
  d <- as.duckdt(data.frame(x = 1), conn = con, name = "x1", copy = TRUE)
  expect_true(duckdt_disconnect(d))
  expect_false(DBI::dbIsValid(con))
})

test_that("duckdt_example() builds a database worth drawing", {
  expect_message(con <- duckdt_example(), "Example database ready")
  on.exit(duckdt_disconnect(con))

  dm <- duckdt_data_model(con)
  expect_setequal(dm$tables$table, c("customers", "items", "order_lines", "orders"))
  # order_lines has a compound key and references both orders and items.
  expect_equal(sum(dm$columns[table == "order_lines"]$key > 0), 2L)
  expect_equal(length(unique(dm$references$ref_id)), 3L)

  expect_equal(nrow(as.data.table(duckdt(con, "order_lines"))), 4L)
})

test_that("duckdt_explorer() builds an app object", {
  skip_if_not_installed("shiny")
  con <- duckdt_example(quiet = TRUE)
  on.exit(duckdt_disconnect(con))

  app <- duckdt_explorer(con)
  expect_s3_class(app, "shiny.appobj")

  # It also works with no connection at all, on a data model alone.
  expect_s3_class(duckdt_explorer(duckdt_data_model(con)), "shiny.appobj")
})
