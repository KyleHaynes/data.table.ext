# The binary columns here are a BLOB (which DuckDB can hand back to R, as a
# list of raw vectors) and a BIT (which it cannot -- `dbGetQuery()` fails at
# prepare time). Both are excluded, so a table reads the same way whether or
# not the driver happens to cope.
binary_duckdt <- function(conn) {
  DBI::dbExecute(conn, "CREATE OR REPLACE TABLE docs (
     id INTEGER, name VARCHAR, payload VARBINARY, flag BIT)")
  DBI::dbExecute(conn, "INSERT INTO docs VALUES
     (1, 'a', 'xx'::BLOB, '101'::BIT), (2, 'b', 'yy'::BLOB, '110'::BIT)")
  duckdt(conn, "docs")
}

test_that("a table with binary columns opens at all", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  # DuckDB's own dbExistsTable()/dbListFields() probe with a SELECT that
  # fails on the BIT column, so duckdt() has to fall back to the catalogue.
  d <- binary_duckdt(conn)
  expect_s3_class(d, "duckdt")
  expect_equal(duckdt_columns(d), c("id", "name", "payload", "flag"))
  expect_equal(dim(d), c(2L, 4L))
})

test_that("binary columns are left out of the rows returned to R", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  d <- binary_duckdt(conn)

  expect_equal(names(d[]), c("id", "name"))
  expect_equal(names(d[id == 2]), c("id", "name"))
  expect_equal(names(head(d, 1)), c("id", "name"))
  expect_equal(names(tail(d, 1)), c("id", "name"))
  expect_equal(names(duckdt_sample(d, 1)), c("id", "name"))
  expect_equal(names(data.table::as.data.table(d)), c("id", "name"))
  expect_equal(d[id == 2]$name, "b")
})

test_that("naming a binary column in j still selects it", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  d <- binary_duckdt(conn)
  res <- d[, .(id, payload)]
  expect_equal(names(res), c("id", "payload"))
  expect_true(is.list(res$payload))
})

test_that("binary columns still exist in the database", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  d <- binary_duckdt(conn)
  # duckdt_temp() keeps its rows in the database, so it copies every column.
  t <- duckdt_temp(d, id == 1)
  expect_true(all(c("payload", "flag") %in% duckdt_columns(t)))
  expect_equal(as.integer(nrow(t)), 1L)
})

test_that("a join drops binary columns from the result but not from the keys", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  d <- binary_duckdt(conn)

  labels <- data.frame(id = 1:2, label = c("one", "two"))
  res <- duckdt_join(d, labels, by = "id")
  expect_equal(names(res), c("id", "name", "label"))

  # `by` is resolved before anything is dropped, so a binary join key is
  # kept -- the join must not silently key on a different set of columns.
  keyed <- duckdt_join(d, duckdt_temp(d), by = "payload")
  expect_true("payload" %in% names(keyed))
  expect_equal(nrow(keyed), 2L)
})

test_that("print() reports the columns it isn't fetching", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  d <- binary_duckdt(conn)
  out <- paste(utils::capture.output(print(d)), collapse = "
")
  expect_match(out, "Columns: id, name, payload, flag")
  expect_match(out, "Not fetched (binary): payload, flag", fixed = TRUE)
})

test_that("a table of nothing but binary columns says so", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  DBI::dbExecute(conn, "CREATE TABLE blobs AS SELECT 'z'::BLOB AS b")
  d <- duckdt(conn, "blobs")
  expect_error(d[], "every column of 'blobs' is a binary type")
})

test_that("a table without binary columns still emits a plain SELECT *", {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  d <- as.duckdt(datasets::mtcars, conn = conn, name = "mt", copy = TRUE)
  expect_equal(duckdt_star(d), "*")
})

test_that("binary type names are classified per dialect", {
  expect_true(duckdt_is_binary_type("varbinary", "mssql"))
  expect_true(duckdt_is_binary_type("VARBINARY(MAX)", "mssql"))
  expect_true(duckdt_is_binary_type("image", "mssql"))
  # SQL Server's `timestamp` is rowversion, a binary(8) -- not a datetime.
  expect_true(duckdt_is_binary_type("timestamp", "mssql"))
  expect_false(duckdt_is_binary_type("bit", "mssql"))
  expect_false(duckdt_is_binary_type("datetime2", "mssql"))

  expect_true(duckdt_is_binary_type("BLOB", "duckdb"))
  expect_true(duckdt_is_binary_type("BIT", "duckdb"))
  # DuckDB's TIMESTAMP is a datetime, unlike SQL Server's.
  expect_false(duckdt_is_binary_type("TIMESTAMP", "duckdb"))
  expect_false(duckdt_is_binary_type("VARCHAR", "duckdb"))
})
