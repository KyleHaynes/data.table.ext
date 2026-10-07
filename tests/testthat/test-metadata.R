# Handles look their table's columns up once, resolve schemas, and say what
# they send when asked to.

count_catalogue <- function(statements) {
  sum(grepl("DESCRIBE|information_schema|sys[.]", statements))
}

test_that("a handle looks its columns up once, and := refreshes them", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- as.dbdt(data.frame(id = 1:3, x = c(1, 2, 3)), conn = con, name = "t", copy = TRUE)

  statements <- character()
  query <- DBI::dbGetQuery
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    statements <<- c(statements, statement)
    query(conn, statement, ...)
  }, .package = "DBI")

  d[x > 1]
  d[, .(s = sum(x))]
  head(d)
  names(d)
  expect_lte(count_catalogue(statements), 1L)

  invisible(d[, y := x * 2])
  expect_true("y" %in% names(d))
  expect_equal(d[id == 2]$y, 4)
})

test_that("dbdt() reaches a table outside the default schema", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE SCHEMA sales")
  DBI::dbExecute(con, "CREATE TABLE sales.orders (id INTEGER PRIMARY KEY, total DOUBLE)")
  DBI::dbExecute(con, "INSERT INTO sales.orders VALUES (1, 10), (2, 20)")
  # A same-named table in the default schema must not get in the way.
  DBI::dbExecute(con, "CREATE TABLE orders (other VARCHAR)")

  d <- dbdt(con, "orders", schema = "sales", writable = TRUE)
  expect_identical(names(d), c("id", "total"))
  expect_equal(d[total > 15]$id, 2L)
  expect_output(print(d), "<duckdt> sales.orders [2 x 2]", fixed = TRUE)
  invisible(d[id == 1, total := 11])
  expect_equal(DBI::dbGetQuery(con, "SELECT total FROM sales.orders WHERE id = 1")$total, 11)
  expect_identical(names(dbdt(con, "orders")), "other")
  expect_error(dbdt(con, "orders", schema = "nope"), "does not exist")
  expect_error(dbdt(con, c("a", "b")), "single table")
})

test_that("dbdt_sql() shows the query `[` would run", {
  d <- as.dbdt(datasets::mtcars, name = "cars_sql")
  sql <- dbdt_sql(d, cyl == 6, .(n = .N), by = gear)
  expect_s4_class(sql, "SQL")
  # DuckDB quotes identifiers only where it has to; compare without quotes.
  unquote <- function(s) gsub('"', "", as.character(s), fixed = TRUE)
  expect_identical(
    unquote(sql),
    "SELECT gear, count(*) AS n FROM cars_sql WHERE cyl = 6 GROUP BY gear"
  )
  expect_identical(unquote(dbdt_sql(d)), "SELECT * FROM cars_sql")
  expect_error(dbdt_sql(d, , hp := 1), "writes rather than queries")
})

test_that("options(duckdt.trace = TRUE) reports each statement and its time", {
  d <- as.dbdt(datasets::mtcars, name = "cars_trace")
  old <- options(duckdt.trace = TRUE)
  on.exit(options(old))
  msgs <- character()
  withCallingHandlers(d[cyl == 6, .(n = .N)], message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  msgs <- gsub('"', "", msgs, fixed = TRUE)
  expect_true(any(grepl("^\\[duckdt +[0-9.]+s\\] SELECT count\\(\\*\\) AS n FROM cars_trace", msgs)))
  options(duckdt.trace = NULL)
  expect_silent(d[cyl == 6, .(n = .N)])
})

test_that("SQL Server lookups go to sys.* by OBJECT_ID, with a fallback", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_identical(
    duckdt_mssql_object_id(con, "orders", "sales"),
    "OBJECT_ID(N'[sales].[orders]')"
  )
  expect_identical(duckdt_mssql_object_id(con, "we]ird"), "OBJECT_ID(N'[we]]ird]')")
  expect_identical(duckdt_mssql_object_id(con, "#scratch"), "OBJECT_ID(N'tempdb..[#scratch]')")

  # The sys views don't exist on DuckDB: the portable lookups take over, so a
  # SQL Server connection whose catalogue can't be read still works.
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER, payload BLOB)")
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  d <- dbdt(con, "t")
  expect_true(d$materialized)
  expect_identical(names(d), c("id", "payload"))
  expect_true(is.na(duckdt_approx_rows(d)))
})

test_that("SQL Server print() shows the catalogue's row count, marked approximate", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- as.dbdt(data.frame(id = 1:3), conn = con, name = "big", copy = TRUE)
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  query <- DBI::dbGetQuery
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    if (grepl("sys.partitions", statement, fixed = TRUE)) return(data.frame(n = 1234567))
    if (grepl("count(*)", statement, fixed = TRUE)) stop("no full count expected")
    if (grepl("SELECT TOP", statement, fixed = TRUE)) return(data.frame(id = 1:3))
    query(conn, statement, ...)
  }, .package = "DBI")
  expect_output(print(d), "[~1,234,567 x 1]", fixed = TRUE)
})

test_that("SQL Server dim() reads partition metadata instead of counting", {
  # VS Code's R session watcher calls dim() on every workspace object after
  # each command, so a count(*) here scanned the table every time.
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- as.dbdt(data.frame(id = 1:3, x = 4:6), conn = con, name = "big", copy = TRUE)
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  query <- DBI::dbGetQuery
  partitions <- data.frame(n = 1234567)
  counted <- 0L
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    if (grepl("sys.partitions", statement, fixed = TRUE)) return(partitions)
    if (grepl("count(*)", statement, fixed = TRUE)) counted <<- counted + 1L
    if (grepl("OFFSET", statement, fixed = TRUE)) return(data.frame(id = 3L, x = 6L))
    query(conn, statement, ...)
  }, .package = "DBI")
  expect_equal(dim(d), c(1234567, 2))
  expect_equal(nrow(d), 1234567)
  expect_equal(ncol(d), 2L)
  expect_identical(counted, 0L)

  # A view has no partition metadata: NA rather than a scan.
  partitions <- data.frame(n = NA_real_)
  expect_equal(dim(d), c(NA, 2))
  expect_identical(counted, 0L)

  # Without a clustered index (this stand-in catalogue has none) tail() needs
  # the exact count to compute its offset, so it still counts.
  tail(d, 1)
  expect_identical(counted, 1L)
})

test_that("SQL Server data model reads row counts from partition metadata", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE a (id INTEGER)")
  DBI::dbExecute(con, "CREATE VIEW v AS SELECT * FROM a")
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  query <- DBI::dbGetQuery
  counted <- character()
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    if (grepl("sys.partitions", statement, fixed = TRUE)) {
      return(data.frame(table_schema = "main", table_name = "a", n = 42))
    }
    if (grepl("count(*)", statement, fixed = TRUE)) counted <<- c(counted, statement)
    query(conn, statement, ...)
  }, .package = "DBI")
  dm <- dbdt_data_model(con, row_counts = TRUE)
  expect_equal(dm$tables$n_rows[dm$tables$name == "a"], 42)
  # The view has no partitions, and counting it would run its query on the
  # server, so it is left uncounted.
  expect_true(is.na(dm$tables$n_rows[dm$tables$name == "v"]))
  expect_length(counted, 0L)
})

test_that("dbdt_temp() on SQL Server won't replace a table it didn't make", {
  # on.exit() first: called after them, it would replace the handler that
  # undoes the mocks.
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  local_mocked_bindings(duckdt_mssql_exists = function(conn, name) TRUE)
  expect_error(
    duckdt_create_temp(con, "orders", "SELECT 1 AS x", "mssql"),
    "already exists, and this session did not create it"
  )
})

test_that("staging names are random and never derived from the target table", {
  a <- duckdt_staging_name("merge")
  b <- duckdt_staging_name("merge")
  expect_match(a, "^duckdt_stage_merge_[0-9a-f]+$")
  expect_false(identical(a, b))
})
