test_that("catalogue queries avoid reserved aliases and filter tables on the server", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE parent (id INTEGER PRIMARY KEY)")
  DBI::dbExecute(con, "CREATE TABLE other_table (value INTEGER)")
  query <- DBI::dbGetQuery
  statements <- character()
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    # Reproduce SQL Server rejecting an unquoted SCHEMA alias.
    if (grepl("AS schema\\b", statement, ignore.case = TRUE)) {
      stop("Incorrect syntax near the keyword 'schema'.")
    }
    statements <<- c(statements, statement)
    out <- query(conn, statement, ...)
    # SQL Server drivers can preserve the catalogue's uppercase names.
    names(out) <- toupper(names(out))
    out
  }, .package = "DBI")

  expect_setequal(dbdt_tables(con)$name, c("parent", "other_table"))
  expect_identical(names(dbdt_tables(con)), c("schema", "name", "type"))
  statements <- character()
  out <- dbdt_schema(con, "parent")
  expect_identical(out$table, "parent")
  expect_identical(out$primary_key, TRUE)
  expect_length(statements, 2L)
  expect_true(all(grepl("table_name = 'parent'", statements, fixed = TRUE)))
  expect_equal(nrow(dbdt_schema(con, "missing' OR 1=1 --")), 0L)
  expect_error(dbdt_schema(con, NA_character_), "single table name")
})

test_that("fast SQL Server sampling samples pages before random ordering", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- as.dbdt(data.frame(id = 1:3), conn = con, copy = TRUE)
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  statements <- character()
  query <- DBI::dbGetQuery
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    if (grepl("information_schema", statement, fixed = TRUE)) {
      return(query(conn, statement, ...))
    }
    statements <<- c(statements, statement)
    data.frame(id = integer())
  }, .package = "DBI")

  out <- dbdt_sample(d, 10, method = "fast")
  expect_s3_class(out, "data.table")
  expect_identical(out$id, integer())
  expect_length(statements, 1L)
  expect_match(statements, 'SELECT TOP (10)', fixed = TRUE)
  expect_match(statements, 'TABLESAMPLE SYSTEM (10 ROWS) ORDER BY NEWID()', fixed = TRUE)
  expect_false(grepl("COUNT", statements, ignore.case = TRUE))

  d$materialized <- FALSE
  expect_error(dbdt_sample(d, 10, method = "fast"), "base table")
  expect_equal(nrow(dbdt_sample(d, 0, method = "fast")), 0L)
  expect_false(grepl("TABLESAMPLE", tail(statements, 1), fixed = TRUE))
  dbdt_sample(d, 2)
  expect_match(tail(statements, 1), "ORDER BY NEWID()", fixed = TRUE)
  expect_false(grepl("TABLESAMPLE", tail(statements, 1), fixed = TRUE))
})

test_that("SQL Server previews avoid counting rows unless requested", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- as.dbdt(data.frame(id = 1:3), conn = con, copy = TRUE)
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  statements <- character()
  query <- DBI::dbGetQuery
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    statements <<- c(statements, statement)
    if (grepl("SELECT TOP", statement, fixed = TRUE)) return(data.frame(id = 1:3))
    query(conn, statement, ...)
  }, .package = "DBI")

  expect_output(print(d), "[? x 1]", fixed = TRUE)
  expect_false(any(grepl("count(*)", statements, fixed = TRUE)))
  expect_output(print(d, count = TRUE), "[3 x 1]", fixed = TRUE)
  expect_true(any(grepl("count(*)", statements, fixed = TRUE)))
  expect_equal(nrow(d), 3)
})

test_that("sampling validates row counts and handles zero and empty results", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- as.dbdt(data.frame(id = 1:3), conn = con, copy = TRUE)
  for (bad in list(NULL, NA, NA_real_, Inf, -1, 1.5, "2", c(1, 2))) {
    expect_error(dbdt_sample(d, bad), "non-negative finite whole number")
  }
  expect_identical(dbdt_sample(d, 0)$id, integer())
  expect_equal(nrow(dbdt_sample(d, 10, method = "fast")), 3L)
  DBI::dbExecute(con, paste0("DELETE FROM ", DBI::dbQuoteIdentifier(con, d$tbl)))
  expect_identical(dbdt_sample(d, 5)$id, integer())
})
