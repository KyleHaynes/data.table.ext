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
    # Catalogue lookups go to the real (DuckDB) connection, where the SQL
    # Server catalogue views don't exist, so duckdt falls back to the
    # portable ones.
    if (grepl("information_schema|sys[.]", statement)) {
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

  # nrow() reads the same metadata as print(), which this stand-in catalogue
  # doesn't have, so NA without a scan; d[, .N] is the exact count.
  counts <- sum(grepl("count(*)", statements, fixed = TRUE))
  expect_true(is.na(nrow(d)))
  expect_identical(sum(grepl("count(*)", statements, fixed = TRUE)), counts)
  expect_equal(d[, .N]$N, 3)
})

test_that("SQL Server tail() reads a clustered index backwards instead of counting", {
  # The count(*) and OFFSET each scanned the table: on ~300M rows, tail()
  # took minutes where head() took milliseconds.
  d <- duckdt_handle(ansi_conn(), "big")
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  # Upper-case names, as some SQL Server drivers return them.
  key <- data.frame(COLUMN_NAME = c("grp", "id"), DESCENDING = c(0L, 1L))
  lookups <- 0L
  statements <- character()
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    if (grepl("sys.indexes", statement, fixed = TRUE)) {
      lookups <<- lookups + 1L
      return(key)
    }
    if (grepl("sys.columns", statement, fixed = TRUE)) {
      return(data.frame(column_name = c("grp", "id"), data_type = "int"))
    }
    statements <<- c(statements, statement)
    if (grepl("count(*)", statement, fixed = TRUE)) return(data.frame(n = 3))
    if (grepl("TOP (0)", statement, fixed = TRUE)) return(data.frame(grp = integer(), id = integer()))
    data.frame(grp = c(1L, 1L), id = c(3L, 2L))
  }, .package = "DBI")

  out <- tail(d, 2)
  expect_identical(statements, 'SELECT TOP (2) * FROM "big" ORDER BY "grp" DESC, "id" ASC')
  # Back in index order, the way head() returns rows.
  expect_identical(out$id, c(2L, 3L))
  expect_true(withVisible(tail(d, 2))$visible)
  expect_identical(lookups, 1L)
  expect_equal(nrow(tail(d, 0)), 0L)

  # A negative n is most of the table anyway, so it still counts.
  statements <- character()
  tail(d, -1)
  expect_match(statements[1], "count(*)", fixed = TRUE)

  # A heap, or a clustered columnstore index, has no key to read backwards.
  d <- duckdt_handle(ansi_conn(), "heap")
  key <- data.frame(COLUMN_NAME = character(), DESCENDING = integer())
  statements <- character()
  tail(d, 2)
  expect_match(statements[1], "count(*)", fixed = TRUE)
  expect_match(statements[2], "OFFSET 1 ROWS FETCH NEXT 2 ROWS ONLY", fixed = TRUE)

  # A view is never looked up.
  lookups <- 0L
  d <- duckdt_handle(ansi_conn(), "v", materialized = FALSE)
  tail(d, 2)
  expect_identical(lookups, 0L)
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
