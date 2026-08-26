test_that("duckdt_dialect() detects duckdb connections", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(duckdt_dialect(con), "duckdb")
})

test_that("duckdt_dialect() detects an MS SQL Server connection by class", {
  # odbc::dbConnect() tags a live SQL Server connection with the S4 class
  # "Microsoft SQL Server" (the driver-reported DBMS name); fake that here
  # since no real SQL Server is available for testing.
  methods::setClass("Microsoft SQL Server", contains = "DBIConnection")
  con <- methods::new("Microsoft SQL Server")
  expect_equal(duckdt_dialect(con), "mssql")
})
