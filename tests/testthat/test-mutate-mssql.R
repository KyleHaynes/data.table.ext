# duckdt_rebuild_table_mssql() emits T-SQL with no DuckDB equivalent
# (SELECT ... INTO / sp_rename), so it can't be executed against the
# in-memory duckdb test connection. Instead, verify it against a minimal
# recording fake DBI connection that just logs the SQL text/statement
# sequence instead of executing it -- this checks the exact SQL shape and
# transaction wrapping without needing a real SQL Server.

make_fake_mssql_conn <- function(fail_pattern = NULL) {
  if (!methods::isClass("FakeMssqlConn")) {
    methods::setClass("FakeMssqlConn", contains = "DBIConnection", slots = c(log = "environment"))

    # Pass the actual DBI generic *function objects* (not bare names) to
    # setMethod(): duckdt never `library(DBI)`s (it only ever calls
    # `DBI::fn()`), so an unqualified "dbExecute" isn't a visible generic to
    # look up by name from here.
    methods::setMethod(DBI::dbExecute, methods::signature("FakeMssqlConn", "character"), function(conn, statement, ...) {
      conn@log$calls <- c(conn@log$calls, statement)
      if (!is.null(conn@log$fail_pattern) && grepl(conn@log$fail_pattern, statement)) stop("boom")
      invisible(0L)
    })
    methods::setMethod(DBI::dbBegin, "FakeMssqlConn", function(conn, ...) {
      conn@log$calls <- c(conn@log$calls, "BEGIN")
      invisible(TRUE)
    })
    methods::setMethod(DBI::dbCommit, "FakeMssqlConn", function(conn, ...) {
      conn@log$calls <- c(conn@log$calls, "COMMIT")
      invisible(TRUE)
    })
    methods::setMethod(DBI::dbRollback, "FakeMssqlConn", function(conn, ...) {
      conn@log$calls <- c(conn@log$calls, "ROLLBACK")
      invisible(TRUE)
    })
  }
  log_env <- new.env()
  log_env$fail_pattern <- fail_pattern
  methods::new("FakeMssqlConn", log = log_env)
}

test_that("duckdt_rebuild_table_mssql() adds a new column via SELECT INTO + sp_rename, in a transaction", {
  conn <- make_fake_mssql_conn()
  x <- list(conn = conn, tbl = "mytbl")
  cols <- c("a", "b")
  qcol <- DBI::dbQuoteIdentifier(conn, "c")

  duckdt_rebuild_table_mssql(x, cols, col_name = "c", col_exists = FALSE, new_val = "a + 1", qcol = qcol)

  calls <- conn@log$calls
  expect_equal(calls[1], "BEGIN")
  expect_match(calls[2], "^IF OBJECT_ID\\('mytbl__duckdt_tmp', 'U'\\) IS NOT NULL DROP TABLE")
  expect_match(calls[3], "^SELECT .*a \\+ 1 AS \"c\" INTO .*mytbl__duckdt_tmp.* FROM \"mytbl\"$")
  expect_match(calls[3], '"a"')
  expect_match(calls[3], '"b"')
  expect_equal(calls[4], "DROP TABLE \"mytbl\"")
  expect_match(calls[5], "^EXEC sp_rename 'mytbl__duckdt_tmp', 'mytbl'$")
  expect_equal(calls[6], "COMMIT")
})

test_that("duckdt_rebuild_table_mssql() updating an existing column excludes it from the carried-over list", {
  conn <- make_fake_mssql_conn()
  x <- list(conn = conn, tbl = "mytbl")
  cols <- c("a", "b", "c")
  qcol <- DBI::dbQuoteIdentifier(conn, "b")

  duckdt_rebuild_table_mssql(x, cols, col_name = "b", col_exists = TRUE, new_val = "b * 2", qcol = qcol)

  select_stmt <- conn@log$calls[3]
  expect_match(select_stmt, "^SELECT ")
  expect_match(select_stmt, '"a"')
  expect_match(select_stmt, '"c"')
  expect_match(select_stmt, 'b \\* 2 AS "b"')
  # only one occurrence of the bare identifier "b" as a carried-over column
  # (the CASE/expr text "b * 2" also contains a "b", so just check the tail)
  expect_match(select_stmt, 'AS "b" INTO')
})

test_that("duckdt_rebuild_table_mssql() rolls back and re-raises on error", {
  conn <- make_fake_mssql_conn(fail_pattern = "^SELECT")
  x <- list(conn = conn, tbl = "mytbl")

  expect_error(
    duckdt_rebuild_table_mssql(x, c("a"), col_name = "c", col_exists = FALSE, new_val = "1", qcol = DBI::dbQuoteIdentifier(conn, "c")),
    "boom"
  )
  expect_true("ROLLBACK" %in% conn@log$calls)
  expect_false("COMMIT" %in% conn@log$calls)
})
