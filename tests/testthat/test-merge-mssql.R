# duckdt_merge_exec_mssql()/duckdt_merge_stage() emit T-SQL (native MERGE,
# SELECT ... INTO) with no DuckDB equivalent, so -- like
# duckdt_rebuild_table_mssql() in test-mutate-mssql.R -- they're verified
# against a minimal recording fake DBI connection rather than a real SQL
# Server.

make_fake_mssql_conn <- function(fail_pattern = NULL) {
  if (!methods::isClass("FakeMssqlConn")) {
    methods::setClass("FakeMssqlConn", contains = "DBIConnection", slots = c(log = "environment"))

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

test_that("duckdt_merge_exec_mssql() emits a single native MERGE with all three clauses", {
  conn <- make_fake_mssql_conn()
  x <- list(conn = conn, tbl = "mytbl")

  duckdt_merge_exec_mssql(x, "mytbl__duckdt_merge_tmp", by = "id", update_cols = c("a", "b"),
                           update = TRUE, insert = TRUE, delete = TRUE)

  sql <- conn@log$calls[1]
  expect_match(sql, "^MERGE INTO \"mytbl\" AS t USING .*mytbl__duckdt_merge_tmp.* AS s ON \\(t\\.\"id\" = s\\.\"id\"\\)")
  expect_match(sql, "WHEN MATCHED THEN UPDATE SET t\\.\"a\" = s\\.\"a\", t\\.\"b\" = s\\.\"b\"")
  expect_match(sql, "WHEN NOT MATCHED BY TARGET THEN INSERT \\(\"id\", \"a\", \"b\"\\) VALUES \\(s\\.\"id\", s\\.\"a\", s\\.\"b\"\\)")
  expect_match(sql, "WHEN NOT MATCHED BY SOURCE THEN DELETE")
  expect_match(sql, ";$")
})

test_that("duckdt_merge_exec_mssql() omits clauses for update = FALSE / insert = FALSE / delete = FALSE", {
  conn <- make_fake_mssql_conn()
  x <- list(conn = conn, tbl = "mytbl")

  duckdt_merge_exec_mssql(x, "tmp", by = "id", update_cols = c("a"),
                           update = FALSE, insert = TRUE, delete = FALSE)

  sql <- conn@log$calls[1]
  expect_false(grepl("WHEN MATCHED THEN UPDATE", sql))
  expect_true(grepl("WHEN NOT MATCHED BY TARGET THEN INSERT", sql))
  expect_false(grepl("WHEN NOT MATCHED BY SOURCE THEN DELETE", sql))
})

test_that("duckdt_merge_exec_mssql() skips the UPDATE clause when there are no shared non-key columns", {
  conn <- make_fake_mssql_conn()
  x <- list(conn = conn, tbl = "mytbl")

  duckdt_merge_exec_mssql(x, "tmp", by = "id", update_cols = character(0),
                           update = TRUE, insert = TRUE, delete = FALSE)

  sql <- conn@log$calls[1]
  expect_false(grepl("WHEN MATCHED", sql))
  expect_match(sql, "INSERT \\(\"id\"\\) VALUES \\(s\\.\"id\"\\)")
})

test_that("duckdt_merge_stage() drops-then-SELECT-INTOs a duckdt source on mssql", {
  conn <- make_fake_mssql_conn()
  y <- list(conn = conn, tbl = "src_tbl")
  class(y) <- "duckdt"

  duckdt_merge_stage(conn, y, y_is_duckdt = TRUE, cols = c("id", "a"), tmp_name = "mytbl__duckdt_merge_tmp", dialect = "mssql")

  calls <- conn@log$calls
  expect_match(calls[1], "^IF OBJECT_ID\\('mytbl__duckdt_merge_tmp', 'U'\\) IS NOT NULL DROP TABLE")
  expect_match(calls[2], '^SELECT "id", "a" INTO .*mytbl__duckdt_merge_tmp.* FROM "src_tbl"$')
})

test_that("duckdt_merge_drop_tmp() emits a guarded DROP TABLE on mssql", {
  conn <- make_fake_mssql_conn()
  duckdt_merge_drop_tmp(conn, "mytbl__duckdt_merge_tmp", "mssql")
  expect_match(conn@log$calls[1], "^IF OBJECT_ID\\('mytbl__duckdt_merge_tmp', 'U'\\) IS NOT NULL DROP TABLE")
})

test_that("duckdt_merge_exec_duckdb() wraps UPDATE/INSERT/DELETE in a transaction and rolls back on error", {
  conn <- make_fake_mssql_conn(fail_pattern = "^DELETE")
  x <- list(conn = conn, tbl = "mytbl")

  expect_error(
    duckdt_merge_exec_duckdb(x, "tmp", by = "id", update_cols = c("a"),
                              update = TRUE, insert = TRUE, delete = TRUE),
    "boom"
  )
  calls <- conn@log$calls
  expect_equal(calls[1], "BEGIN")
  expect_match(calls[2], "^UPDATE")
  expect_match(calls[3], "^INSERT INTO")
  expect_match(calls[4], "^DELETE FROM")
  expect_true("ROLLBACK" %in% calls)
  expect_false("COMMIT" %in% calls)
})
