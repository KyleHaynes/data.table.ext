# duckdt_create_temp() emits T-SQL (SELECT ... INTO) with no DuckDB
# equivalent, so -- like test-merge-mssql.R and test-mutate-mssql.R -- it is
# verified against a minimal recording fake DBI connection rather than a real
# SQL Server. duckdt_join_sql() is dialect-independent, but its SQL is
# checked here too since a fake connection is the only way to inspect the
# statement without running it.

make_fake_mssql_conn <- function(fail_pattern = NULL) {
  if (!methods::isClass("FakeMssqlConn")) {
    methods::setClass("FakeMssqlConn", contains = "DBIConnection", slots = c(log = "environment"))

    methods::setMethod(DBI::dbExecute, methods::signature("FakeMssqlConn", "character"), function(conn, statement, ...) {
      conn@log$calls <- c(conn@log$calls, statement)
      if (!is.null(conn@log$fail_pattern) && grepl(conn@log$fail_pattern, statement)) stop("boom")
      invisible(0L)
    })
    # The transaction methods aren't used here, but the class is shared with
    # test-merge-mssql.R / test-mutate-mssql.R, whose own definitions are
    # skipped once this one has registered the class -- so it has to carry
    # the full set whichever file runs first.
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

test_that("duckdt_create_temp() drops-then-SELECT-INTOs on mssql", {
  conn <- make_fake_mssql_conn()
  duckdt_create_temp(conn, "tmp_sub", 'SELECT * FROM "t" WHERE "a" = 1', "mssql")

  calls <- conn@log$calls
  expect_match(calls[1], "^IF OBJECT_ID\\('tmp_sub', 'U'\\) IS NOT NULL DROP TABLE")
  expect_match(calls[2], '^SELECT \\* INTO .*tmp_sub.* FROM \\(SELECT \\* FROM "t" WHERE "a" = 1\\) AS duckdt_src$')
})

test_that("duckdt_drop_table() emits a guarded DROP TABLE on mssql", {
  conn <- make_fake_mssql_conn()
  duckdt_drop_table(conn, "tmp_sub", "mssql")
  expect_match(conn@log$calls[1], "^IF OBJECT_ID\\('tmp_sub', 'U'\\) IS NOT NULL DROP TABLE")
})

test_that("duckdt_join_sql() renders an inner join with aliased, suffixed columns", {
  conn <- make_fake_mssql_conn()
  sql <- duckdt_join_sql(
    conn, '"xt"', '"yt"',
    x_cols = c("id", "v"), y_cols = c("id", "v", "w"),
    keys = list(x = "id", y = "id"),
    all.x = FALSE, all.y = FALSE, suffixes = c(".x", ".y"), sort = TRUE
  )

  expect_match(sql, '^SELECT duckdt_x\\."id" AS "id", duckdt_x\\."v" AS "v\\.x", duckdt_y\\."v" AS "v\\.y", duckdt_y\\."w" AS "w"')
  expect_match(sql, 'FROM "xt" AS duckdt_x INNER JOIN "yt" AS duckdt_y ON duckdt_x\\."id" = duckdt_y\\."id"')
  expect_match(sql, 'ORDER BY "id"$')
})

test_that("duckdt_join_sql() picks the join type from all.x/all.y", {
  conn <- make_fake_mssql_conn()
  render <- function(all.x, all.y) {
    duckdt_join_sql(conn, '"xt"', '"yt"', "id", "id", list(x = "id", y = "id"),
                    all.x = all.x, all.y = all.y, suffixes = c(".x", ".y"), sort = FALSE)
  }
  expect_match(render(FALSE, FALSE), "INNER JOIN")
  expect_match(render(TRUE, FALSE), "LEFT JOIN")
  expect_match(render(FALSE, TRUE), "RIGHT JOIN")
  expect_match(render(TRUE, TRUE), "FULL OUTER JOIN")
})

test_that("duckdt_join_sql() coalesces the key only when y-only rows are possible", {
  conn <- make_fake_mssql_conn()
  render <- function(all.y) {
    duckdt_join_sql(conn, '"xt"', '"yt"', "id", "key", list(x = "id", y = "key"),
                    all.x = FALSE, all.y = all.y, suffixes = c(".x", ".y"), sort = FALSE)
  }
  expect_match(render(TRUE), 'COALESCE\\(duckdt_x\\."id", duckdt_y\\."key"\\) AS "id"')
  expect_no_match(render(FALSE), "COALESCE")
})

test_that("duckdt_join_sql() omits ORDER BY when sort = FALSE", {
  conn <- make_fake_mssql_conn()
  sql <- duckdt_join_sql(conn, '"xt"', '"yt"', "id", "id", list(x = "id", y = "id"),
                         all.x = FALSE, all.y = FALSE, suffixes = c(".x", ".y"), sort = FALSE)
  expect_no_match(sql, "ORDER BY")
})
