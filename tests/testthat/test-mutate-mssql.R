# `:=` on SQL Server emits T-SQL with no DuckDB equivalent
# (sys.dm_exec_describe_first_result_set(), ALTER TABLE ... ADD without
# COLUMN), so it can't be executed against the in-memory duckdb test
# connection. Instead, verify it against a minimal recording fake DBI
# connection that logs each statement instead of executing it -- this checks
# the exact SQL shape and transaction wrapping without needing a real SQL
# Server.

make_fake_mssql_conn <- function(fail_pattern = NULL, type = "int") {
  if (!methods::isClass("FakeMssqlConn")) {
    methods::setClass("FakeMssqlConn", contains = "DBIConnection", slots = c(log = "environment"))
  }
  # Pass the actual DBI generic *function objects* (not bare names) to
  # setMethod(): duckdt never `library(DBI)`s (it only ever calls
  # `DBI::fn()`), so an unqualified "dbExecute" isn't a visible generic to
  # look up by name from here.
  methods::setMethod(DBI::dbExecute, methods::signature("FakeMssqlConn", "character"), function(conn, statement, ...) {
    conn@log$calls <- c(conn@log$calls, statement)
    if (!is.null(conn@log$fail_pattern) && grepl(conn@log$fail_pattern, statement)) stop("boom")
    invisible(0L)
  })
  # `:=` asks for the type of a new column's value, and afterwards the handle
  # looks its columns up again. Catalogue lookups aren't logged: they aren't
  # part of the write.
  methods::setMethod(DBI::dbGetQuery, methods::signature("FakeMssqlConn", "character"), function(conn, statement, ...) {
    if (grepl("sys.columns", statement, fixed = TRUE)) {
      return(data.frame(column_name = c("a", "b"), data_type = "int"))
    }
    conn@log$calls <- c(conn@log$calls, statement)
    data.frame(system_type_name = conn@log$type)
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
  log_env <- new.env()
  log_env$fail_pattern <- fail_pattern
  log_env$type <- type
  methods::new("FakeMssqlConn", log = log_env)
}

fake_mssql_handle <- function(conn, cols = c("a", "b")) {
  x <- duckdt_handle(conn, "mytbl", materialized = TRUE, writable = TRUE)
  x$meta$types <- data.frame(column = cols, type = "int")
  x
}

test_that(":= on SQL Server adds a new column with ALTER TABLE, then UPDATE, in a transaction", {
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  conn <- make_fake_mssql_conn(type = "int")
  x <- fake_mssql_handle(conn)

  invisible(x[, c := a + 1L])

  calls <- conn@log$calls
  expect_equal(calls[1], "BEGIN")
  expect_match(calls[2], "sys.dm_exec_describe_first_result_set(N'SELECT \"a\" + 1 AS duckdt_value FROM \"mytbl\"', NULL, 0)",
    fixed = TRUE)
  expect_equal(calls[3], 'ALTER TABLE "mytbl" ADD "c" int NULL')
  expect_equal(calls[4], 'UPDATE "mytbl" SET "c" = "a" + 1')
  expect_equal(calls[5], "COMMIT")
  # Nothing copies, drops or renames the table any more.
  expect_false(any(grepl("INTO|DROP|sp_rename", calls)))
})

test_that(":= on SQL Server updates an existing column in place, with `i` as the WHERE clause", {
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  conn <- make_fake_mssql_conn()
  x <- fake_mssql_handle(conn)

  invisible(x[a > 5, b := b * 2L])

  expect_equal(conn@log$calls, c("BEGIN", 'UPDATE "mytbl" SET "b" = "b" * 2 WHERE "a" > 5', "COMMIT"))
})

test_that(":= on SQL Server gives literal values R's type, and widens text", {
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  conn <- make_fake_mssql_conn(type = "varchar(2)")
  x <- fake_mssql_handle(conn)

  # A literal never asks the server: "no" would describe as varchar(2), too
  # narrow for a later "yes"; 0.5 as numeric(1,1), too narrow for 12.25.
  invisible(x[, flag := "no"])
  invisible(x[, rate := 0.5])
  invisible(x[, ok := TRUE])
  calls <- conn@log$calls
  expect_false(any(grepl("describe_first_result_set", calls)))
  expect_true('ALTER TABLE "mytbl" ADD "flag" nvarchar(4000) NULL' %in% calls)
  expect_true('ALTER TABLE "mytbl" ADD "rate" float NULL' %in% calls)
  expect_true('ALTER TABLE "mytbl" ADD "ok" bit NULL' %in% calls)

  # A computed text value is widened from the width the server describes.
  conn2 <-make_fake_mssql_conn(type = "varchar(51)")
  x2 <- fake_mssql_handle(conn2)
  invisible(x2[, code := paste0("x", a)])
  expect_true('ALTER TABLE "mytbl" ADD "code" varchar(8000) NULL' %in% conn2@log$calls)
  expect_equal(duckdt_mssql_widen("nvarchar(10)"), "nvarchar(4000)")
  expect_equal(duckdt_mssql_widen("varchar(max)"), "varchar(max)")
  expect_equal(duckdt_mssql_widen("decimal(10,2)"), "decimal(10,2)")
})

test_that(":= on SQL Server turns a comparison into a bit value", {
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  conn <- make_fake_mssql_conn(type = "bit")
  x <- fake_mssql_handle(conn)

  invisible(x[, big := a > 10])
  expect_true(paste0(
    'UPDATE "mytbl" SET "big" = CAST(CASE WHEN "a" > 10 THEN 1 ',
    'WHEN NOT ("a" > 10) THEN 0 END AS BIT)'
  ) %in% conn@log$calls)
})

test_that(":= NULL drops the column", {
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  conn <- make_fake_mssql_conn()
  x <- fake_mssql_handle(conn)

  invisible(x[, b := NULL])
  expect_equal(conn@log$calls, c("BEGIN", 'ALTER TABLE "mytbl" DROP COLUMN "b"', "COMMIT"))
  expect_error(x[a > 1, b := NULL], "can't be combined with `i`")
  expect_error(x[, zz := NULL], "no such column")
})

test_that(":= on SQL Server rolls back every assignment and re-raises on error", {
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  conn <- make_fake_mssql_conn(fail_pattern = "^UPDATE \"mytbl\" SET \"d\"")
  x <- fake_mssql_handle(conn)

  expect_error(x[, `:=`(c = a + 1L, d = b + 1L)], "boom")
  expect_true("ROLLBACK" %in% conn@log$calls)
  expect_false("COMMIT" %in% conn@log$calls)
  # The first assignment ran inside the same transaction that was rolled back.
  expect_true(which(conn@log$calls == "BEGIN") < which(grepl('SET "c"', conn@log$calls)))
})

test_that(":= writes nothing when a later assignment can't be translated", {
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  conn <- make_fake_mssql_conn()
  x <- fake_mssql_handle(conn)

  expect_error(x[, `:=`(c = a + 1L, d = no_such_thing)], "could not resolve")
  expect_length(conn@log$calls, 0L)
})
