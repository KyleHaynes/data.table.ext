# These tests force dialect = "mssql" through the translators directly,
# using the package's in-memory duckdb test connection purely to get a
# working DBI connection for dbQuoteIdentifier()/dbQuoteString() (no real
# SQL Server is available in this environment). Identifier/string quoting
# characters here reflect duckdb's ANSI quoting rather than a real
# odbc/SQL-Server connection's bracket quoting -- that's cosmetic. What's
# under test is the generated SQL *shape*: function names, literal forms,
# and clause syntax that are specific to the mssql dialect.

test_that("logical literals render as 1/0 for mssql, TRUE/FALSE for duckdb", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_equal(translate_literal(TRUE, con, "mssql"), "1")
  expect_equal(translate_literal(FALSE, con, "mssql"), "0")
  expect_equal(translate_literal(TRUE, con, "duckdb"), "TRUE")
  expect_equal(translate_literal(FALSE, con, "duckdb"), "FALSE")
})

test_that("mssql FN_MAP renames functions that differ from duckdb", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cols <- c("name", "hp")
  env <- environment()

  qname <- as.character(DBI::dbQuoteIdentifier(con, "name"))
  qhp <- as.character(DBI::dbQuoteIdentifier(con, "hp"))

  expect_equal(
    translate_expr(quote(nchar(name)), cols, env, con, "mssql"),
    paste0("len(", qname, ")")
  )
  expect_equal(
    translate_expr(quote(sd(hp)), cols, env, con, "mssql"),
    paste0("stdev(", qhp, ")")
  )
  expect_equal(
    translate_expr(quote(var(hp)), cols, env, con, "mssql"),
    paste0("var(", qhp, ")")
  )
})

test_that("median() is a window in a subquery for mssql, median() for duckdb", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cols <- c("hp")
  env <- environment()
  qhp <- as.character(DBI::dbQuoteIdentifier(con, "hp"))

  expect_equal(
    translate_expr(quote(median(hp)), cols, env, con, "duckdb"),
    paste0("median(", qhp, ")")
  )
  # T-SQL's PERCENTILE_CONT is only a window function, so outside a `j` that
  # can host the subquery there is nothing valid to emit.
  expect_error(translate_expr(quote(median(hp)), cols, env, con, "mssql"), "only be used in `j`")

  d <- as.dbdt(data.frame(g = c(1, 1, 2), hp = c(1, 3, 5)), conn = con, name = "med", copy = TRUE)
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  sql <- as.character(dbdt_sql(d, hp > 0, .(m = median(hp), n = .N), by = g))
  # DuckDB quotes identifiers only where it has to; compare without quotes.
  expect_identical(gsub('"', "", sql, fixed = TRUE), gsub('"', "", paste0(
    'SELECT "g", MAX("duckdt_median_1") AS "m", count(*) AS "n" ',
    'FROM (SELECT *, PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY "hp") ',
    'OVER (PARTITION BY "g") AS "duckdt_median_1" FROM "med" WHERE "hp" > 0) ',
    'AS duckdt_w GROUP BY "g"'
  ), fixed = TRUE))
  # No `by`: the window spans every row the WHERE clause keeps.
  sql <- as.character(dbdt_sql(d, , .(m = median(hp))))
  expect_match(sql, "OVER () AS", fixed = TRUE)
  expect_false(grepl("GROUP BY", sql))
})

test_that("%flike% uses CHARINDEX for mssql, contains() for duckdb", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cols <- c("name")
  env <- environment()
  qname <- as.character(DBI::dbQuoteIdentifier(con, "name"))
  qpat <- as.character(DBI::dbQuoteString(con, "ab"))

  expect_equal(
    translate_expr(quote(name %flike% "ab"), cols, env, con, "mssql"),
    paste0("CHARINDEX(", qpat, ", ", qname, ") > 0")
  )
  expect_equal(
    translate_expr(quote(name %flike% "ab"), cols, env, con, "duckdb"),
    paste0("contains(", qname, ", ", qpat, ")")
  )
})

test_that("%like%/%ilike%/%plike% use REGEXP_LIKE() for mssql, regexp_matches() for duckdb", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cols <- c("name")
  env <- environment()
  qname <- as.character(DBI::dbQuoteIdentifier(con, "name"))
  qpat <- as.character(DBI::dbQuoteString(con, "a.*"))
  qi <- as.character(DBI::dbQuoteString(con, "i"))

  expect_equal(
    translate_expr(quote(name %like% "a.*"), cols, env, con, "mssql"),
    paste0("REGEXP_LIKE(", qname, ", ", qpat, ")")
  )
  expect_equal(
    translate_expr(quote(name %ilike% "a.*"), cols, env, con, "mssql"),
    paste0("REGEXP_LIKE(", qname, ", ", qpat, ", ", qi, ")")
  )
  expect_equal(
    translate_expr(quote(name %plike% "a.*"), cols, env, con, "mssql"),
    paste0("REGEXP_LIKE(", qname, ", ", qpat, ")")
  )
  expect_equal(
    translate_expr(quote(name %like% "a.*"), cols, env, con, "duckdb"),
    paste0("regexp_matches(", qname, ", ", qpat, ")")
  )
})
