test_that("duckdt_tables() lists tables and views, excluding internal schemas", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE t1 (id INTEGER)")
  DBI::dbExecute(con, "CREATE VIEW v1 AS SELECT * FROM t1")

  out <- duckdt_tables(con)
  expect_s3_class(out, "data.table")
  expect_setequal(names(out), c("schema", "name", "type"))
  expect_true(all(c("t1", "v1") %in% out$name))
  expect_equal(out$type[out$name == "t1"], "BASE TABLE")
  expect_equal(out$type[out$name == "v1"], "VIEW")
  expect_false(any(out$schema %in% c("information_schema", "pg_catalog")))
})

test_that("duckdt_tables() accepts a duckdt object directly", {
  d <- mtcars_dt(copy = TRUE)
  out <- duckdt_tables(d)
  expect_true("mtcars_test" %in% out$name)
})

test_that("duckdt_schema() lists columns with type and primary_key flag, optionally filtered", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE parent (id INTEGER PRIMARY KEY, name VARCHAR)")
  DBI::dbExecute(con, "CREATE TABLE child (id INTEGER PRIMARY KEY, parent_id INTEGER)")

  out <- duckdt_schema(con)
  expect_setequal(names(out), c("schema", "table", "column", "type", "ordinal_position", "primary_key"))
  expect_true(all(c("parent", "child") %in% out$table))
  expect_true(out$primary_key[out$table == "parent" & out$column == "id"])
  expect_false(out$primary_key[out$table == "parent" & out$column == "name"])

  only_parent <- duckdt_schema(con, table = "parent")
  expect_true(all(only_parent$table == "parent"))
  expect_equal(sort(only_parent$column), c("id", "name"))
})

test_that("duckdt_relationships() detects declared foreign keys and returns zero rows otherwise", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE parent (id INTEGER PRIMARY KEY)")
  DBI::dbExecute(con, "
    CREATE TABLE child (id INTEGER PRIMARY KEY, parent_id INTEGER, FOREIGN KEY (parent_id) REFERENCES parent(id))
  ")

  out <- duckdt_relationships(con)
  expect_equal(nrow(out), 1L)
  expect_equal(out$fk_table, "child")
  expect_equal(out$fk_column, "parent_id")
  expect_equal(out$pk_table, "parent")
  expect_equal(out$pk_column, "id")

  con2 <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con2, "CREATE TABLE lonely (id INTEGER)")
  expect_equal(nrow(duckdt_relationships(con2)), 0L)
})
