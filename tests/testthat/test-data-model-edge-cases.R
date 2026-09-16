test_that("disconnected tables are excluded from the complete query", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "connected", data.frame(id = 1L))
  DBI::dbWriteTable(con, "loose", data.frame(id = 2L))
  dm <- duckdt_data_model(con)
  sql <- duckdt_dm_query(dm, c("connected", "loose"))
  expect_identical(attr(sql, "unjoined"), "loose")
  expect_equal(DBI::dbGetQuery(con, sql), data.frame(id = 1L))
})

test_that("compound foreign keys pair corresponding columns once", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE parent (a INTEGER, b INTEGER, PRIMARY KEY (a,b))")
  DBI::dbExecute(con, paste0("CREATE TABLE child (x INTEGER, y INTEGER, ",
                            "FOREIGN KEY (x,y) REFERENCES parent(a,b))"))
  DBI::dbExecute(con, "INSERT INTO parent VALUES (1, 10), (1, 20)")
  DBI::dbExecute(con, "INSERT INTO child VALUES (1, 20)")
  refs <- duckdt_relationships(con)
  expect_equal(nrow(refs), 2L)
  expect_equal(refs$pk_column[match(c("x", "y"), refs$fk_column)], c("a", "b"))
  dm <- duckdt_data_model(con)
  sql <- duckdt_dm_query(dm, c("child", "parent"))
  expect_equal(DBI::dbGetQuery(con, sql), data.frame(x = 1L, y = 20L, a = 1L, b = 20L))
})

test_that("SQL Server relationships use exact catalog column pairs", {
  # Execute the catalog query against a fixture with reversed target
  # positions; matching columns by their table ordinal would be wrong.
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE SCHEMA sys")
  catalog <- list(
    schemas = data.frame(schema_id = 1L, name = "dbo"),
    tables = data.frame(object_id = c(10L, 20L), schema_id = 1L,
                        name = c("child", "parent")),
    columns = data.frame(object_id = c(10L, 10L, 20L, 20L),
                         column_id = c(1L, 2L, 1L, 2L), name = c("x", "y", "a", "b")),
    foreign_key_columns = data.frame(
      constraint_object_id = 30L, constraint_column_id = 1:2,
      parent_object_id = 10L, parent_column_id = 1:2,
      referenced_object_id = 20L, referenced_column_id = 2:1
    )
  )
  for (table in names(catalog)) {
    DBI::dbWriteTable(con, DBI::Id(schema = "sys", table = table), catalog[[table]])
  }
  local_mocked_bindings(duckdt_dialect = function(conn) "mssql")
  refs <- duckdt_relationships(con)
  expect_identical(refs$fk_column, c("x", "y"))
  expect_identical(refs$pk_column, c("b", "a"))
  expect_identical(refs$fk_schema, rep("dbo", 2L))
})
