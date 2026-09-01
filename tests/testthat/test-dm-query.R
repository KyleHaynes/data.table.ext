shop_query_dm <- function() {
  dm <- duckdt_data_model(list(
    orders = data.frame(id = 1L, customer_id = 1L, total = 1),
    customers = data.frame(id = 1L, name = "a"),
    loose = data.frame(x = 1)
  ))
  duckdt_dm_add_references(dm, orders$customer_id == customers$id)
}

test_that("duckdt_dm_query() joins along the model's references", {
  sql <- duckdt_dm_query(shop_query_dm(), c("orders", "customers"))
  expect_match(sql, 'FROM "orders" AS o')
  expect_match(sql, 'LEFT JOIN "customers" AS c')
  expect_match(sql, 'ON o."customer_id" = c."id"')
  # `id` exists in both tables, so it is aliased apart.
  expect_match(sql, 'o."id" AS "orders_id"', fixed = TRUE)
  expect_match(sql, 'c."id" AS "customers_id"', fixed = TRUE)
  expect_equal(attr(sql, "unjoined"), character())
})

test_that("duckdt_dm_query() selects only the columns asked for", {
  sql <- duckdt_dm_query(
    shop_query_dm(), c("orders", "customers"),
    columns = list(orders = "total", customers = "name")
  )
  expect_match(sql, 'o."total"', fixed = TRUE)
  expect_match(sql, 'c."name"', fixed = TRUE)
  # The join key is still needed in ON, just not selected.
  expect_false(grepl('o."customer_id",', sql, fixed = TRUE))
  expect_match(sql, 'ON o."customer_id" = c."id"', fixed = TRUE)
})

test_that("duckdt_dm_query() adds WHERE and LIMIT, and reports unjoinable tables", {
  sql <- duckdt_dm_query(shop_query_dm(), "orders", where = "total > 100", limit = 10)
  expect_match(sql, "WHERE total > 100")
  expect_match(sql, "LIMIT 10")

  loose <- duckdt_dm_query(shop_query_dm(), c("orders", "loose"))
  expect_equal(attr(loose, "unjoined"), "loose")
  expect_false(grepl("JOIN", loose))

  expect_error(duckdt_dm_query(shop_query_dm(), "nope"), "no table")
})

test_that("the generated SQL runs and is schema-qualified against a real database", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE customers (id INTEGER PRIMARY KEY, name VARCHAR)")
  DBI::dbExecute(con, "
    CREATE TABLE orders (
      id INTEGER PRIMARY KEY, customer_id INTEGER, total DOUBLE,
      FOREIGN KEY (customer_id) REFERENCES customers(id)
    )
  ")
  DBI::dbExecute(con, "INSERT INTO customers VALUES (1, 'Ada'), (2, 'Grace')")
  DBI::dbExecute(con, "INSERT INTO orders VALUES (10, 1, 99.5), (11, 2, 12.25)")

  dm <- duckdt_data_model(con)
  sql <- duckdt_dm_query(dm, c("orders", "customers"),
    columns = list(orders = "total", customers = "name"), where = "total > 50")
  expect_match(sql, 'FROM "main"."orders"', fixed = TRUE)

  res <- DBI::dbGetQuery(con, sql)
  expect_equal(nrow(res), 1L)
  expect_equal(res$name, "Ada")
  expect_equal(res$total, 99.5)
})

test_that("a reference with no known target column is drawn but not joined on", {
  # `ref` given without `ref_col`, and no key on the target to fall back to.
  dm <- duckdt_data_model(data.frame(
    table = c("a", "b"), column = c("b_id", "id"),
    ref = c("b", NA), stringsAsFactors = FALSE
  ))
  expect_true(is.na(dm$references$ref_col))
  expect_match(duckdt_dm_mermaid(dm), "b ||--o{ a", fixed = TRUE)

  sql <- duckdt_dm_query(dm, c("a", "b"))
  expect_false(grepl("JOIN", sql))
  expect_equal(attr(sql, "unjoined"), "b")
})
