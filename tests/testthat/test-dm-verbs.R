shop_dm <- function() {
  duckdt_data_model(list(
    customers = data.frame(customer_id = 1L, name = "a"),
    orders = data.frame(order_id = 1L, customer_id = 1L, total = 1),
    order_lines = data.frame(order_id = 1L, line_no = 1L, item_id = 1L),
    items = data.frame(item_id = 1L, label = "a")
  ))
}

test_that("duckdt_dm_set_key() marks keys, including compound ones", {
  dm <- duckdt_dm_set_key(shop_dm(), "order_lines", c("order_id", "line_no"))
  keys <- dm$columns[table == "order_lines"]
  expect_equal(keys[column == "order_id"]$key, 1L)
  expect_equal(keys[column == "line_no"]$key, 2L)
  expect_equal(keys[column == "item_id"]$key, 0L)

  expect_error(duckdt_dm_set_key(dm, "orders", "nope"), "no column")
})

test_that("duckdt_dm_add_references() reads table$column == table$column", {
  dm <- shop_dm()
  dm <- duckdt_dm_add_references(
    dm,
    orders$customer_id == customers$customer_id,
    order_lines$order_id == orders$order_id
  )
  expect_equal(nrow(dm$references), 2L)
  expect_equal(
    dm$references[table == "orders"]$ref, "customers"
  )
  # Being referenced makes the target a key.
  expect_equal(dm$columns[table == "customers" & column == "customer_id"]$key, 1L)

  expect_error(duckdt_dm_add_references(dm, orders$customer_id), "table\\$column")
})

test_that("duckdt_dm_add_reference() validates its arguments", {
  dm <- shop_dm()
  expect_error(duckdt_dm_add_reference(dm, "nope", "x", "orders"), "no table")
  expect_error(duckdt_dm_add_reference(dm, "orders", "x", "nope"), "no referenced table")
  expect_error(duckdt_dm_add_reference(dm, "orders", "nope", "customers"), "no column")
  expect_error(
    duckdt_dm_add_reference(dm, "orders", "customer_id", "customers", "nope"),
    "no column"
  )
})

test_that("a compound reference is one relationship, two references are two", {
  dm <- duckdt_data_model(list(
    orders = data.frame(order_id = 1L, line_no = 1L),
    shipments = data.frame(order_id = 1L, line_no = 1L, sold_by = 1L, packed_by = 1L),
    staff = data.frame(id = 1L)
  ))
  dm <- duckdt_dm_set_key(dm, "orders", c("order_id", "line_no"))
  dm <- duckdt_dm_set_key(dm, "staff", "id")
  dm <- duckdt_dm_add_reference(dm, "shipments", c("order_id", "line_no"), "orders",
    c("order_id", "line_no"))
  dm <- duckdt_dm_add_reference(dm, "shipments", "sold_by", "staff", "id")
  dm <- duckdt_dm_add_reference(dm, "shipments", "packed_by", "staff", "id")

  expect_equal(nrow(dm$references), 4L)
  # order_id + line_no are one compound reference; the two staff columns are
  # two separate ones.
  expect_equal(length(unique(dm$references$ref_id)), 3L)
  compound <- dm$references[ref == "orders"]
  expect_equal(compound$ref_col_num, 1:2)
  expect_equal(length(unique(compound$ref_id)), 1L)
})

test_that("duckdt_dm_infer_references() guesses conventional foreign keys", {
  dm <- shop_dm()
  dm <- duckdt_dm_set_key(dm, "customers", "customer_id")
  dm <- duckdt_dm_set_key(dm, "orders", "order_id")
  dm <- duckdt_dm_set_key(dm, "items", "item_id")

  dm <- duckdt_dm_infer_references(dm, quiet = TRUE)
  found <- paste(dm$references$table, dm$references$column, dm$references$ref)
  expect_true("orders customer_id customers" %in% found)
  expect_true("order_lines order_id orders" %in% found)
  expect_true("order_lines item_id items" %in% found)
  # A table never references itself, so orders$order_id is left alone.
  expect_false(any(dm$references$table == "orders" & dm$references$column == "order_id"))
})

test_that("duckdt_dm_infer_references() is conservative", {
  # `person_id` should find `persons` through the naive plural rule, while
  # `code_id` matches `codes` by name but not by type, so it is rejected.
  dm <- duckdt_data_model(list(
    persons = data.frame(id = 1L, name = "a"),
    codes = data.frame(code_id = 1L, label = "a"),
    notes = data.frame(id = 1L, person_id = 1L, code_id = "x", stringsAsFactors = FALSE)
  ))
  dm <- duckdt_dm_set_key(dm, "persons", "id")
  dm <- duckdt_dm_set_key(dm, "codes", "code_id")
  dm <- duckdt_dm_infer_references(dm, quiet = TRUE)

  expect_equal(nrow(dm$references), 1L)
  expect_equal(dm$references$column, "person_id")
  expect_equal(attr(dm$references, "inferred"), "notes$person_id -> persons$id")

  # A bare `id` is never treated as pointing at another table.
  expect_false(any(dm$references$column == "id"))
})

test_that("duckdt_dm_infer_references() reports what it guessed", {
  dm <- duckdt_data_model(list(
    persons = data.frame(id = 1L),
    notes = data.frame(person_id = 1L)
  ))
  dm <- duckdt_dm_set_key(dm, "persons", "id")
  expect_message(duckdt_dm_infer_references(dm), "Inferred 1 reference")
})

test_that("duckdt_dm_infer_references() says when there is nothing to aim at", {
  expect_message(
    dm <- duckdt_dm_infer_references(shop_dm()),
    "No single-column primary keys"
  )
  expect_equal(nrow(dm$references), 0L)
})

test_that("duckdt_dm_set_segment() and duckdt_dm_set_display() tag tables", {
  dm <- duckdt_dm_set_segment(shop_dm(), list(sales = c("orders", "order_lines")))
  dm <- duckdt_dm_set_display(dm, list(accent1 = "customers", hide = "items"))

  expect_equal(dm$tables[table == "orders"]$segment, "sales")
  expect_true(is.na(dm$tables[table == "items"]$segment))
  expect_equal(dm$tables[table == "customers"]$display, "accent1")

  # A hidden table is left out of the diagrams.
  expect_false(grepl("items", duckdt_dm_mermaid(dm)))
})

test_that("duckdt_dm_filter() keeps neighbours up to `depth`", {
  dm <- shop_dm()
  dm <- duckdt_dm_set_key(dm, "customers", "customer_id")
  dm <- duckdt_dm_set_key(dm, "orders", "order_id")
  dm <- duckdt_dm_add_references(
    dm,
    orders$customer_id == customers$customer_id,
    order_lines$order_id == orders$order_id
  )

  expect_equal(duckdt_dm_filter(dm, "orders")$tables$table, "orders")
  expect_setequal(
    duckdt_dm_filter(dm, "orders", depth = 1)$tables$table,
    c("orders", "customers", "order_lines")
  )
  expect_setequal(
    duckdt_dm_filter(dm, "customers", depth = 2)$tables$table,
    c("customers", "orders", "order_lines")
  )

  # Filtering by columns drops the rest, and a reference whose other end is
  # gone is dropped rather than left dangling.
  narrow <- duckdt_dm_filter(dm, "orders", columns = list(orders = "total"))
  expect_equal(narrow$columns$column, "total")
  expect_equal(nrow(narrow$references), 0L)

  expect_error(duckdt_dm_filter(dm, "nope"), "no table")
})
