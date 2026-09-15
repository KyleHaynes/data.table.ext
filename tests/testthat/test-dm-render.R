two_table_dm <- function() {
  dm <- duckdt_data_model(list(
    orders = data.frame(id = 1L, customer_id = 1L, total = 1),
    customers = data.frame(id = 1L, name = "a")
  ))
  duckdt_dm_add_references(dm, orders$customer_id == customers$id)
}

test_that("duckdt_dm_mermaid() draws entities, keys and relationships", {
  m <- duckdt_dm_mermaid(two_table_dm())
  expect_match(m, "^erDiagram")
  expect_match(m, "orders \\{")
  expect_match(m, "integer id PK")
  expect_match(m, "integer customer_id FK")
  expect_match(m, 'customers \\|\\|--o\\{ orders : "customer_id"')
})

test_that("duckdt_dm_mermaid() honours the view type", {
  dm <- two_table_dm()

  keys <- duckdt_dm_mermaid(dm, view = "keys_only")
  expect_match(keys, "customer_id")
  expect_false(grepl("total", keys))

  titles <- duckdt_dm_mermaid(dm, view = "title_only")
  expect_false(grepl("integer customer_id", titles))   # no columns...
  expect_match(titles, 'orders : "customer_id"')       # ...but still linked
  expect_match(titles, "orders \\{")
})

test_that("mermaid names that aren't identifiers are sanitized", {
  dm <- duckdt_data_model(data.frame(
    table = "Order Lines", column = "Line number", type = "INTEGER (32)",
    stringsAsFactors = FALSE
  ))
  m <- duckdt_dm_mermaid(dm)
  expect_match(m, "Order_Lines \\{")
  expect_match(m, "INTEGER_32_ Line_number")
})

test_that("duckdt_dm_dot() emits a graph with HTML labels and edges", {
  dot <- duckdt_dm_dot(two_table_dm())
  expect_s3_class(dot, "duckdt_dot")
  expect_match(unclass(dot), "^#duckdt_data_model\ndigraph \\{")
  expect_match(unclass(dot), "'orders'->'customers'")
  expect_match(unclass(dot), "<U>id</U>")     # primary key underlined
  expect_match(unclass(dot), "rankdir=BT")
})

test_that("duckdt_dm_dot() supports view types, segments and column arrows", {
  dm <- duckdt_dm_set_segment(two_table_dm(), list(sales = "orders"))

  dot <- unclass(duckdt_dm_dot(dm, view = "keys_only"))
  expect_false(grepl("total", dot))
  expect_match(dot, "subgraph cluster_1")
  expect_match(dot, "label='sales'")

  arrows <- unclass(duckdt_dm_dot(dm, column_arrows = TRUE))
  expect_match(arrows, "'orders':'customer_id'->'customers':'id'")
  expect_match(arrows, 'PORT="customer_id"')

  expect_match(unclass(duckdt_dm_dot(dm, col_attr = c("column", "type"))), "integer")
  expect_error(duckdt_dm_dot(dm, col_attr = "nope"), "col_attr")
})

test_that("dot labels escape characters that would break the XML label", {
  dm <- duckdt_data_model(data.frame(
    table = "a", column = "x<y&z", stringsAsFactors = FALSE
  ))
  expect_match(unclass(duckdt_dm_dot(dm)), "x&lt;y&amp;z", fixed = TRUE)
})

test_that("hidden tables are left out of both renderings", {
  dm <- duckdt_dm_set_display(two_table_dm(), list(hide = "customers"))
  expect_false(grepl("customers", unclass(duckdt_dm_dot(dm))))
  expect_false(grepl("customers", duckdt_dm_mermaid(dm)))
})

test_that("the colour scheme is set up and extensible", {
  scheme <- duckdt_dm_get_color_scheme()
  expect_true(all(c("default", "accent1", "accent7nb") %in% names(scheme)))
  expect_null(scheme$accent1nb$line_color)

  on.exit(duckdt_dm_set_color_scheme(scheme))
  duckdt_dm_add_colors(duckdt_dm_color_scheme(
    tester = duckdt_dm_palette(
      line_color = "#111111", header_bgcolor = "#222222",
      header_font = "#333333", bgcolor = "#444444"
    )
  ))
  dm <- duckdt_dm_set_display(two_table_dm(), list(tester = "orders"))
  expect_match(unclass(duckdt_dm_dot(dm)), "#222222", fixed = TRUE)
})
