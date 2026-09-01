#' An example database to try things on
#'
#' Creates a small in-memory DuckDB database -- four tables of a toy shop
#' schema, with real primary and foreign keys declared, and a few rows in
#' each. Somewhere to point [duckdt_erd()], [duckdt_explorer()] or
#' [duckdt_data_model()] when you want to see what they do before aiming
#' them at a database of your own.
#'
#' The tables are `customers`, `items`, `orders` and `order_lines`;
#' `order_lines` has a compound primary key and references both `orders` and
#' `items`, so the diagram has something to draw.
#'
#' @param quiet Skip the message describing what was created.
#'
#' @return A `DBI` connection to a new in-memory DuckDB database. Close it
#'   with [duckdt_disconnect()].
#' @examples
#' con <- duckdt_example(quiet = TRUE)
#' duckdt_tables(con)
#' duckdt_data_model(con)
#'
#' d <- duckdt(con, "orders")
#' d[, .N, by = customer_id]
#'
#' duckdt_disconnect(con)
#' @export
duckdt_example <- function(quiet = FALSE) {
  con <- DBI::dbConnect(duckdb::duckdb())

  DBI::dbExecute(con, "
    CREATE TABLE customers (
      customer_id INTEGER PRIMARY KEY,
      name        VARCHAR,
      city        VARCHAR
    )")
  DBI::dbExecute(con, "
    CREATE TABLE items (
      item_id INTEGER PRIMARY KEY,
      label   VARCHAR,
      price   DOUBLE
    )")
  DBI::dbExecute(con, "
    CREATE TABLE orders (
      order_id    INTEGER PRIMARY KEY,
      customer_id INTEGER REFERENCES customers(customer_id),
      ordered_on  DATE
    )")
  DBI::dbExecute(con, "
    CREATE TABLE order_lines (
      order_id INTEGER REFERENCES orders(order_id),
      line_no  INTEGER,
      item_id  INTEGER REFERENCES items(item_id),
      qty      INTEGER,
      PRIMARY KEY (order_id, line_no)
    )")

  DBI::dbExecute(con, "INSERT INTO customers VALUES
    (1, 'Ada Lovelace', 'Perth'),
    (2, 'Grace Hopper', 'Sydney'),
    (3, 'Alan Turing', 'Perth')")
  DBI::dbExecute(con, "INSERT INTO items VALUES
    (1, 'Rubber duck', 9.95),
    (2, 'Keyboard', 129.00),
    (3, 'Monitor', 449.00)")
  DBI::dbExecute(con, "INSERT INTO orders VALUES
    (100, 1, DATE '2024-03-01'),
    (101, 2, DATE '2024-03-04'),
    (102, 1, DATE '2024-04-11')")
  DBI::dbExecute(con, "INSERT INTO order_lines VALUES
    (100, 1, 1, 3),
    (100, 2, 2, 1),
    (101, 1, 3, 2),
    (102, 1, 1, 1)")

  if (!quiet) {
    cli::cli_inform(c(
      "v" = "Example database ready: {.val customers}, {.val items}, {.val orders}, {.val order_lines}.",
      ">" = "{.code duckdt_erd(con)} to see the tables and how they connect",
      ">" = "{.code duckdt(con, \"orders\")[, .N, by = customer_id]} to query one"
    ))
  }
  con
}
