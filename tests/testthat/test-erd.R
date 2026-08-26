test_that("duckdt_erd() renders tables, columns, and a detected foreign key", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "CREATE TABLE erd_parent (id INTEGER PRIMARY KEY, name VARCHAR)")
  DBI::dbExecute(con, "
    CREATE TABLE erd_child (
      id INTEGER PRIMARY KEY,
      parent_id INTEGER,
      FOREIGN KEY (parent_id) REFERENCES erd_parent(id)
    )
  ")

  path <- duckdt_erd(con, open = FALSE)
  expect_true(file.exists(path))

  mermaid <- attr(path, "mermaid")
  expect_match(mermaid, "^erDiagram")
  expect_match(mermaid, "main_erd_parent \\{")
  expect_match(mermaid, "main_erd_child \\{")
  expect_match(mermaid, "PK")
  expect_match(mermaid, 'main_erd_parent \\|\\|--o\\{ main_erd_child : "parent_id"')

  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "mermaid")
  expect_match(html, "erd_parent")
})

test_that("duckdt_erd() accepts a duckdt object and works without any foreign keys", {
  d <- mtcars_dt(copy = TRUE)

  path <- duckdt_erd(d, open = FALSE)
  mermaid <- attr(path, "mermaid")
  expect_match(mermaid, "main_mtcars_test \\{")
  expect_false(grepl("--o\\{", mermaid))
})

test_that("duckdt_erd() errors clearly when there are no tables", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_error(duckdt_erd(con, open = FALSE), "no tables")
})

test_that("duckdt_erd() can include row counts", {
  d <- mtcars_dt(copy = TRUE)

  path <- duckdt_erd(d, include_row_counts = TRUE, open = FALSE)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "Rows")
  expect_match(html, "32")
})
