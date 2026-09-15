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

  expect_true(is_duckdt_data_model(attr(path, "data_model")))

  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "mermaid")
  expect_match(html, "erd_parent")
  # The page carries the model it draws from, so its JavaScript can redraw
  # whatever subset of it you tick.
  expect_match(html, '"table":"erd_child"', fixed = TRUE)
  expect_match(html, '"ref":"erd_parent"', fixed = TRUE)
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
  expect_match(html, '"n_rows":32', fixed = TRUE)
  expect_equal(attr(path, "data_model")$tables$n_rows, 32)
})

test_that("duckdt_erd() takes a data model, a table subset, and a view type", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE a (id INTEGER PRIMARY KEY, note VARCHAR)")
  DBI::dbExecute(con, "CREATE TABLE b (id INTEGER PRIMARY KEY)")

  path <- duckdt_erd(con, tables = "a", open = FALSE, view = "keys_only")
  expect_false(grepl("note", attr(path, "mermaid")))
  expect_equal(attr(path, "data_model")$tables$table, "a")

  dm <- duckdt_dm_filter(duckdt_data_model(con), "b")
  path <- duckdt_erd(dm, open = FALSE)
  expect_false(grepl("main_a \\{", attr(path, "mermaid")))
})

test_that("duckdt_erd() writes where told and escapes names into the page safely", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, 'CREATE TABLE t ("</script> x" INTEGER)')

  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  path <- duckdt_erd(con, open = FALSE, file = file)
  expect_equal(as.character(path), file)

  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # The column name must not be able to close the page's <script> block.
  expect_false(grepl("</script> x", html, fixed = TRUE))
  expect_match(html, "\\\\u003c/script", fixed = FALSE)
})
