test_that("the slide demo creates its database and preserves existing files", {
  demo <- new.env(parent = globalenv())
  sys.source(system.file("slides", "demo-db.R", package = "data.table.ext"), envir = demo)
  con <- demo$demo_address_db()
  on.exit(duckdt_disconnect(con))
  expect_equal(sort(duckdt_tables(con)$name),
               c("addresses", "client_addresses", "localities", "states"))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM addresses")$n, 8)

  path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(path), add = TRUE)
  writeLines("existing user data", path)
  expect_error(demo$demo_address_db(path), "already exists")
  expect_identical(readLines(path), "existing user data")
})
