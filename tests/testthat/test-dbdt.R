test_that("every public duckdt function has a compatible dbdt name", {
  exports <- getNamespaceExports("data.table.ext")
  legacy <- grep("duckdt", exports, value = TRUE)
  expect_length(legacy, 36L)
  for (old in legacy) {
    new <- sub("duckdt", "dbdt", old, fixed = TRUE)
    expect_true(new %in% exports, info = new)
    expect_identical(getExportedValue("data.table.ext", new),
                     getExportedValue("data.table.ext", old), info = new)
  }
})

test_that("dbdt names preserve NSE, automatic names, and S3 dispatch", {
  con <- dbdt_connect(quiet = TRUE)
  on.exit(dbdt_disconnect(con))
  source_rows <- data.frame(id = 1:3, value = c(5L, 10L, 15L))
  d <- as.dbdt(source_rows, conn = con, copy = TRUE)
  expect_identical(d$tbl, "source_rows")
  expect_identical(d[value > 5, .(id)]$id, 2:3)
  expect_identical(head(dbdt(con, "source_rows"), 1)$id, 1L)
  expect_identical(data.table::as.data.table(d)$value, source_rows$value)
  tmp <- dbdt_temp(d, value > 5, .(id))
  expect_identical(sort(tmp[, id]$id), 2:3)
  dbdt_drop(tmp)
  dm <- dbdt_data_model(list(parent = data.frame(id = 1L),
                             child = data.frame(parent_id = 1L)))
  dm <- dbdt_dm_add_references(dm, child$parent_id == parent$id)
  expect_true(is_dbdt_data_model(dm))
  expect_equal(nrow(dm$references), 1L)
  expect_true(is_dbdt_data_model(dbdt_data_model(d)))
})
