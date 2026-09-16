test_that("membership handles empty sets and missing values like data.table", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ref <- data.table(x = c(1L, 2L, NA_integer_))
  d <- as.duckdt(ref, conn = con, name = "membership")

  for (values in list(integer(), NULL, 1L, NA_integer_, c(1L, NA_integer_))) {
    expect_same_rows(d[x %in% values], ref[x %in% values])
    expect_same_rows(d[!x %in% values], ref[!x %in% values])
  }
  expect_equal(d[, .(hit = x %in% 1L)]$hit, ref$x %in% 1L)
  expect_same_rows(d[x %in% c(1L, NA_integer_) & x > 1L], ref[0L])
})

test_that("character and date membership retain literal types", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ref <- data.table(label = c("O'Brien", "b", NA_character_),
                    day = as.Date(c("2026-01-01", "2026-01-02", NA)))
  d <- as.duckdt(ref, conn = con, name = "typed_membership")
  values <- as.Date(c("2026-01-01", NA))
  expect_same_rows(d[day %in% values], ref[day %in% values])
  expect_same_rows(d[label %chin% c("O'Brien", NA_character_)],
                   ref[label %chin% c("O'Brien", NA_character_)])
})

test_that("named grouping expressions work for queries and temporary tables", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ref <- data.table(x = c(1L, 2L, 3L), y = c(3L, 4L, 5L))
  d <- as.duckdt(ref, conn = con, name = "grouping")
  expected <- ref[, .N, by = .(bucket = y > 3L)]
  expect_same_rows(d[, .N, by = .(bucket = y > 3L)], expected)
  out <- duckdt_temp(d, , .N, by = .(bucket = y > 3L))
  expect_same_rows(as.data.table(out), expected)
})

test_that("unmapped SQL functions reach the database", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- as.duckdt(data.frame(x = c(1, 4, 9)), conn = con, name = "functions")
  expect_equal(d[, .(root = sqrt(x))]$root, c(1, 2, 3))
})
