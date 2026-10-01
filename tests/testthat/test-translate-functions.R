# The R and data.table functions translate.R knows, checked by running them
# in DuckDB and comparing with what R computes on the same data, and (for
# SQL Server, where there is no server to run them on) by the T-SQL emitted.

# Each test closes the connection with on.exit(dbdt_disconnect(s$con)).
local_cars <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  cars <- data.table::data.table(
    car = rownames(datasets::mtcars), datasets::mtcars,
    cyl_i = as.integer(datasets::mtcars$cyl),
    made = as.Date("2024-01-07") + 0:31
  )
  list(con = con, dt = cars, d = as.dbdt(cars, conn = con, name = "cars", copy = TRUE))
}

test_that("na.rm is dropped from aggregates, which already skip NULL", {
  s <- local_cars()
  on.exit(dbdt_disconnect(s$con), add = TRUE)
  out <- s$d[, .(m = mean(mpg, na.rm = TRUE), s = sum(hp, na.rm = TRUE), md = median(wt, na.rm = TRUE))]
  expect_equal(out$m, mean(s$dt$mpg))
  expect_equal(out$s, sum(s$dt$hp))
  expect_equal(out$md, median(s$dt$wt))
})

test_that("ifelse(), fifelse() and fcase() become CASE, with R's missing-value rules", {
  s <- local_cars()
  on.exit(dbdt_disconnect(s$con), add = TRUE)
  out <- s$d[, .(car, size = ifelse(cyl > 4, "big", "small"),
                 band = fcase(hp < 100, "low", hp < 200, "mid", default = "high"),
                 f = fifelse(am == 1, 1, 0))][order(car)]
  exp <- s$dt[, .(car, size = ifelse(cyl > 4, "big", "small"),
                  band = data.table::fcase(hp < 100, "low", hp < 200, "mid", default = "high"),
                  f = data.table::fifelse(am == 1, 1, 0))][order(car)]
  expect_equal(out, exp, ignore_attr = TRUE)

  # A missing test gives a missing result, not the "no" branch.
  con <- s$con
  n <- as.dbdt(data.frame(x = c(1, NA, 3)), conn = con, name = "nas", copy = TRUE)
  expect_equal(n[, .(y = ifelse(x > 2, "hi", "lo"))]$y, c("lo", NA, "hi"))
  # data.table's own fifelse(na =) fills that case.
  expect_equal(n[, .(y = data.table::fifelse(x > 2, "hi", "lo", na = "?"))]$y, c("lo", "?", "hi"))
})

test_that("uniqueN() counts distinct values", {
  s <- local_cars()
  on.exit(dbdt_disconnect(s$con), add = TRUE)
  out <- s$d[, .(n = uniqueN(gear)), by = cyl][order(cyl)]
  exp <- s$dt[, .(n = data.table::uniqueN(gear)), by = cyl][order(cyl)]
  expect_equal(out$n, exp$n)
})

test_that("text functions: paste0, paste, substr, startsWith, endsWith, grepl, trimws", {
  s <- local_cars()
  on.exit(dbdt_disconnect(s$con), add = TRUE)
  out <- s$d[, .(car,
    a = paste0(car, "-", cyl_i),
    b = paste(car, cyl_i, sep = "/"),
    c = substr(car, 2, 4),
    t = trimws(paste0("  ", car, " "))
  )][order(car)]
  exp <- s$dt[, .(car,
    a = paste0(car, "-", cyl_i),
    b = paste(car, cyl_i, sep = "/"),
    c = substr(car, 2, 4),
    t = car
  )][order(car)]
  expect_equal(out, exp, ignore_attr = TRUE)

  expect_setequal(s$d[startsWith(car, "Merc")]$car, s$dt[startsWith(car, "Merc")]$car)
  expect_setequal(s$d[endsWith(car, "C")]$car, s$dt[endsWith(car, "C")]$car)
  expect_setequal(s$d[grepl("^To", car)]$car, s$dt[grepl("^To", car)]$car)
  expect_setequal(s$d[grepl("rx", car, ignore.case = TRUE)]$car, s$dt[grepl("rx", car, ignore.case = TRUE)]$car)
  expect_setequal(s$d[grepl(".", car, fixed = TRUE)]$car, s$dt[grepl(".", car, fixed = TRUE)]$car)
})

test_that("arithmetic and casts follow R: log, %%, %/%, ^, as.integer, as.character", {
  s <- local_cars()
  on.exit(dbdt_disconnect(s$con), add = TRUE)
  out <- s$d[, .(car,
    ln = log(hp), l2 = log(hp, 2), l10 = log10(hp),
    m = cyl_i %% 3L, q = hp %/% 7, p = cyl_i^0.5,
    i = as.integer(wt), ch = as.character(cyl_i), r = round(wt)
  )][order(car)]
  exp <- s$dt[, .(car,
    ln = log(hp), l2 = log(hp, 2), l10 = log10(hp),
    m = cyl_i %% 3L, q = hp %/% 7, p = cyl_i^0.5,
    i = as.integer(wt), ch = as.character(cyl_i), r = round(wt)
  )][order(car)]
  expect_equal(out, exp, ignore_attr = TRUE)
})

test_that("date parts match data.table's year(), month(), mday(), wday(), yday(), quarter()", {
  s <- local_cars()
  on.exit(dbdt_disconnect(s$con), add = TRUE)
  out <- s$d[, .(car, y = year(made), mo = month(made), d = mday(made),
                 w = wday(made), yd = yday(made), q = quarter(made))][order(car)]
  exp <- s$dt[, .(car, y = data.table::year(made), mo = data.table::month(made),
                  d = data.table::mday(made), w = data.table::wday(made),
                  yd = data.table::yday(made), q = data.table::quarter(made))][order(car)]
  expect_equal(out, exp, ignore_attr = TRUE)
})

test_that("a comparison can be averaged, between() and %notin% filter", {
  s <- local_cars()
  on.exit(dbdt_disconnect(s$con), add = TRUE)
  out <- s$d[, .(share = mean(hp > 150), n = sum(hp > 150)), by = cyl][order(cyl)]
  exp <- s$dt[, .(share = mean(hp > 150), n = sum(hp > 150)), by = cyl][order(cyl)]
  expect_equal(out$share, exp$share)
  expect_equal(out$n, exp$n)
  expect_setequal(s$d[between(hp, 100, 150)]$car, s$dt[data.table::between(hp, 100, 150)]$car)
  expect_setequal(s$d[cyl %notin% c(4, 6)]$car, s$dt[!cyl %in% c(4, 6)]$car)
})

test_that("pkg:: prefixes are ignored and date-time literals are quoted", {
  s <- local_cars()
  on.exit(dbdt_disconnect(s$con), add = TRUE)
  expect_equal(nrow(s$d[data.table::fifelse(cyl == 4, TRUE, FALSE)]), sum(s$dt$cyl == 4))
  con <- s$con
  ts <- as.POSIXct(c("2024-01-01 09:00:00", "2024-01-01 11:00:00"), tz = "UTC")
  e <- as.dbdt(data.frame(id = 1:2, at = ts), conn = con, name = "events", copy = TRUE)
  cut <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC")
  expect_equal(e[at > cut]$id, 2L)
  expect_equal(translate_literal(cut, con, "duckdb"), "'2024-01-01 10:00:00.000'")
})

test_that("SQL Server: predicates and values convert at the boundary", {
  con <- ansi_conn()
  cols <- c("x", "flag", "name")
  env <- environment()
  tv <- function(e, bit = FALSE) translate_value(e, cols, env, con, "mssql", bit = bit)
  tc <- function(e) translate_condition(e, cols, env, con, "mssql")

  # A comparison where a value goes becomes 1/0 (NULL when unknown).
  expect_equal(tv(quote(x > 1)), 'CASE WHEN "x" > 1 THEN 1 WHEN NOT ("x" > 1) THEN 0 END')
  expect_equal(tv(quote(x > 1), bit = TRUE),
    'CAST(CASE WHEN "x" > 1 THEN 1 WHEN NOT ("x" > 1) THEN 0 END AS BIT)')
  expect_equal(tv(quote(sum(x > 1))), 'sum(CASE WHEN "x" > 1 THEN 1 WHEN NOT ("x" > 1) THEN 0 END)')
  # A bit column where a condition goes is compared with 1.
  expect_equal(tc(quote(flag)), '("flag") = 1')
  expect_equal(tc(quote(flag & x > 1)), '("flag") = 1 AND "x" > 1')
  expect_equal(tc(quote(!flag)), 'NOT (("flag") = 1)')
  # DuckDB needs none of this.
  expect_equal(translate_condition(quote(flag), cols, env, con, "duckdb"), '"flag"')
  expect_equal(translate_value(quote(x > 1), cols, env, con, "duckdb"), '"x" > 1')
})

test_that("SQL Server: mean, division, powers and rounding avoid integer arithmetic", {
  con <- ansi_conn()
  cols <- c("x", "y")
  env <- environment()
  tm <- function(e) translate_expr(e, cols, env, con, "mssql")
  expect_equal(tm(quote(mean(x))), 'AVG(CAST("x" AS FLOAT))')
  expect_equal(tm(quote(x / y)), 'CAST("x" AS FLOAT) / "y"')
  expect_equal(tm(quote(x^2)), 'POWER(CAST("x" AS FLOAT), 2)')
  expect_equal(tm(quote(round(x))), 'ROUND("x", 0)')
  expect_equal(tm(quote(round(x, 2))), 'ROUND("x", 2)')
  expect_equal(tm(quote(trunc(x))), 'ROUND("x", 0, 1)')
  expect_equal(tm(quote(log(x))), 'LOG("x")')
  expect_equal(tm(quote(log(x, 2))), 'LOG("x", 2)')
  expect_equal(tm(quote(quarter(x))), 'DATEPART(quarter, "x")')
  expect_equal(tm(quote(wday(x))), '((DATEPART(weekday, "x") + @@DATEFIRST - 1) % 7 + 1)')
  expect_equal(tm(quote(paste0(x))), "CONCAT(\"x\", '')")
  expect_equal(tm(quote(paste(x, y, x, sep = "-"))), "CONCAT(\"x\", '-', \"y\", '-', \"x\")")
  expect_equal(tm(quote(paste(x, y))), "CONCAT(\"x\", ' ', \"y\")")
  expect_error(tm(quote(paste(x, collapse = ","))), "combines rows")
  expect_equal(tm(quote(trimws(x))), 'LTRIM(RTRIM("x"))')
  expect_equal(tm(quote(as.integer(x))), 'CAST(CAST("x" AS FLOAT) AS INT)')
})

test_that("SQL Server: plain-text regular expressions become LIKE", {
  con <- ansi_conn()
  cols <- "name"
  env <- environment()
  tm <- function(e) translate_expr(e, cols, env, con, "mssql")
  expect_equal(tm(quote(name %like% "smith")), "\"name\" LIKE '%smith%'")
  expect_equal(tm(quote(name %like% "^Mc")), "\"name\" LIKE 'Mc%'")
  expect_equal(tm(quote(name %like% "son$")), "\"name\" LIKE '%son'")
  expect_equal(tm(quote(name %like% "^exact$")), "\"name\" LIKE 'exact'")
  # LIKE's own wildcards in the text are matched literally.
  expect_equal(tm(quote(name %like% "50%_off")), "\"name\" LIKE '%50[%][_]off%'")
  expect_equal(tm(quote(name %ilike% "^mc")), "LOWER(\"name\") LIKE LOWER('mc%')")
  expect_equal(tm(quote(grepl("^Mc", name))), "\"name\" LIKE 'Mc%'")
  expect_equal(tm(quote(startsWith(name, "a_b"))), "\"name\" LIKE 'a[_]b%'")
  # Anything more is still a regular expression.
  expect_equal(tm(quote(name %like% "^M.c")), "REGEXP_LIKE(\"name\", '^M.c')")
  expect_equal(tm(quote(name %like% "a|b")), "REGEXP_LIKE(\"name\", 'a|b')")
})

test_that("order() in i sorts, descending with - or decreasing, missing values last", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- as.dbdt(data.frame(g = c("b", "a", "b", "a"), x = c(2, NA, 3, 1)), conn = con, name = "ord", copy = TRUE)
  expect_identical(d[order(x)]$x, c(1, 2, 3, NA))
  expect_identical(d[order(-x)]$x, c(3, 2, 1, NA))
  expect_identical(d[order(x, decreasing = TRUE)]$x, c(3, 2, 1, NA))
  expect_identical(d[order(x, na.last = FALSE)]$x, c(NA, 1, 2, 3))
  expect_identical(d[order(g, -x), .(g, x)]$x, c(1, NA, 3, 2))
  expect_error(d[order(x), .(n = .N), by = g], "can't be combined with `by`")
  # A temporary table has no order to keep, so it is simply not sorted.
  expect_equal(nrow(dbdt_temp(d, order(x))), 4L)

  # SQL Server has no NULLS LAST; missing values get a sort key of their own.
  expect_identical(
    translate_order(quote(order(-x)), "x", environment(), ansi_conn(), "mssql"),
    c('CASE WHEN "x" IS NULL THEN 1 ELSE 0 END', '"x" DESC')
  )
})
