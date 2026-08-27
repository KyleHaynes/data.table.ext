# Regression test for a data.table quirk: setDT() marks its result so the
# *next* top-level auto-print is silently skipped (the same mechanism `:=`
# uses) unless the returned data.table ends with `[]`. Without that, e.g.
# `head(d)` at the console would print nothing the first two times and only
# work once `[]` was appended by hand. withVisible()$visible mirrors
# whether R's auto-print would actually fire.

test_that("results auto-print (are visible) rather than being silently suppressed", {
  d <- mtcars_dt()

  expect_true(withVisible(d[cyl == 6])$visible)
  expect_true(withVisible(head(d))$visible)
  expect_true(withVisible(tail(d))$visible)
  expect_true(withVisible(duckdt_sample(d, 3))$visible)
  expect_true(withVisible(as.data.table(d))$visible)
})
