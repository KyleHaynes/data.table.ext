#' Return the first or last rows of a duckdt table
#'
#' Pushes a `LIMIT`/`OFFSET` query down to DuckDB and materializes the result
#' as a `data.table`. Negative `n` mirrors [utils::head()]/[utils::tail()]:
#' `head(x, -2)` returns all but the last 2 rows.
#'
#' Because DuckDB tables are unordered without an explicit `ORDER BY`,
#' `tail()` reflects DuckDB's current scan order rather than a guaranteed
#' original row order — see the package README for details.
#'
#' @param x A `"duckdt"` object.
#' @param n Number of rows. Negative values count from the opposite end.
#' @param ... Unused.
#'
#' @return A `data.table`.
#' @name duckdt-head-tail
NULL

#' @rdname duckdt-head-tail
#' @exportS3Method utils::head
head.duckdt <- function(x, n = 6L, ...) {
  nr <- dim(x)[1]
  n <- if (n < 0) max(nr + n, 0) else min(n, nr)
  sql <- paste0("SELECT * FROM ", duckdt_qtbl(x), " LIMIT ", as.integer(n))
  data.table::setDT(DBI::dbGetQuery(x$conn, sql))
}

#' @rdname duckdt-head-tail
#' @exportS3Method utils::tail
tail.duckdt <- function(x, n = 6L, ...) {
  nr <- dim(x)[1]
  n <- if (n < 0) max(nr + n, 0) else min(n, nr)
  off <- max(nr - n, 0)
  sql <- paste0("SELECT * FROM ", duckdt_qtbl(x), " LIMIT ", as.integer(n), " OFFSET ", as.integer(off))
  data.table::setDT(DBI::dbGetQuery(x$conn, sql))
}

#' Sample rows from a duckdt table
#'
#' Pushes DuckDB's `USING SAMPLE` clause down to the database, materializing
#' `n` sampled rows as a `data.table`. This is a plain function rather than a
#' `sample.duckdt` S3 method because base R's [sample()] is not a true S3
#' generic (it doesn't call `UseMethod()`), so a `sample.duckdt` method would
#' never be dispatched by a plain `sample(d, ...)` call.
#'
#' @param x A `"duckdt"` object.
#' @param n Number of rows to sample.
#'
#' @return A `data.table` of `n` sampled rows.
#' @export
duckdt_sample <- function(x, n) {
  nr <- dim(x)[1]
  n <- min(as.integer(n), nr)
  sql <- paste0("SELECT * FROM ", duckdt_qtbl(x), " USING SAMPLE reservoir(", n, " ROWS)")
  data.table::setDT(DBI::dbGetQuery(x$conn, sql))
}
