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
#' @section Binary columns:
#' Binary columns (`BLOB`/`BIT` on DuckDB, `varbinary`/`binary`/`image` on
#' MS SQL Server) are left out of the result: no driver hands them back as an
#' R vector, and asking for one fails the whole query rather than just that
#' column.
#' @name duckdt-head-tail
NULL

# Shared by head.duckdt() and print.duckdt(): a plain "first n rows" query,
# rendered per-dialect since MS SQL Server has no LIMIT clause. `sel` is the
# projection, "*" unless binary columns had to be dropped from it.
duckdt_limit_sql <- function(qtbl, n, dialect, sel = "*") {
  # A limit can exceed R's integer range even for a small source table.
  n_sql <- format(trunc(n), scientific = FALSE, trim = TRUE)
  if (dialect == "mssql") {
    paste0("SELECT TOP (", n_sql, ") ", sel, " FROM ", qtbl)
  } else {
    paste0("SELECT ", sel, " FROM ", qtbl, " LIMIT ", n_sql)
  }
}

#' @rdname duckdt-head-tail
#' @exportS3Method utils::head
head.duckdt <- function(x, n = 6L, ...) {
  # LIMIT/TOP already stop at the available rows. Counting first can force
  # a full scan of a file-backed view just to preview a handful of rows.
  if (n < 0 || is.infinite(n)) {
    nr <- dim(x)[1]
    n <- if (n < 0) max(nr + n, 0) else nr
  }
  dialect <- duckdt_dialect(x$conn)
  sql <- duckdt_limit_sql(duckdt_qtbl(x), n, dialect, duckdt_star(x))
  # `[]` works around a data.table quirk where setDT() suppresses the next
  # top-level auto-print (see the note in bracket.R).
  data.table::setDT(DBI::dbGetQuery(x$conn, sql))[]
}

#' @rdname duckdt-head-tail
#' @exportS3Method utils::tail
tail.duckdt <- function(x, n = 6L, ...) {
  nr <- dim(x)[1]
  n <- if (n < 0) max(nr + n, 0) else min(n, nr)
  off <- max(nr - n, 0)
  dialect <- duckdt_dialect(x$conn)
  sql <- duckdt_tail_sql(duckdt_qtbl(x), n, off, dialect, duckdt_star(x))
  data.table::setDT(DBI::dbGetQuery(x$conn, sql))[]
}

# OFFSET/FETCH requires an ORDER BY in T-SQL; ordering by a constant subquery
# is the standard way to get an unspecified order, matching DuckDB's own "no
# ordering guarantee" LIMIT/OFFSET semantics here.
duckdt_tail_sql <- function(qtbl, n, off, dialect, sel = "*") {
  if (dialect == "mssql") {
    paste0(
      "SELECT ", sel, " FROM ", qtbl, " ORDER BY (SELECT NULL) OFFSET ",
      as.integer(off), " ROWS FETCH NEXT ", as.integer(n), " ROWS ONLY"
    )
  } else {
    paste0("SELECT ", sel, " FROM ", qtbl, " LIMIT ", as.integer(n), " OFFSET ", as.integer(off))
  }
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
  n <- as.integer(n)
  dialect <- duckdt_dialect(x$conn)
  sql <- duckdt_sample_sql(duckdt_qtbl(x), n, dialect, duckdt_star(x))
  data.table::setDT(DBI::dbGetQuery(x$conn, sql))[]
}

# T-SQL's TABLESAMPLE is page-based/approximate and can't guarantee an exact
# row count; ORDER BY NEWID() is the standard exact-n-row random sample idiom.
duckdt_sample_sql <- function(qtbl, n, dialect, sel = "*") {
  if (dialect == "mssql") {
    paste0("SELECT TOP (", n, ") ", sel, " FROM ", qtbl, " ORDER BY NEWID()")
  } else {
    paste0("SELECT ", sel, " FROM ", qtbl, " USING SAMPLE reservoir(", n, " ROWS)")
  }
}
