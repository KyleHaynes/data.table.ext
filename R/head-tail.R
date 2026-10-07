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
#' On SQL Server, `tail()` of a table with a clustered index returns the last
#' `n` rows by that index's key, read from the end of the index, so it costs
#' about what `head()` does. Without one (a heap, a clustered columnstore
#' index or a view), it counts the rows and then reads past all but the last
#' `n`, which on a large table means two full scans.
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
#' @examples
#' d <- as.dbdt(datasets::mtcars)
#' head(d, 3)
#' tail(d, 3)
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
    nr <- duckdt_count_rows(x)
    n <- if (n < 0) max(nr + n, 0) else nr
  }
  dialect <- duckdt_dialect(x$conn)
  sql <- duckdt_limit_sql(duckdt_qtbl(x), n, dialect, duckdt_star(x))
  # `[]` works around a data.table quirk where setDT() suppresses the next
  # top-level auto-print (see the note in bracket.R).
  data.table::setDT(duckdt_get_query(x$conn, sql))[]
}

#' @rdname duckdt-head-tail
#' @exportS3Method utils::tail
tail.duckdt <- function(x, n = 6L, ...) {
  dialect <- duckdt_dialect(x$conn)
  # On SQL Server the OFFSET below reads past every row before the last n,
  # after a count(*) that scans the table too: two full passes over a large
  # table to show six rows. A table with a clustered index can instead be
  # read backwards from the end of it, which takes the last n rows directly.
  if (dialect == "mssql" && n >= 0 && is.finite(n) && isTRUE(x$materialized)) {
    order <- duckdt_mssql_reverse_order(x)
    if (!is.null(order)) {
      sql <- paste0(duckdt_limit_sql(duckdt_qtbl(x), n, dialect, duckdt_star(x)), " ORDER BY ", order)
      out <- data.table::setDT(duckdt_get_query(x$conn, sql))
      return(out[rev(seq_len(nrow(out)))])
    }
  }
  nr <- duckdt_count_rows(x)
  n <- if (n < 0) max(nr + n, 0) else min(n, nr)
  off <- max(nr - n, 0)
  sql <- duckdt_tail_sql(duckdt_qtbl(x), n, off, dialect, duckdt_star(x))
  data.table::setDT(duckdt_get_query(x$conn, sql))[]
}

# The ORDER BY that walks a SQL Server table's clustered rowstore index from
# its end: each key column in the opposite direction to the index's own.
# NULL for a heap, a clustered columnstore index (which has no key order), or
# if the catalogue can't be read. Kept on the handle after the first lookup,
# like the column types.
duckdt_mssql_reverse_order <- function(x) {
  cache <- x$meta
  if (is.environment(cache) && !is.null(cache$reverse_order)) {
    return(if (nzchar(cache$reverse_order)) cache$reverse_order)
  }
  sys <- duckdt_mssql_sys(x$tbl)
  res <- tryCatch(duckdt_get_query(x$conn, paste0(
    "SELECT c.name AS column_name, ic.is_descending_key AS descending ",
    "FROM ", sys, "indexes i ",
    "JOIN ", sys, "index_columns ic ON ic.object_id = i.object_id AND ic.index_id = i.index_id ",
    "JOIN ", sys, "columns c ON c.object_id = ic.object_id AND c.column_id = ic.column_id ",
    "WHERE i.object_id = ", duckdt_mssql_object_id(x$conn, x$tbl, x$schema),
    " AND i.type = 1 AND ic.key_ordinal > 0 ORDER BY ic.key_ordinal"
  )), error = function(e) NULL)
  order <- if (is.data.frame(res) && nrow(res) && ncol(res) >= 2) {
    paste(
      DBI::dbQuoteIdentifier(x$conn, as.character(res[[1]])),
      ifelse(as.logical(res[[2]]) %in% TRUE, "ASC", "DESC"),
      collapse = ", "
    )
  } else {
    ""
  }
  if (is.environment(cache)) cache$reverse_order <- order
  if (nzchar(order)) order
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
#' Samples in the database, materializing the result as a `data.table`.
#' This is a plain function rather than a
#' `sample.duckdt` S3 method because base R's [sample()] is not a true S3
#' generic (it doesn't call `UseMethod()`), so a `sample.duckdt` method would
#' never be dispatched by a plain `sample(d, ...)` call.
#'
#' @param x A `"duckdt"` object.
#' @param n Number of rows to sample, a non-negative whole number.
#' @param method `"random"` (default) returns `n` random rows, or all rows
#'   for a smaller table. On SQL Server this uses `ORDER BY NEWID()` and
#'   processes the entire table. `"fast"` uses SQL Server's
#'   `TABLESAMPLE SYSTEM` to read a sample of pages, capped at `n` rows.
#'   It may return fewer rows (even zero), and rows on the same page are
#'   sampled together; it is intended for exploration, not uniform random
#'   sampling. It requires a local base table, not a view. On DuckDB both
#'   methods use reservoir sampling.
#'
#' @return A `data.table` of at most `n` sampled rows.
#' @examples
#' d <- as.dbdt(datasets::mtcars)
#' dbdt_sample(d, 5)
#' @export
duckdt_sample <- function(x, n, method = c("random", "fast")) {
  method <- match.arg(method)
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
      n < 0 || n != trunc(n)) {
    stop("`n` must be a single non-negative finite whole number.", call. = FALSE)
  }
  dialect <- duckdt_dialect(x$conn)
  if (n > 0 && dialect == "mssql" && method == "fast" && !isTRUE(x$materialized)) {
    stop('Fast sampling requires a SQL Server base table; use method = "random" or head() for views.',
         call. = FALSE)
  }
  sql <- duckdt_sample_sql(duckdt_qtbl(x), n, dialect, duckdt_star(x), method)
  data.table::setDT(duckdt_get_query(x$conn, sql))[]
}

# T-SQL's TABLESAMPLE is page-based/approximate and can't guarantee an exact
# row count; ORDER BY NEWID() is the standard exact-n-row random sample idiom.
duckdt_sample_sql <- function(qtbl, n, dialect, sel = "*", method = "random") {
  if (n == 0) return(duckdt_limit_sql(qtbl, 0, dialect, sel))
  n <- format(n, scientific = FALSE, trim = TRUE)
  if (dialect == "mssql") {
    sample <- if (method == "fast") paste0(" TABLESAMPLE SYSTEM (", n, " ROWS)") else ""
    paste0("SELECT TOP (", n, ") ", sel, " FROM ", qtbl, sample, " ORDER BY NEWID()")
  } else {
    paste0("SELECT ", sel, " FROM ", qtbl, " USING SAMPLE reservoir(", n, " ROWS)")
  }
}
