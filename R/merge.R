# Internal: merge (upsert, optionally delete-unmatched) a subset `y` into a
# materialized duckdt table `x`, entirely inside the database. `y` is staged
# into a temporary table on `x`'s connection first (see duckdt_merge_stage())
# so `x`'s existing data never has to round-trip through R -- only `y`
# crosses into the database, and matching/updating/inserting/deleting all run
# as SQL against the staged table.

#' Merge a subset into a duckdt table, inside the database
#'
#' Loads `y` into a temporary table on `x`'s connection, then reconciles `x`
#' against it entirely in SQL: matching rows are updated, rows only in `y`
#' are inserted, and (opt-in via `delete = TRUE`) rows only in `x` are
#' deleted. This is a database-side upsert/merge -- `x`'s existing data never
#' round-trips through R, and the staging table is dropped once the merge
#' completes.
#'
#' On DuckDB this is emulated with `UPDATE ... FROM`, an anti-join `INSERT`,
#' and (if `delete = TRUE`) an anti-join `DELETE`, wrapped in a transaction.
#' On MS SQL Server it compiles to a single native T-SQL `MERGE` statement.
#'
#' @param x A `"duckdt"` object backed by a **materialized, writable** table
#'   (see [as.duckdt()]'s `copy` argument, and [duckdt()]'s `writable`
#'   argument) -- `duckdt_merge()` mutates `x` in place, so this can't be a
#'   read-only view, nor a handle from `duckdt(conn, table)` that hasn't
#'   opted into `writable = TRUE`.
#' @param y The subset to merge in: a `data.frame`/`data.table`, or another
#'   `"duckdt"` object (table, view, or query result) on the *same*
#'   connection as `x`. Columns present in `y` but not in `x` are ignored --
#'   use `:=` first if you need to add a column.
#' @param by Key column(s) used to match rows between `x` and `y`. Defaults
#'   to *every* column common to both (a natural join, mirroring base
#'   [merge()]) -- since a column can't be both a match key and something
#'   `update` sets from `y`, pass `by=` explicitly whenever `y` carries value
#'   columns you want updated rather than matched on.
#' @param update Update matching rows' shared non-key columns from `y`?
#'   Default `TRUE`.
#' @param insert Insert rows present in `y` but not `x`? Default `TRUE`.
#' @param delete Delete rows present in `x` but not `y`? Default `FALSE`;
#'   set `TRUE` for a "replace this subset entirely" merge.
#'
#' @return `x`, invisibly.
#' @export
duckdt_merge <- function(x, y, by = NULL, update = TRUE, insert = TRUE, delete = FALSE) {
  if (!inherits(x, "duckdt")) {
    stop("duckdt_merge: `x` must be a duckdt object.", call. = FALSE)
  }
  if (!isTRUE(x$materialized)) {
    stop(
      "duckdt: `duckdt_merge()` requires a materialized table since it mutates data in place. ",
      "This handle is backed by a read-only view. Use `as.duckdt(x, copy = TRUE)` ",
      "or `duckdt(conn, table)` pointing at a real DuckDB table.",
      call. = FALSE
    )
  }
  if (!isTRUE(x$writable)) {
    stop(
      "duckdt: `duckdt_merge()` requires a writable handle. Handles from `duckdt(conn, table)` ",
      "are read-only by default to avoid accidental writes -- pass `writable = TRUE` ",
      "to allow this, e.g. `duckdt(conn, table, writable = TRUE)`.",
      call. = FALSE
    )
  }
  if (!update && !insert && !delete) {
    stop("duckdt_merge: at least one of `update`, `insert`, `delete` must be TRUE.", call. = FALSE)
  }

  conn <- x$conn
  y_is_duckdt <- inherits(y, "duckdt")
  if (y_is_duckdt) {
    if (!identical(y$conn, conn)) {
      stop("duckdt_merge: `y` must be a duckdt object on the same connection as `x`.", call. = FALSE)
    }
  } else if (!is.data.frame(y)) {
    stop("duckdt_merge: `y` must be a data.frame/data.table or a duckdt object.", call. = FALSE)
  }

  x_cols <- duckdt_columns(x)
  y_cols <- if (y_is_duckdt) duckdt_columns(y) else names(y)

  if (is.null(by)) {
    by <- intersect(x_cols, y_cols)
    if (length(by) == 0) {
      stop("duckdt_merge: `x` and `y` share no columns to merge `by`; specify `by=`.", call. = FALSE)
    }
  } else {
    missing_x <- setdiff(by, x_cols)
    missing_y <- setdiff(by, y_cols)
    if (length(missing_x) > 0) {
      stop("duckdt_merge: `by` column(s) not found in `x`: ", paste(missing_x, collapse = ", "), call. = FALSE)
    }
    if (length(missing_y) > 0) {
      stop("duckdt_merge: `by` column(s) not found in `y`: ", paste(missing_y, collapse = ", "), call. = FALSE)
    }
  }

  update_cols <- setdiff(intersect(x_cols, y_cols), by)
  dialect <- duckdt_dialect(conn)
  tmp_name <- paste0(x$tbl, "__duckdt_merge_tmp")

  duckdt_merge_stage(conn, y, y_is_duckdt, c(by, update_cols), tmp_name, dialect)
  on.exit(duckdt_merge_drop_tmp(conn, tmp_name, dialect), add = TRUE)

  if (dialect == "mssql") {
    duckdt_merge_exec_mssql(x, tmp_name, by, update_cols, update, insert, delete)
  } else {
    duckdt_merge_exec_duckdb(x, tmp_name, by, update_cols, update, insert, delete)
  }

  invisible(x)
}

# Materialize `y`'s `cols` into a staging table named `tmp_name` on `conn`.
duckdt_merge_stage <- function(conn, y, y_is_duckdt, cols, tmp_name, dialect) {
  qtmp <- DBI::dbQuoteIdentifier(conn, tmp_name)
  qcols <- vapply(cols, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
  select_list <- paste(qcols, collapse = ", ")

  if (dialect == "mssql") {
    if (y_is_duckdt) {
      qtmp_str <- DBI::dbQuoteString(conn, tmp_name)
      DBI::dbExecute(conn, paste0("IF OBJECT_ID(", qtmp_str, ", 'U') IS NOT NULL DROP TABLE ", qtmp))
      DBI::dbExecute(conn, paste0(
        "SELECT ", select_list, " INTO ", qtmp, " FROM ", duckdt_qtbl(y)
      ))
    } else {
      DBI::dbWriteTable(conn, tmp_name, as.data.frame(y)[cols], overwrite = TRUE)
    }
  } else {
    if (y_is_duckdt) {
      DBI::dbExecute(conn, paste0(
        "CREATE OR REPLACE TEMP TABLE ", qtmp, " AS SELECT ", select_list,
        " FROM ", duckdt_qtbl(y)
      ))
    } else {
      src_name <- paste0(tmp_name, "_src")
      qsrc <- DBI::dbQuoteIdentifier(conn, src_name)
      duckdb::duckdb_register(conn, src_name, as.data.frame(y)[cols])
      on.exit(try(duckdb::duckdb_unregister(conn, src_name), silent = TRUE), add = TRUE)
      DBI::dbExecute(conn, paste0(
        "CREATE OR REPLACE TEMP TABLE ", qtmp, " AS SELECT ", select_list, " FROM ", qsrc
      ))
    }
  }
  invisible(NULL)
}

duckdt_merge_drop_tmp <- function(conn, tmp_name, dialect) {
  qtmp <- DBI::dbQuoteIdentifier(conn, tmp_name)
  if (dialect == "mssql") {
    qtmp_str <- DBI::dbQuoteString(conn, tmp_name)
    try(DBI::dbExecute(conn, paste0("IF OBJECT_ID(", qtmp_str, ", 'U') IS NOT NULL DROP TABLE ", qtmp)), silent = TRUE)
  } else {
    try(DBI::dbExecute(conn, paste0("DROP TABLE IF EXISTS ", qtmp)), silent = TRUE)
  }
}

# DuckDB has no native MERGE, so it's emulated with three statements --
# UPDATE ... FROM for matches, an anti-join INSERT for `y`-only rows, and (if
# `delete`) an anti-join DELETE for `x`-only rows -- wrapped in a transaction
# since it's multi-statement (mirroring duckdt_rebuild_table_mssql()'s use of
# a transaction for its multi-statement rebuild).
duckdt_merge_exec_duckdb <- function(x, tmp_name, by, update_cols, update, insert, delete) {
  conn <- x$conn
  qtbl <- duckdt_qtbl(x)
  qtmp <- DBI::dbQuoteIdentifier(conn, tmp_name)
  qby <- vapply(by, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
  match_sql <- paste(sprintf("%s.%s = %s.%s", qtbl, qby, qtmp, qby), collapse = " AND ")

  DBI::dbBegin(conn)
  tryCatch({
    if (update && length(update_cols) > 0) {
      qup <- vapply(update_cols, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
      set_list <- paste(sprintf("%s = %s.%s", qup, qtmp, qup), collapse = ", ")
      DBI::dbExecute(conn, paste0(
        "UPDATE ", qtbl, " SET ", set_list, " FROM ", qtmp, " WHERE ", match_sql
      ))
    }
    if (insert) {
      all_cols <- c(by, update_cols)
      qall <- vapply(all_cols, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
      col_list <- paste(qall, collapse = ", ")
      DBI::dbExecute(conn, paste0(
        "INSERT INTO ", qtbl, " (", col_list, ") SELECT ", col_list, " FROM ", qtmp,
        " WHERE NOT EXISTS (SELECT 1 FROM ", qtbl, " WHERE ", match_sql, ")"
      ))
    }
    if (delete) {
      DBI::dbExecute(conn, paste0(
        "DELETE FROM ", qtbl, " WHERE NOT EXISTS (SELECT 1 FROM ", qtmp, " WHERE ", match_sql, ")"
      ))
    }
    DBI::dbCommit(conn)
  }, error = function(e) {
    DBI::dbRollback(conn)
    stop(e)
  })
}

# MS SQL Server has a native MERGE statement, so the whole reconciliation
# compiles to a single statement against the staged table.
duckdt_merge_exec_mssql <- function(x, tmp_name, by, update_cols, update, insert, delete) {
  conn <- x$conn
  qtbl <- duckdt_qtbl(x)
  qtmp <- DBI::dbQuoteIdentifier(conn, tmp_name)
  qby <- vapply(by, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
  on_clause <- paste(sprintf("t.%s = s.%s", qby, qby), collapse = " AND ")

  clauses <- character(0)
  if (update && length(update_cols) > 0) {
    qup <- vapply(update_cols, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
    set_list <- paste(sprintf("t.%s = s.%s", qup, qup), collapse = ", ")
    clauses <- c(clauses, paste0("WHEN MATCHED THEN UPDATE SET ", set_list))
  }
  if (insert) {
    all_cols <- c(by, update_cols)
    qall <- vapply(all_cols, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
    clauses <- c(clauses, sprintf(
      "WHEN NOT MATCHED BY TARGET THEN INSERT (%s) VALUES (%s)",
      paste(qall, collapse = ", "),
      paste(sprintf("s.%s", qall), collapse = ", ")
    ))
  }
  if (delete) {
    clauses <- c(clauses, "WHEN NOT MATCHED BY SOURCE THEN DELETE")
  }

  sql <- sprintf(
    "MERGE INTO %s AS t USING %s AS s ON (%s) %s;",
    qtbl, qtmp, on_clause, paste(clauses, collapse = " ")
  )
  DBI::dbExecute(conn, sql)
}
