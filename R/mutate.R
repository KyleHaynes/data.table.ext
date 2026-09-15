# Internal: implements `d[i, col := expr]` / `d[, `:=`(col1 = e1, col2 = e2)]`
# by rebuilding the underlying table with CREATE OR REPLACE TABLE, which lets
# DuckDB infer column types instead of us having to.
duckdt_mutate <- function(x, ie, je, has_i, has_by, cols, env) {
  if (has_by) {
    stop("duckdt: grouped `:=` (i.e. `by=` together with `:=`) is not supported yet.", call. = FALSE)
  }
  if (!isTRUE(x$materialized)) {
    stop(
      "duckdt: `:=` requires a materialized table since it mutates data in place. ",
      "This handle is backed by a read-only view. Use `as.duckdt(x, copy = TRUE)` ",
      "or `duckdt(conn, table)` pointing at a real DuckDB table.",
      call. = FALSE
    )
  }
  if (!isTRUE(x$writable)) {
    stop(
      "duckdt: `:=` requires a writable handle. Handles from `duckdt(conn, table)` ",
      "are read-only by default to avoid accidental writes -- pass `writable = TRUE` ",
      "to allow this, e.g. `duckdt(conn, table, writable = TRUE)`.",
      call. = FALSE
    )
  }

  conn <- x$conn
  dialect <- duckdt_dialect(conn)
  args <- as.list(je)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))

  if (length(args) == 2 && !any(nzchar(nms))) {
    col_name <- if (is.symbol(args[[1]])) as.character(args[[1]]) else as.character(eval(args[[1]], envir = env))
    exprs <- stats::setNames(list(args[[2]]), col_name)
  } else {
    if (!all(nzchar(nms))) {
      stop("duckdt: `:=`(...) requires named arguments, e.g. `:=`(col1 = expr1, col2 = expr2).", call. = FALSE)
    }
    exprs <- stats::setNames(args, nms)
  }

  where_sql <- if (has_i) translate_expr(ie, cols, env, conn, dialect) else NULL

  for (col_name in names(exprs)) {
    val_sql <- translate_expr(exprs[[col_name]], cols, env, conn, dialect)
    qcol <- DBI::dbQuoteIdentifier(conn, col_name)
    col_exists <- col_name %in% cols

    new_val <- if (is.null(where_sql)) {
      val_sql
    } else if (col_exists) {
      paste0("CASE WHEN ", where_sql, " THEN ", val_sql, " ELSE ", qcol, " END")
    } else {
      paste0("CASE WHEN ", where_sql, " THEN ", val_sql, " ELSE NULL END")
    }

    if (dialect == "mssql") {
      duckdt_rebuild_table_mssql(x, cols, col_name, col_exists, new_val, qcol)
    } else {
      duckdt_rebuild_table_duckdb(x, col_exists, new_val, qcol)
    }
    if (!col_exists) cols <- c(cols, col_name)
  }

  invisible(x)
}

duckdt_rebuild_table_duckdb <- function(x, col_exists, new_val, qcol) {
  select_cols <- if (col_exists) {
    paste0("* EXCLUDE (", qcol, "), ", new_val, " AS ", qcol)
  } else {
    paste0("*, ", new_val, " AS ", qcol)
  }
  # A TEMP table has to be recreated as TEMP: an unqualified CREATE TABLE
  # lands in `main`, where it would be shadowed by the temp table of the same
  # name that every later query still resolves to -- so the `:=` would look
  # like it silently did nothing.
  create <- if (isTRUE(x$temporary)) "CREATE OR REPLACE TEMP TABLE " else "CREATE OR REPLACE TABLE "
  sql <- paste0(
    create, duckdt_qtbl(x), " AS SELECT ", select_cols,
    " FROM ", duckdt_qtbl(x)
  )
  DBI::dbExecute(x$conn, sql)
}

# MS SQL Server has neither `CREATE OR REPLACE TABLE` nor `* EXCLUDE(...)`, so
# `:=` is emulated by building an explicit column list (from the already-known
# `cols`), `SELECT ... INTO` a temp table (which lets SQL Server infer the new
# column's type, mirroring the DuckDB path's type-inference intent), then
# swapping it in for the original. This is a multi-statement rebuild -- unlike
# the single-statement DuckDB path -- so it's wrapped in a transaction for
# safety, though it still isn't a true atomic replace (e.g. permissions or
# triggers on the original table are not preserved, same caveat the DuckDB
# drop+recreate path already implicitly carries).
duckdt_rebuild_table_mssql <- function(x, cols, col_name, col_exists, new_val, qcol) {
  conn <- x$conn
  other_cols <- vapply(
    cols[cols != col_name],
    function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)),
    character(1)
  )
  select_list <- c(other_cols, paste0(new_val, " AS ", qcol))

  tmp_name <- paste0(x$tbl, "__duckdt_tmp")
  qtmp <- DBI::dbQuoteIdentifier(conn, tmp_name)
  qtmp_str <- DBI::dbQuoteString(conn, tmp_name)
  qtbl_str <- DBI::dbQuoteString(conn, x$tbl)

  DBI::dbBegin(conn)
  tryCatch({
    DBI::dbExecute(conn, paste0(
      "IF OBJECT_ID(", qtmp_str, ", 'U') IS NOT NULL DROP TABLE ", qtmp
    ))
    DBI::dbExecute(conn, paste0(
      "SELECT ", paste(select_list, collapse = ", "), " INTO ", qtmp,
      " FROM ", duckdt_qtbl(x)
    ))
    DBI::dbExecute(conn, paste0("DROP TABLE ", duckdt_qtbl(x)))
    DBI::dbExecute(conn, paste0("EXEC sp_rename ", qtmp_str, ", ", qtbl_str))
    DBI::dbCommit(conn)
  }, error = function(e) {
    DBI::dbRollback(conn)
    stop(e)
  })
}
