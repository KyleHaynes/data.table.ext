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

  conn <- x$conn
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

  where_sql <- if (has_i) translate_expr(ie, cols, env, conn) else NULL

  for (col_name in names(exprs)) {
    val_sql <- translate_expr(exprs[[col_name]], cols, env, conn)
    qcol <- DBI::dbQuoteIdentifier(conn, col_name)
    col_exists <- col_name %in% cols

    new_val <- if (is.null(where_sql)) {
      val_sql
    } else if (col_exists) {
      paste0("CASE WHEN ", where_sql, " THEN ", val_sql, " ELSE ", qcol, " END")
    } else {
      paste0("CASE WHEN ", where_sql, " THEN ", val_sql, " ELSE NULL END")
    }

    select_cols <- if (col_exists) {
      paste0("* EXCLUDE (", qcol, "), ", new_val, " AS ", qcol)
    } else {
      paste0("*, ", new_val, " AS ", qcol)
    }

    sql <- paste0(
      "CREATE OR REPLACE TABLE ", duckdt_qtbl(x), " AS SELECT ", select_cols,
      " FROM ", duckdt_qtbl(x)
    )
    DBI::dbExecute(conn, sql)
    if (!col_exists) cols <- c(cols, col_name)
  }

  invisible(x)
}
