# Internal: implements `d[i, col := expr]`, `d[, `:=`(col1 = e1, col2 = e2)]`
# and `d[, col := NULL]` as in-place changes to the table: UPDATE for a
# column that exists, ALTER TABLE ... ADD then UPDATE for a new one, ALTER
# TABLE ... DROP COLUMN for NULL. Unlike rebuilding the table, this keeps its
# primary key, constraints, indexes and permissions; works on a table other
# tables reference; and with `i`, touches only the matching rows.
#
# Every assignment is translated before anything is written, so an
# expression that can't be translated changes nothing, and the writes run in
# one transaction, so they land together or not at all. As in the rest of
# duckdt (and unlike data.table, which evaluates all right-hand sides first),
# a later assignment in the same call sees an earlier one's result.
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
  exprs <- duckdt_assignments(je, env)

  where_sql <- if (has_i) translate_condition(ie, cols, env, conn, dialect) else NULL

  steps <- list()
  for (col_name in names(exprs)) {
    e <- exprs[[col_name]]
    exists <- col_name %in% cols
    if (is.null(e)) {
      if (has_i) {
        stop("duckdt: `:= NULL` deletes a whole column, so it can't be combined with `i`.", call. = FALSE)
      }
      if (!exists) {
        stop(sprintf("duckdt: can't delete column '%s': there is no such column.", col_name), call. = FALSE)
      }
      steps[[length(steps) + 1L]] <- list(kind = "drop", col = col_name)
      cols <- setdiff(cols, col_name)
      next
    }
    val <- translate_value(e, cols, env, conn, dialect, bit = TRUE)
    kind <- if (!exists) "add" else if (is.null(where_sql) && dialect != "mssql") "replace" else "update"
    steps[[length(steps) + 1L]] <- list(
      kind = kind, col = col_name, val = val,
      type = duckdt_literal_type(e, cols, env, dialect),
      refs = intersect(all.names(e), cols)
    )
    if (!exists) cols <- c(cols, col_name)
  }

  # DuckDB can't commit a transaction that changes a table's columns after
  # updating its rows, so every schema change (ADD, DROP, a change of type)
  # runs first and the UPDATEs follow, each phase in assignment order. That
  # order matches the one written as long as a column being dropped isn't
  # used by another assignment in the same call -- checked here, before
  # anything is written.
  dropped <- vapply(steps, function(s) if (s$kind == "drop") s$col else NA_character_, "")
  used <- unlist(lapply(steps, `[[`, "refs"))
  clash <- intersect(stats::na.omit(dropped), used)
  if (length(clash)) {
    stop(sprintf(
      "duckdt: column '%s' is dropped and used in the same `:=`; drop it in a separate call.",
      clash[1]
    ), call. = FALSE)
  }

  on.exit(duckdt_meta_reset(x), add = TRUE)
  duckdt_transaction(conn, {
    pending <- list()
    assigned <- character()
    for (s in steps) {
      if (duckdt_mutate_ddl(x, s, dialect, assigned)) pending[[length(pending) + 1L]] <- s
      assigned <- c(assigned, s$col)
    }
    where <- if (is.null(where_sql)) "" else paste0(" WHERE ", where_sql)
    for (s in pending) {
      duckdt_execute(conn, paste0(
        "UPDATE ", duckdt_qtbl(x), " SET ", DBI::dbQuoteIdentifier(conn, s$col), " = ", s$val, where
      ))
    }
  })
  invisible(x)
}

# The column => expression list from the `j` of a `:=` call. `col := expr`,
# `"col" := expr`, `(name_var) := expr` and `:=`(a = e1, b = e2) are all
# data.table spellings.
duckdt_assignments <- function(je, env) {
  args <- as.list(je)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  if (length(args) == 2 && !any(nzchar(nms))) {
    lhs <- args[[1]]
    col_name <- if (is.symbol(lhs)) as.character(lhs) else as.character(eval(lhs, envir = env))
    if (length(col_name) != 1L) {
      stop("duckdt: assign one column at a time, or use `:=`(a = ..., b = ...).", call. = FALSE)
    }
    out <- list(args[[2]])
    names(out) <- col_name
    return(out)
  }
  if (!all(nzchar(nms))) {
    stop("duckdt: `:=`(...) requires named arguments, e.g. `:=`(col1 = expr1, col2 = expr2).", call. = FALSE)
  }
  args
}

# The schema change one assignment needs, if any. TRUE if it still needs an
# UPDATE to fill in its values. `assigned` are the columns earlier
# assignments in the same call write.
duckdt_mutate_ddl <- function(x, s, dialect, assigned) {
  conn <- x$conn
  qtbl <- duckdt_qtbl(x)
  qcol <- DBI::dbQuoteIdentifier(conn, s$col)

  if (s$kind == "drop") {
    duckdt_execute(conn, paste0("ALTER TABLE ", qtbl, " DROP COLUMN ", qcol))
    return(FALSE)
  }
  if (s$kind == "add") {
    # The type is worked out now, after any earlier assignment's column has
    # been added, so a value built on one (`b = a * 2`) can be described.
    type <- if (is.null(s$type)) duckdt_expr_type(x, s$val, dialect) else s$type
    if (dialect == "mssql") {
      type <- duckdt_mssql_widen(type)
      duckdt_execute(conn, paste0("ALTER TABLE ", qtbl, " ADD ", qcol, " ", type, " NULL"))
    } else {
      duckdt_execute(conn, paste0("ALTER TABLE ", qtbl, " ADD COLUMN ", qcol, " ", type))
    }
    return(TRUE)
  }
  if (s$kind == "replace") {
    # Assigning a whole column replaces it, type and all, in data.table: a
    # numeric expression into an integer column gives a numeric column. An
    # UPDATE would convert the values to the old type instead, so when the
    # type changes DuckDB's ALTER ... SET DATA TYPE ... USING does the
    # replacing -- as a schema change, so before the call's UPDATEs.
    type <- if (is.null(s$type)) duckdt_expr_type(x, s$val, dialect) else s$type
    old <- duckdt_meta(x)
    old_type <- old$type[match(s$col, old$column)]
    if (!identical(toupper(type), toupper(old_type))) {
      if (any(s$refs %in% assigned)) {
        stop(sprintf(paste0(
          "duckdt: changing the type of '%s' uses a column assigned earlier in the same `:=`; ",
          "assign them in separate calls."
        ), s$col), call. = FALSE)
      }
      duckdt_execute(conn, paste0(
        "ALTER TABLE ", qtbl, " ALTER COLUMN ", qcol, " SET DATA TYPE ", type,
        " USING (", s$val, ")"
      ))
      return(FALSE)
    }
  }
  TRUE
}

# The SQL type an expression evaluates to over this table, asked of the
# database itself so it is exactly what a column holding it needs: DuckDB's
# DESCRIBE, or on SQL Server sys.dm_exec_describe_first_result_set(), which
# describes a query without running it.
duckdt_expr_type <- function(x, val, dialect) {
  conn <- x$conn
  probe <- paste0("SELECT ", val, " AS duckdt_value FROM ", duckdt_qtbl(x))
  type <- if (dialect == "mssql") {
    duckdt_get_query(conn, paste0(
      "SELECT system_type_name FROM sys.dm_exec_describe_first_result_set(N",
      DBI::dbQuoteString(conn, probe), ", NULL, 0) WHERE column_ordinal = 1"
    ))[[1]]
  } else {
    duckdt_get_query(conn, paste0("DESCRIBE ", probe))[[2]]
  }
  type <- type[!is.na(type)]
  if (!length(type)) {
    stop("duckdt: could not work out the type of the assigned value.", call. = FALSE)
  }
  # A bare NULL describes as SQL's NULL type, which no column can have.
  if (toupper(type[1]) == "NULL") return(if (dialect == "mssql") "int" else "INTEGER")
  type[1]
}

# The column type for an R literal, or an R variable holding one, assigned
# as is: R's own type, which is what data.table would give the column. The
# database would describe `0.5` as a one-digit decimal and `"no"` as two
# characters wide, so a later `d[i, col := 12.25]` or `:= "yes"` wouldn't fit.
# NULL for anything else, whose type the database works out.
duckdt_literal_type <- function(e, cols, env, dialect) {
  if (is.symbol(e) && !is_column(as.character(e), cols) && as.character(e) != ".N") {
    e <- tryCatch(get(as.character(e), envir = env), error = function(err) NULL)
  }
  if (!is.atomic(e) || length(e) != 1L || is.object(e)) return(NULL)
  mssql <- dialect == "mssql"
  if (is.logical(e)) return(if (mssql) "bit" else "BOOLEAN")
  if (is.double(e)) return(if (mssql) "float" else "DOUBLE")
  if (is.character(e)) return(if (mssql) "nvarchar(4000)" else "VARCHAR")
  NULL
}

# SQL Server describes text by the width of the value at hand: CONCAT('a',
# x) might be varchar(51). A column added at that width can't take a longer
# value later, so text columns are given the widest width that still stores
# in the row; `max` types are left alone.
duckdt_mssql_widen <- function(type) {
  t <- tolower(type)
  if (grepl("^n(var)?char\\(\\d+\\)$", t)) return("nvarchar(4000)")
  if (grepl("^(var)?char\\(\\d+\\)$", t)) return("varchar(8000)")
  type
}

# Run `code` in a transaction: committed if it finishes, rolled back (and
# the error passed on) if it doesn't.
duckdt_transaction <- function(conn, code) {
  DBI::dbBegin(conn)
  tryCatch({
    force(code)
    DBI::dbCommit(conn)
  }, error = function(e) {
    try(DBI::dbRollback(conn), silent = TRUE)
    stop(e)
  })
  invisible()
}
