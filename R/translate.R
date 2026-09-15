# Internal: translate captured (unevaluated) R expressions into SQL fragments.
# Columns are resolved against `cols`; any other symbol is pulled as a literal
# value from `env` (the caller's frame), mirroring data.table's own scoping.
#
# Every translator takes a `dialect` (see dialect.R, `"duckdb"` or `"mssql"`)
# so the same expression tree can be rendered as either engine's SQL.

is_column <- function(name, cols) name %in% cols

translate_literal <- function(val, conn, dialect) {
  if (is.null(val)) return("NULL")
  if (length(val) > 1) {
    return(paste(vapply(val, translate_literal, character(1), conn = conn, dialect = dialect), collapse = ", "))
  }
  if (is.na(val)) return("NULL")
  if (is.character(val) || is.factor(val)) {
    return(as.character(DBI::dbQuoteString(conn, as.character(val))))
  }
  if (is.logical(val)) {
    if (dialect == "mssql") return(if (val) "1" else "0")
    return(if (val) "TRUE" else "FALSE")
  }
  if (inherits(val, "Date")) return(as.character(DBI::dbQuoteString(conn, format(val))))
  as.character(val)
}

FN_MAP <- list(
  duckdb = c(
    mean = "avg", sum = "sum", min = "min", max = "max",
    sd = "stddev", var = "variance", median = "median",
    length = "count", toupper = "upper", tolower = "lower",
    nchar = "length", abs = "abs", round = "round"
  ),
  # `median` is deliberately absent here -- MS SQL Server has no MEDIAN()
  # aggregate, so it's special-cased in translate_call_default() instead.
  mssql = c(
    mean = "avg", sum = "sum", min = "min", max = "max",
    sd = "stdev", var = "var",
    length = "count", toupper = "upper", tolower = "lower",
    nchar = "len", abs = "abs", round = "round"
  )
)

binop <- function(expr, sqlop, cols, env, conn, dialect) {
  lhs <- translate_expr(expr[[2]], cols, env, conn, dialect)
  rhs <- translate_expr(expr[[3]], cols, env, conn, dialect)
  paste(lhs, sqlop, rhs)
}

arith <- function(expr, sqlop, cols, env, conn, dialect) {
  if (length(expr) == 2) {
    return(paste0(sqlop, translate_expr(expr[[2]], cols, env, conn, dialect)))
  }
  binop(expr, sqlop, cols, env, conn, dialect)
}

translate_in <- function(expr, cols, env, conn, dialect) {
  lhs <- translate_expr(expr[[2]], cols, env, conn, dialect)
  vals <- eval(expr[[3]], envir = env)
  vals_sql <- vapply(vals, translate_literal, character(1), conn = conn, dialect = dialect)
  paste0(lhs, " IN (", paste(vals_sql, collapse = ", "), ")")
}

translate_between <- function(expr, cols, env, conn, dialect) {
  lhs <- translate_expr(expr[[2]], cols, env, conn, dialect)
  bounds <- eval(expr[[3]], envir = env)
  paste0(lhs, " BETWEEN ", translate_literal(bounds[1], conn, dialect),
         " AND ", translate_literal(bounds[2], conn, dialect))
}

# %like%/%ilike%/%plike% are regex matches in data.table (not SQL LIKE wildcard
# syntax), so they map onto a regex function rather than the SQL LIKE
# keyword: DuckDB's regexp_matches(), or (as of SQL Server 2025 / Azure SQL
# DB, Fabric SQL DB -- database compatibility level 170+) T-SQL's native
# REGEXP_LIKE(). Both engines happen to use Google's RE2 as their regex
# engine, so the same pattern text works on either and neither supports PCRE
# backreferences/lookaround -- %plike% is a best-effort alias of %like% on
# both. Against an older SQL Server (compat level < 170, no REGEXP_LIKE),
# this will surface as a plain "not a recognized built-in function name"
# error from the server itself.
translate_regex_like <- function(expr, cols, env, conn, dialect, options = NULL) {
  lhs <- translate_expr(expr[[2]], cols, env, conn, dialect)
  pattern_sql <- translate_literal(eval(expr[[3]], envir = env), conn, dialect)
  fn <- if (dialect == "mssql") "REGEXP_LIKE" else "regexp_matches"
  if (is.null(options)) {
    paste0(fn, "(", lhs, ", ", pattern_sql, ")")
  } else {
    paste0(fn, "(", lhs, ", ", pattern_sql, ", ", translate_literal(options, conn, dialect), ")")
  }
}

# %flike% is a fixed (literal) substring match, i.e. grepl(..., fixed = TRUE).
translate_flike <- function(expr, cols, env, conn, dialect) {
  lhs <- translate_expr(expr[[2]], cols, env, conn, dialect)
  pattern_sql <- translate_literal(eval(expr[[3]], envir = env), conn, dialect)
  if (dialect == "mssql") {
    # CHARINDEX does a plain substring search, so the pattern needs no
    # LIKE-wildcard escaping to stay a literal match.
    paste0("CHARINDEX(", pattern_sql, ", ", lhs, ") > 0")
  } else {
    paste0("contains(", lhs, ", ", pattern_sql, ")")
  }
}

translate_call_default <- function(expr, fn, cols, env, conn, dialect) {
  if (fn == "median" && dialect == "mssql") {
    arg_sql <- translate_expr(expr[[2]], cols, env, conn, dialect)
    return(paste0("PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", arg_sql, ")"))
  }
  sql_fn <- FN_MAP[[dialect]][[fn]]
  if (is.null(sql_fn)) sql_fn <- toupper(fn)
  args <- lapply(as.list(expr)[-1], translate_expr, cols = cols, env = env, conn = conn, dialect = dialect)
  paste0(sql_fn, "(", paste(unlist(args), collapse = ", "), ")")
}

translate_expr <- function(expr, cols, env, conn, dialect) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm == ".N") return("count(*)")
    if (is_column(nm, cols)) return(as.character(DBI::dbQuoteIdentifier(conn, nm)))
    val <- tryCatch(
      get(nm, envir = env),
      error = function(e) {
        stop(sprintf(
          "duckdt: could not resolve `%s` (not a column and not found in the calling environment)",
          nm
        ), call. = FALSE)
      }
    )
    return(translate_literal(val, conn, dialect))
  }

  if (is.atomic(expr) && length(expr) >= 1) {
    return(translate_literal(expr, conn, dialect))
  }

  if (is.call(expr)) {
    fn <- as.character(expr[[1]])
    switch(fn,
      "(" = paste0("(", translate_expr(expr[[2]], cols, env, conn, dialect), ")"),
      "==" = binop(expr, "=", cols, env, conn, dialect),
      "!=" = binop(expr, "<>", cols, env, conn, dialect),
      ">"  = binop(expr, ">", cols, env, conn, dialect),
      ">=" = binop(expr, ">=", cols, env, conn, dialect),
      "<"  = binop(expr, "<", cols, env, conn, dialect),
      "<=" = binop(expr, "<=", cols, env, conn, dialect),
      "&"  = ,
      "&&" = binop(expr, "AND", cols, env, conn, dialect),
      "|"  = ,
      "||" = binop(expr, "OR", cols, env, conn, dialect),
      "!"  = paste0("NOT (", translate_expr(expr[[2]], cols, env, conn, dialect), ")"),
      "%in%" = translate_in(expr, cols, env, conn, dialect),
      "%chin%" = translate_in(expr, cols, env, conn, dialect),
      "%between%" = translate_between(expr, cols, env, conn, dialect),
      "%like%" = translate_regex_like(expr, cols, env, conn, dialect),
      "%ilike%" = translate_regex_like(expr, cols, env, conn, dialect, options = "i"),
      "%flike%" = translate_flike(expr, cols, env, conn, dialect),
      "%plike%" = translate_regex_like(expr, cols, env, conn, dialect),
      "is.na" = paste0(translate_expr(expr[[2]], cols, env, conn, dialect), " IS NULL"),
      "+" = arith(expr, "+", cols, env, conn, dialect),
      "-" = arith(expr, "-", cols, env, conn, dialect),
      "*" = arith(expr, "*", cols, env, conn, dialect),
      "/" = arith(expr, "/", cols, env, conn, dialect),
      translate_call_default(expr, fn, cols, env, conn, dialect)
    )
  } else {
    stop("duckdt: unsupported expression: ", deparse(expr), call. = FALSE)
  }
}

# `j` translation: missing -> *, bare column, or .()/list(...) with named/computed entries.
translate_select <- function(expr, cols, env, conn, dialect) {
  if (is.call(expr) && as.character(expr[[1]]) %in% c(".", "list")) {
    args <- as.list(expr)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    parts <- character(length(args))
    aliases <- character(length(args))
    for (i in seq_along(args)) {
      a <- args[[i]]
      if (is.symbol(a) && as.character(a) == ".N") {
        sql <- "count(*)"
      } else {
        sql <- translate_expr(a, cols, env, conn, dialect)
      }
      if (nzchar(nms[i])) {
        alias <- nms[i]
        parts[i] <- paste0(sql, " AS ", DBI::dbQuoteIdentifier(conn, alias))
      } else if (is.symbol(a) && is_column(as.character(a), cols)) {
        alias <- as.character(a)
        parts[i] <- sql
      } else if (is.symbol(a) && as.character(a) == ".N") {
        alias <- "N"
        parts[i] <- paste0(sql, " AS ", DBI::dbQuoteIdentifier(conn, alias))
      } else {
        alias <- deparse(a)
        parts[i] <- paste0(sql, " AS ", DBI::dbQuoteIdentifier(conn, alias))
      }
      aliases[i] <- alias
    }
    return(list(parts = parts, aliases = aliases))
  }

  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm == ".N") {
      return(list(parts = paste0("count(*) AS ", DBI::dbQuoteIdentifier(conn, "N")), aliases = "N"))
    }
    if (is_column(nm, cols)) {
      return(list(parts = as.character(DBI::dbQuoteIdentifier(conn, nm)), aliases = nm))
    }
  }

  stop("duckdt: unsupported `j` expression: ", deparse(expr),
       ". Use `.(col1, col2 = expr)` to select/compute columns.", call. = FALSE)
}

# `by` translation: character vector, bare column, or .()/list(...)/c(...).
translate_by <- function(expr, cols, env, conn, dialect) {
  if (is.character(expr)) {
    aliases <- expr
    parts <- vapply(expr, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
    return(list(parts = unname(parts), aliases = aliases))
  }

  if (is.call(expr) && as.character(expr[[1]]) %in% c(".", "list", "c")) {
    args <- as.list(expr)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    parts <- character(length(args))
    aliases <- character(length(args))
    for (i in seq_along(args)) {
      a <- args[[i]]
      if (is.character(a) && length(a) == 1 && !nzchar(nms[i])) {
        # e.g. by = c("cyl", "gear") -- a string naming a column, not a value
        alias <- a
        parts[i] <- as.character(DBI::dbQuoteIdentifier(conn, a))
      } else {
        sql <- translate_expr(a, cols, env, conn, dialect)
        alias <- if (nzchar(nms[i])) nms[i] else if (is.symbol(a)) as.character(a) else deparse(a)
        parts[i] <- if (nzchar(nms[i])) paste0(sql, " AS ", DBI::dbQuoteIdentifier(conn, alias)) else sql
      }
      aliases[i] <- alias
    }
    return(list(parts = parts, aliases = aliases))
  }

  if (is.symbol(expr)) {
    nm <- as.character(expr)
    return(list(parts = as.character(DBI::dbQuoteIdentifier(conn, nm)), aliases = nm))
  }

  stop("duckdt: unsupported `by` expression: ", deparse(expr), call. = FALSE)
}
