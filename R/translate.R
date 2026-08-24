# Internal: translate captured (unevaluated) R expressions into SQL fragments.
# Columns are resolved against `cols`; any other symbol is pulled as a literal
# value from `env` (the caller's frame), mirroring data.table's own scoping.

is_column <- function(name, cols) name %in% cols

translate_literal <- function(val, conn) {
  if (is.null(val)) return("NULL")
  if (length(val) > 1) {
    return(paste(vapply(val, translate_literal, character(1), conn = conn), collapse = ", "))
  }
  if (is.na(val)) return("NULL")
  if (is.character(val) || is.factor(val)) {
    return(as.character(DBI::dbQuoteString(conn, as.character(val))))
  }
  if (is.logical(val)) return(if (val) "TRUE" else "FALSE")
  if (inherits(val, "Date")) return(as.character(DBI::dbQuoteString(conn, format(val))))
  as.character(val)
}

FN_MAP <- c(
  mean = "avg", sum = "sum", min = "min", max = "max",
  sd = "stddev", var = "variance", median = "median",
  length = "count", toupper = "upper", tolower = "lower",
  nchar = "length", abs = "abs", round = "round"
)

binop <- function(expr, sqlop, cols, env, conn) {
  lhs <- translate_expr(expr[[2]], cols, env, conn)
  rhs <- translate_expr(expr[[3]], cols, env, conn)
  paste(lhs, sqlop, rhs)
}

arith <- function(expr, sqlop, cols, env, conn) {
  if (length(expr) == 2) {
    return(paste0(sqlop, translate_expr(expr[[2]], cols, env, conn)))
  }
  binop(expr, sqlop, cols, env, conn)
}

translate_in <- function(expr, cols, env, conn) {
  lhs <- translate_expr(expr[[2]], cols, env, conn)
  vals <- eval(expr[[3]], envir = env)
  vals_sql <- vapply(vals, translate_literal, character(1), conn = conn)
  paste0(lhs, " IN (", paste(vals_sql, collapse = ", "), ")")
}

translate_between <- function(expr, cols, env, conn) {
  lhs <- translate_expr(expr[[2]], cols, env, conn)
  bounds <- eval(expr[[3]], envir = env)
  paste0(lhs, " BETWEEN ", translate_literal(bounds[1], conn),
         " AND ", translate_literal(bounds[2], conn))
}

# %like%/%ilike%/%plike% are regex matches in data.table (not SQL LIKE wildcard
# syntax), so they map onto DuckDB's regexp_matches() rather than the SQL LIKE
# keyword. DuckDB's regex engine (RE2) doesn't support PCRE backreferences or
# lookaround, so %plike% is a best-effort alias of %like%.
translate_regex_like <- function(expr, cols, env, conn, options = NULL) {
  lhs <- translate_expr(expr[[2]], cols, env, conn)
  pattern_sql <- translate_literal(eval(expr[[3]], envir = env), conn)
  if (is.null(options)) {
    paste0("regexp_matches(", lhs, ", ", pattern_sql, ")")
  } else {
    paste0("regexp_matches(", lhs, ", ", pattern_sql, ", ", translate_literal(options, conn), ")")
  }
}

# %flike% is a fixed (literal) substring match, i.e. grepl(..., fixed = TRUE).
translate_flike <- function(expr, cols, env, conn) {
  lhs <- translate_expr(expr[[2]], cols, env, conn)
  pattern_sql <- translate_literal(eval(expr[[3]], envir = env), conn)
  paste0("contains(", lhs, ", ", pattern_sql, ")")
}

translate_call_default <- function(expr, fn, cols, env, conn) {
  sql_fn <- FN_MAP[[fn]]
  if (is.null(sql_fn)) sql_fn <- toupper(fn)
  args <- lapply(as.list(expr)[-1], translate_expr, cols = cols, env = env, conn = conn)
  paste0(sql_fn, "(", paste(unlist(args), collapse = ", "), ")")
}

translate_expr <- function(expr, cols, env, conn) {
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
    return(translate_literal(val, conn))
  }

  if (is.atomic(expr) && length(expr) >= 1) {
    return(translate_literal(expr, conn))
  }

  if (is.call(expr)) {
    fn <- as.character(expr[[1]])
    switch(fn,
      "(" = paste0("(", translate_expr(expr[[2]], cols, env, conn), ")"),
      "==" = binop(expr, "=", cols, env, conn),
      "!=" = binop(expr, "<>", cols, env, conn),
      ">"  = binop(expr, ">", cols, env, conn),
      ">=" = binop(expr, ">=", cols, env, conn),
      "<"  = binop(expr, "<", cols, env, conn),
      "<=" = binop(expr, "<=", cols, env, conn),
      "&"  = ,
      "&&" = binop(expr, "AND", cols, env, conn),
      "|"  = ,
      "||" = binop(expr, "OR", cols, env, conn),
      "!"  = paste0("NOT (", translate_expr(expr[[2]], cols, env, conn), ")"),
      "%in%" = translate_in(expr, cols, env, conn),
      "%chin%" = translate_in(expr, cols, env, conn),
      "%between%" = translate_between(expr, cols, env, conn),
      "%like%" = translate_regex_like(expr, cols, env, conn),
      "%ilike%" = translate_regex_like(expr, cols, env, conn, options = "i"),
      "%flike%" = translate_flike(expr, cols, env, conn),
      "%plike%" = translate_regex_like(expr, cols, env, conn),
      "is.na" = paste0(translate_expr(expr[[2]], cols, env, conn), " IS NULL"),
      "+" = arith(expr, "+", cols, env, conn),
      "-" = arith(expr, "-", cols, env, conn),
      "*" = arith(expr, "*", cols, env, conn),
      "/" = arith(expr, "/", cols, env, conn),
      translate_call_default(expr, fn, cols, env, conn)
    )
  } else {
    stop("duckdt: unsupported expression: ", deparse(expr), call. = FALSE)
  }
}

# `j` translation: missing -> *, bare column, or .()/list(...) with named/computed entries.
translate_select <- function(expr, cols, env, conn) {
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
        sql <- translate_expr(a, cols, env, conn)
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
translate_by <- function(expr, cols, env, conn) {
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
        sql <- translate_expr(a, cols, env, conn)
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
