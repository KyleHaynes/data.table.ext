# Internal: translate captured (unevaluated) R expressions into SQL fragments.
# Columns are resolved against `cols`; any other symbol is pulled as a literal
# value from `env` (the caller's frame), mirroring data.table's own scoping.
#
# Every translator takes a `dialect` (see dialect.R, `"duckdb"` or `"mssql"`)
# so the same expression tree can be rendered as either engine's SQL.
#
# T-SQL draws a line DuckDB doesn't: a comparison is a *predicate*, allowed
# after WHERE, WHEN, AND and NOT but not where a value goes -- `SELECT a > 1`
# and `SUM(a > 1)` are syntax errors -- while a bit column is a value, not a
# predicate, so `WHERE flag` is one too. translate_value() and
# translate_condition() bridge the two on SQL Server: a predicate used as a
# value becomes 1/0 (NULL when it is unknown, as R gives NA), and a value used
# as a condition becomes `(value) = 1`.

is_column <- function(name, cols) name %in% cols

translate_literal <- function(val, conn, dialect) {
  if (is.null(val) || !length(val)) return("NULL")
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
  # Both drivers hand R date-times to the database as UTC, so compare in UTC.
  # Milliseconds are the most SQL Server's `datetime` accepts in a literal.
  if (inherits(val, "POSIXt")) {
    return(as.character(DBI::dbQuoteString(
      conn, format(as.POSIXct(val), "%Y-%m-%d %H:%M:%OS3", tz = "UTC")
    )))
  }
  as.character(val)
}

# Functions that are a rename on each engine. Anything not here and not
# handled in translate_call() is sent as-is, upper-cased, which suits the
# many functions R and SQL share (sqrt(), exp(), floor(), coalesce(), ...).
FN_MAP <- list(
  duckdb = c(
    sum = "sum", min = "min", max = "max",
    sd = "stddev", var = "variance",
    length = "count", toupper = "upper", tolower = "lower",
    nchar = "length", abs = "abs", round = "round",
    trimws = "trim", pmin = "least", pmax = "greatest", fcoalesce = "coalesce",
    year = "year", month = "month", mday = "day", quarter = "quarter",
    hour = "hour", minute = "minute", second = "second", yday = "dayofyear",
    trunc = "trunc"
  ),
  mssql = c(
    sum = "sum", min = "min", max = "max",
    sd = "stdev", var = "var",
    length = "count", toupper = "upper", tolower = "lower",
    nchar = "len", abs = "abs",
    pmin = "least", pmax = "greatest", fcoalesce = "coalesce",
    year = "year", month = "month", mday = "day"
  )
)

# Calls whose SQL is a predicate rather than a value.
PREDICATE_FNS <- c(
  "==", "!=", ">", ">=", "<", "<=", "&", "&&", "|", "||", "!",
  "%in%", "%chin%", "%notin%", "%between%", "%like%", "%ilike%", "%flike%", "%plike%",
  "is.na", "between", "startsWith", "endsWith", "grepl"
)

# The function a call is calling, with any `pkg::` prefix dropped so that
# `data.table::fifelse(...)` translates like `fifelse(...)`.
call_name <- function(expr) {
  f <- expr[[1]]
  if (is.symbol(f)) return(as.character(f))
  if (is.call(f) && length(f) == 3 && as.character(f[[1]]) %in% c("::", ":::")) {
    return(as.character(f[[3]]))
  }
  stop("duckdt: unsupported expression: ", paste(deparse(expr), collapse = " "), call. = FALSE)
}

is_predicate <- function(expr) {
  if (!is.call(expr)) return(FALSE)
  fn <- call_name(expr)
  if (fn == "(") return(is_predicate(expr[[2]]))
  fn %in% PREDICATE_FNS
}

# SQL for `expr` where a value is wanted. `bit = TRUE` (a selected or grouped
# column, or an assigned one) makes a SQL Server predicate come back to R as a
# logical; inside an aggregate or arithmetic it stays an integer, which SUM()
# and AVG() accept and bit is not.
translate_value <- function(expr, cols, env, conn, dialect, bit = FALSE) {
  sql <- translate_expr(expr, cols, env, conn, dialect)
  if (dialect != "mssql" || !is_predicate(expr)) return(sql)
  val <- paste0("CASE WHEN ", sql, " THEN 1 WHEN NOT (", sql, ") THEN 0 END")
  if (bit) paste0("CAST(", val, " AS BIT)") else val
}

# SQL for `expr` where a condition is wanted (WHERE, WHEN, AND/OR, NOT).
translate_condition <- function(expr, cols, env, conn, dialect) {
  sql <- translate_expr(expr, cols, env, conn, dialect)
  if (dialect != "mssql" || is_predicate(expr)) return(sql)
  paste0("(", sql, ") = 1")
}

binop <- function(expr, sqlop, cols, env, conn, dialect) {
  lhs <- translate_value(expr[[2]], cols, env, conn, dialect)
  rhs <- translate_value(expr[[3]], cols, env, conn, dialect)
  paste(lhs, sqlop, rhs)
}

logic <- function(expr, sqlop, cols, env, conn, dialect) {
  lhs <- translate_condition(expr[[2]], cols, env, conn, dialect)
  rhs <- translate_condition(expr[[3]], cols, env, conn, dialect)
  paste(lhs, sqlop, rhs)
}

arith <- function(expr, sqlop, cols, env, conn, dialect) {
  if (length(expr) == 2) {
    return(paste0(sqlop, translate_value(expr[[2]], cols, env, conn, dialect)))
  }
  binop(expr, sqlop, cols, env, conn, dialect)
}

# R's `/` always gives a double. T-SQL's divides integers as integers, so
# `5 / 2` would be 2 there: make the left side a float first.
translate_divide <- function(expr, cols, env, conn, dialect) {
  lhs <- translate_value(expr[[2]], cols, env, conn, dialect)
  rhs <- translate_value(expr[[3]], cols, env, conn, dialect)
  if (dialect == "mssql") lhs <- paste0("CAST(", lhs, " AS FLOAT)")
  paste(lhs, "/", rhs)
}

translate_in <- function(expr, cols, env, conn, dialect, negate = FALSE) {
  lhs <- translate_value(expr[[2]], cols, env, conn, dialect)
  vals <- eval(expr[[3]], envir = env)
  if (!length(vals)) return(if (negate) "(1 = 1)" else "(1 = 0)")
  missing <- is.na(vals)
  vals_sql <- vapply(unique(vals[!missing]), translate_literal, character(1), conn = conn, dialect = dialect)
  member <- if (!length(vals_sql)) {
    paste0("(", lhs, " IS NULL)")
  } else if (any(missing)) {
    # R's %in% never returns NA. Make the NULL case explicit so negating
    # membership also retains missing rows when NA is absent from the set.
    paste0("(", lhs, " IN (", paste(vals_sql, collapse = ", "), ") OR ", lhs, " IS NULL)")
  } else {
    paste0("(", lhs, " IN (", paste(vals_sql, collapse = ", "), ") AND ", lhs, " IS NOT NULL)")
  }
  if (negate) paste0("NOT ", member) else member
}

translate_between <- function(lhs_expr, lo, hi, cols, env, conn, dialect) {
  lhs <- translate_value(lhs_expr, cols, env, conn, dialect)
  paste0(lhs, " BETWEEN ", translate_value(lo, cols, env, conn, dialect),
         " AND ", translate_value(hi, cols, env, conn, dialect))
}

# A regular expression that is really plain text, optionally anchored with
# `^` and `$`, as list(body, start, end); NULL if it uses anything more.
regex_as_literal <- function(pattern) {
  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern)) return(NULL)
  start <- startsWith(pattern, "^")
  body <- if (start) substring(pattern, 2L) else pattern
  end <- endsWith(body, "$")
  if (end) body <- substr(body, 1L, nchar(body) - 1L)
  if (grepl("[.^$*+?()\\[\\]{}|\\\\]", body, perl = TRUE)) return(NULL)
  list(body = body, start = start, end = end)
}

# Make text match itself in a T-SQL LIKE pattern.
escape_like_mssql <- function(x) gsub("([%_[])", "[\\1]", x)

# %like%/%ilike%/%plike% are regex matches in data.table (not SQL LIKE wildcard
# syntax), so they map onto a regex function rather than the SQL LIKE
# keyword: DuckDB's regexp_matches(), or (as of SQL Server 2025 / Azure SQL
# DB, Fabric SQL DB -- database compatibility level 170+) T-SQL's native
# REGEXP_LIKE(). Both engines happen to use Google's RE2 as their regex
# engine, so the same pattern text works on either and neither supports PCRE
# backreferences/lookaround -- %plike% is a best-effort alias of %like% on
# both.
#
# Most patterns in practice are plain text, perhaps anchored ("^ABC",
# "smith$"). On SQL Server those become LIKE, which every version has and
# which can use an index for a prefix. LIKE follows the column's collation,
# usually case-insensitive, as `==` already does there. Anything more needs
# REGEXP_LIKE(), and an older server says so with its own "not a recognized
# built-in function name" error.
translate_regex_like <- function(lhs_expr, pattern, cols, env, conn, dialect,
                                 ignore_case = FALSE) {
  lhs <- translate_value(lhs_expr, cols, env, conn, dialect)
  if (dialect == "mssql") {
    lit <- regex_as_literal(pattern)
    if (!is.null(lit)) {
      like <- translate_literal(paste0(
        if (!lit$start) "%", escape_like_mssql(lit$body), if (!lit$end) "%"
      ), conn, dialect)
      if (ignore_case) return(paste0("LOWER(", lhs, ") LIKE LOWER(", like, ")"))
      return(paste0(lhs, " LIKE ", like))
    }
  }
  pattern_sql <- translate_literal(pattern, conn, dialect)
  fn <- if (dialect == "mssql") "REGEXP_LIKE" else "regexp_matches"
  opts <- if (ignore_case) paste0(", ", translate_literal("i", conn, dialect)) else ""
  paste0(fn, "(", lhs, ", ", pattern_sql, opts, ")")
}

# %flike% is a fixed (literal) substring match, i.e. grepl(..., fixed = TRUE).
translate_flike <- function(lhs_expr, pattern, cols, env, conn, dialect) {
  lhs <- translate_value(lhs_expr, cols, env, conn, dialect)
  pattern_sql <- translate_literal(pattern, conn, dialect)
  if (dialect == "mssql") {
    # CHARINDEX does a plain substring search, so the pattern needs no
    # LIKE-wildcard escaping to stay a literal match.
    paste0("CHARINDEX(", pattern_sql, ", ", lhs, ") > 0")
  } else {
    paste0("contains(", lhs, ", ", pattern_sql, ")")
  }
}

# startsWith()/endsWith(). On SQL Server a literal affix becomes LIKE, which
# can use an index for a prefix; a computed one is compared directly.
translate_affix <- function(expr, at_start, cols, env, conn, dialect) {
  args <- call_args(expr, c("x", if (at_start) "prefix" else "suffix"))
  lhs <- translate_value(args[[1]], cols, env, conn, dialect)
  if (dialect != "mssql") {
    fn <- if (at_start) "starts_with" else "ends_with"
    return(paste0(fn, "(", lhs, ", ", translate_value(args[[2]], cols, env, conn, dialect), ")"))
  }
  affix <- args[[2]]
  if (is.character(affix) && length(affix) == 1L) {
    pat <- escape_like_mssql(affix)
    pat <- if (at_start) paste0(pat, "%") else paste0("%", pat)
    return(paste0(lhs, " LIKE ", translate_literal(pat, conn, dialect)))
  }
  affix_sql <- translate_value(affix, cols, env, conn, dialect)
  paste0(if (at_start) "LEFT(" else "RIGHT(", lhs, ", LEN(", affix_sql, ")) = ", affix_sql)
}

# A call's arguments matched to `formals` by name, then position, the way R
# matches them; NULL for one not given.
call_args <- function(expr, formals) {
  args <- as.list(expr)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  out <- stats::setNames(vector("list", length(formals)), formals)
  named <- nzchar(nms) & nms %in% formals
  for (i in which(named)) out[nms[i]] <- list(args[[i]])
  free <- setdiff(formals, nms[named])
  pos <- args[!named & !nzchar(nms)]
  for (i in seq_len(min(length(free), length(pos)))) out[free[i]] <- list(pos[[i]])
  out
}

# `na.rm` has no SQL meaning -- aggregates already skip NULL, which is what
# `na.rm = TRUE` asks for -- so it is dropped rather than sent as an argument.
drop_na_rm <- function(expr) {
  nms <- names(expr)
  if (is.null(nms) || !"na.rm" %in% nms) return(expr)
  expr[nms != "na.rm"]
}

translate_call_default <- function(expr, fn, cols, env, conn, dialect) {
  sql_fn <- unname(FN_MAP[[dialect]][fn])
  if (is.na(sql_fn)) sql_fn <- toupper(fn)
  args <- lapply(as.list(expr)[-1], translate_value, cols = cols, env = env, conn = conn, dialect = dialect)
  paste0(sql_fn, "(", paste(unlist(args), collapse = ", "), ")")
}

# median(). DuckDB has a median aggregate. T-SQL only has PERCENTILE_CONT as a
# window function, so on SQL Server the median is computed in a subquery
# (see duckdt_select_sql(), which collects these) and the aggregate picks it
# up again with MAX() -- every row of a group carries the same value.
translate_median <- function(expr, cols, env, conn, dialect) {
  arg <- translate_value(expr[[2]], cols, env, conn, dialect)
  if (dialect != "mssql") return(paste0("median(", arg, ")"))
  tx <- duckdt_env$tx
  if (is.null(tx)) {
    stop("duckdt: on SQL Server, median() can only be used in `j`, e.g. `d[, .(m = median(x)), by = g]`.",
      call. = FALSE)
  }
  alias <- paste0("duckdt_median_", length(tx$windows) + 1L)
  tx$windows <- c(tx$windows, stats::setNames(arg, alias))
  paste0("MAX(", DBI::dbQuoteIdentifier(conn, alias), ")")
}

translate_mean <- function(expr, cols, env, conn, dialect) {
  arg <- translate_value(expr[[2]], cols, env, conn, dialect)
  # T-SQL's AVG() of an integer column is an integer, truncated; R's mean()
  # is a double. DuckDB's avg() can't take a boolean, which mean(x > 1) is.
  if (dialect == "mssql") return(paste0("AVG(CAST(", arg, " AS FLOAT))"))
  if (is_predicate(expr[[2]])) return(paste0("avg(CAST(", arg, " AS INTEGER))"))
  paste0("avg(", arg, ")")
}

# ifelse()/fifelse(). A missing test gives a missing result, as in R, so the
# "no" branch is a second WHEN rather than ELSE.
translate_ifelse <- function(expr, fn, cols, env, conn, dialect) {
  formals <- c("test", "yes", "no", if (fn == "fifelse") "na")
  a <- call_args(expr, formals)
  if (is.null(a$test) || is.null(a$yes) || is.null(a$no)) {
    stop("duckdt: ", fn, "() needs `test`, `yes` and `no`.", call. = FALSE)
  }
  test <- translate_condition(a$test, cols, env, conn, dialect)
  val <- function(e) translate_value(e, cols, env, conn, dialect)
  paste0(
    "CASE WHEN ", test, " THEN ", val(a$yes), " WHEN NOT (", test, ") THEN ", val(a$no),
    if (!is.null(a$na)) paste0(" ELSE ", val(a$na)), " END"
  )
}

# fcase(when1, value1, when2, value2, ..., default = d).
translate_fcase <- function(expr, cols, env, conn, dialect) {
  args <- as.list(expr)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  default <- if ("default" %in% nms) args[[which(nms == "default")[1]]] else NULL
  pairs <- args[!nzchar(nms)]
  if (!length(pairs) || length(pairs) %% 2 != 0) {
    stop("duckdt: fcase() needs condition/value pairs.", call. = FALSE)
  }
  whens <- vapply(seq(1, length(pairs), by = 2), function(k) paste0(
    "WHEN ", translate_condition(pairs[[k]], cols, env, conn, dialect),
    " THEN ", translate_value(pairs[[k + 1]], cols, env, conn, dialect)
  ), character(1))
  paste0(
    "CASE ", paste(whens, collapse = " "),
    if (!is.null(default)) paste0(" ELSE ", translate_value(default, cols, env, conn, dialect)),
    " END"
  )
}

# paste0()/paste() as CONCAT(), which both engines have and which (unlike
# `||` or `+`) converts numbers to text. A missing value becomes "" rather
# than R's "NA".
translate_paste <- function(expr, fn, cols, env, conn, dialect) {
  args <- as.list(expr)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  if ("collapse" %in% nms) {
    stop("duckdt: paste(collapse = ) combines rows and can't be translated.", call. = FALSE)
  }
  sep <- if (fn == "paste") {
    if ("sep" %in% nms) eval(args[[which(nms == "sep")[1]]], envir = env) else " "
  } else ""
  parts <- unlist(lapply(args[!nms %in% c("sep", "collapse")], translate_value,
    cols = cols, env = env, conn = conn, dialect = dialect))
  if (nzchar(sep) && length(parts) > 1) {
    # a, sep, b, sep, c
    spaced <- rep(translate_literal(sep, conn, dialect), 2L * length(parts) - 1L)
    spaced[seq(1L, by = 2L, length.out = length(parts))] <- parts
    parts <- spaced
  }
  # SQL Server's CONCAT() needs at least two arguments.
  if (length(parts) == 1) parts <- c(parts, translate_literal("", conn, dialect))
  paste0("CONCAT(", paste(parts, collapse = ", "), ")")
}

translate_substr <- function(expr, fn, cols, env, conn, dialect) {
  a <- call_args(expr, if (fn == "substr") c("x", "start", "stop") else c("text", "first", "last"))
  val <- function(e) translate_value(e, cols, env, conn, dialect)
  len <- if (is.null(a[[3]])) "2147483647" else paste0("(", val(a[[3]]), ") - (", val(a[[2]]), ") + 1")
  paste0("SUBSTRING(", val(a[[1]]), ", ", val(a[[2]]), ", ", len, ")")
}

translate_cast <- function(expr, fn, cols, env, conn, dialect) {
  arg <- translate_value(expr[[2]], cols, env, conn, dialect)
  mssql <- dialect == "mssql"
  switch(fn,
    as.numeric = ,
    as.double = paste0("CAST(", arg, if (mssql) " AS FLOAT)" else " AS DOUBLE)"),
    # R truncates; DuckDB's cast to INTEGER rounds, so truncate first. Going
    # through a float also lets "3.7"-style text convert, as it does in R.
    as.integer = if (mssql) {
      paste0("CAST(CAST(", arg, " AS FLOAT) AS INT)")
    } else {
      paste0("CAST(trunc(CAST(", arg, " AS DOUBLE)) AS INTEGER)")
    },
    as.character = paste0("CAST(", arg, if (mssql) " AS NVARCHAR(4000))" else " AS VARCHAR)"),
    as.Date = ,
    as.IDate = paste0("CAST(", arg, " AS DATE)")
  )
}

translate_log <- function(expr, fn, cols, env, conn, dialect) {
  a <- call_args(expr, c("x", "base"))
  x <- translate_value(a$x, cols, env, conn, dialect)
  base <- if (fn == "log2") "2" else if (!is.null(a$base)) translate_value(a$base, cols, env, conn, dialect)
  if (fn == "log10") return(paste0(if (dialect == "mssql") "LOG10(" else "log10(", x, ")"))
  if (dialect == "mssql") {
    return(if (is.null(base)) paste0("LOG(", x, ")") else paste0("LOG(", x, ", ", base, ")"))
  }
  # DuckDB's one-argument log() is base 10, and its two-argument form takes
  # the base first.
  if (is.null(base)) paste0("ln(", x, ")") else paste0("log(", base, ", ", x, ")")
}

# Date parts that SQL Server spells with DATEPART() (YEAR(), MONTH() and
# DAY() are in FN_MAP), and data.table's wday(), which counts from Sunday = 1
# whatever the server's DATEFIRST.
translate_datepart <- function(expr, fn, cols, env, conn, dialect) {
  x <- translate_value(expr[[2]], cols, env, conn, dialect)
  if (fn == "wday") {
    if (dialect == "mssql") {
      return(paste0("((DATEPART(weekday, ", x, ") + @@DATEFIRST - 1) % 7 + 1)"))
    }
    return(paste0("(dayofweek(", x, ") + 1)"))
  }
  if (dialect != "mssql") return(paste0(unname(FN_MAP$duckdb[fn]), "(", x, ")"))
  part <- c(quarter = "quarter", hour = "hour", minute = "minute", second = "second",
            yday = "dayofyear")[[fn]]
  paste0("DATEPART(", part, ", ", x, ")")
}

translate_call <- function(expr, cols, env, conn, dialect) {
  fn <- call_name(expr)
  val <- function(e) translate_value(e, cols, env, conn, dialect)
  if (!grepl("^[^A-Za-z.]", fn)) expr <- drop_na_rm(expr)

  switch(fn,
    "(" = paste0("(", translate_expr(expr[[2]], cols, env, conn, dialect), ")"),
    "==" = binop(expr, "=", cols, env, conn, dialect),
    "!=" = binop(expr, "<>", cols, env, conn, dialect),
    ">"  = binop(expr, ">", cols, env, conn, dialect),
    ">=" = binop(expr, ">=", cols, env, conn, dialect),
    "<"  = binop(expr, "<", cols, env, conn, dialect),
    "<=" = binop(expr, "<=", cols, env, conn, dialect),
    "&"  = ,
    "&&" = logic(expr, "AND", cols, env, conn, dialect),
    "|"  = ,
    "||" = logic(expr, "OR", cols, env, conn, dialect),
    "!"  = paste0("NOT (", translate_condition(expr[[2]], cols, env, conn, dialect), ")"),
    "%in%" = ,
    "%chin%" = translate_in(expr, cols, env, conn, dialect),
    "%notin%" = translate_in(expr, cols, env, conn, dialect, negate = TRUE),
    "%between%" = {
      bounds <- eval(expr[[3]], envir = env)
      translate_between(expr[[2]], bounds[1], bounds[2], cols, env, conn, dialect)
    },
    "between" = {
      a <- call_args(expr, c("x", "lower", "upper"))
      translate_between(a$x, a$lower, a$upper, cols, env, conn, dialect)
    },
    "%like%" = ,
    "%plike%" = translate_regex_like(expr[[2]], eval(expr[[3]], envir = env), cols, env, conn, dialect),
    "%ilike%" = translate_regex_like(expr[[2]], eval(expr[[3]], envir = env), cols, env, conn, dialect,
      ignore_case = TRUE),
    "%flike%" = translate_flike(expr[[2]], eval(expr[[3]], envir = env), cols, env, conn, dialect),
    "grepl" = {
      a <- call_args(expr, c("pattern", "x", "ignore.case", "perl", "fixed"))
      pattern <- eval(a$pattern, envir = env)
      if (isTRUE(eval(a$fixed, envir = env))) {
        translate_flike(a$x, pattern, cols, env, conn, dialect)
      } else {
        translate_regex_like(a$x, pattern, cols, env, conn, dialect,
          ignore_case = isTRUE(eval(a$ignore.case, envir = env)))
      }
    },
    "startsWith" = translate_affix(expr, TRUE, cols, env, conn, dialect),
    "endsWith" = translate_affix(expr, FALSE, cols, env, conn, dialect),
    "is.na" = {
      arg <- expr[[2]]
      sql <- val(arg)
      if (is.call(arg)) sql <- paste0("(", sql, ")")
      paste0(sql, " IS NULL")
    },
    "+" = arith(expr, "+", cols, env, conn, dialect),
    "-" = arith(expr, "-", cols, env, conn, dialect),
    "*" = arith(expr, "*", cols, env, conn, dialect),
    "/" = translate_divide(expr, cols, env, conn, dialect),
    "^" = {
      base <- val(expr[[2]])
      # T-SQL's POWER() returns the type of its first argument: an integer
      # base would truncate a fractional result.
      if (dialect == "mssql") base <- paste0("CAST(", base, " AS FLOAT)")
      paste0("POWER(", base, ", ", val(expr[[3]]), ")")
    },
    "%%" = paste0("(", val(expr[[2]]), " % ", val(expr[[3]]), ")"),
    "%/%" = paste0("FLOOR(", translate_divide(expr, cols, env, conn, dialect), ")"),
    "mean" = translate_mean(expr, cols, env, conn, dialect),
    "median" = translate_median(expr, cols, env, conn, dialect),
    "uniqueN" = paste0("count(DISTINCT ", val(expr[[2]]), ")"),
    "ifelse" = ,
    "fifelse" = translate_ifelse(expr, fn, cols, env, conn, dialect),
    "fcase" = translate_fcase(expr, cols, env, conn, dialect),
    "paste" = ,
    "paste0" = translate_paste(expr, fn, cols, env, conn, dialect),
    "substr" = ,
    "substring" = translate_substr(expr, fn, cols, env, conn, dialect),
    "as.numeric" = ,
    "as.double" = ,
    "as.integer" = ,
    "as.character" = ,
    "as.Date" = ,
    "as.IDate" = translate_cast(expr, fn, cols, env, conn, dialect),
    "log" = ,
    "log2" = ,
    "log10" = translate_log(expr, fn, cols, env, conn, dialect),
    "wday" = translate_datepart(expr, fn, cols, env, conn, dialect),
    "quarter" = ,
    "hour" = ,
    "minute" = ,
    "second" = ,
    "yday" = translate_datepart(expr, fn, cols, env, conn, dialect),
    "round" = if (dialect == "mssql" && length(expr) == 2) {
      # T-SQL's ROUND() requires the number of digits.
      paste0("ROUND(", val(expr[[2]]), ", 0)")
    } else {
      translate_call_default(expr, fn, cols, env, conn, dialect)
    },
    "trunc" = if (dialect == "mssql") {
      paste0("ROUND(", val(expr[[2]]), ", 0, 1)")
    } else {
      translate_call_default(expr, fn, cols, env, conn, dialect)
    },
    "trimws" = if (dialect == "mssql") {
      paste0("LTRIM(RTRIM(", val(expr[[2]]), "))")
    } else {
      translate_call_default(expr, fn, cols, env, conn, dialect)
    },
    translate_call_default(expr, fn, cols, env, conn, dialect)
  )
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

  if (is.null(expr) || (is.atomic(expr) && length(expr) >= 1)) {
    return(translate_literal(expr, conn, dialect))
  }

  if (is.call(expr)) {
    translate_call(expr, cols, env, conn, dialect)
  } else {
    stop("duckdt: unsupported expression: ", deparse(expr), call. = FALSE)
  }
}

# `i = order(...)`: an ORDER BY list. `-x` and `decreasing = TRUE` sort
# descending; missing values sort last, as R's order() puts them, which SQL
# Server (no NULLS LAST) needs spelled out as a sort key of its own.
translate_order <- function(expr, cols, env, conn, dialect) {
  args <- as.list(expr)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  decreasing <- if ("decreasing" %in% nms) isTRUE(eval(args[[which(nms == "decreasing")]], envir = env)) else FALSE
  na_last <- if ("na.last" %in% nms) eval(args[[which(nms == "na.last")]], envir = env) else TRUE
  if (!isTRUE(na_last) && !isFALSE(na_last)) {
    stop("duckdt: order(na.last = ) must be TRUE or FALSE in a query.", call. = FALSE)
  }
  keys <- args[!nms %in% c("decreasing", "na.last", "method")]
  if (!length(keys)) stop("duckdt: order() needs at least one column to sort by.", call. = FALSE)
  unlist(lapply(keys, function(k) {
    desc <- decreasing
    if (is.call(k) && identical(k[[1]], as.name("-")) && length(k) == 2) {
      desc <- !desc
      k <- k[[2]]
    }
    sql <- translate_value(k, cols, env, conn, dialect)
    dir <- if (desc) "DESC" else "ASC"
    if (dialect == "mssql") {
      c(paste0("CASE WHEN ", sql, " IS NULL THEN ", if (na_last) "1 ELSE 0" else "0 ELSE 1", " END"),
        paste(sql, dir))
    } else {
      paste(sql, dir, if (na_last) "NULLS LAST" else "NULLS FIRST")
    }
  }))
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
        sql <- translate_value(a, cols, env, conn, dialect, bit = TRUE)
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
        alias <- paste(deparse(a), collapse = " ")
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

  stop("duckdt: unsupported `j` expression: ", paste(deparse(expr), collapse = " "),
       ". Use `.(col1, col2 = expr)` to select/compute columns.", call. = FALSE)
}

# `by` translation: character vector, bare column, or .()/list(...)/c(...).
translate_by <- function(expr, cols, env, conn, dialect) {
  if (is.character(expr)) {
    aliases <- expr
    parts <- vapply(expr, function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm)), character(1))
    return(list(parts = unname(parts), aliases = aliases, group_parts = unname(parts)))
  }

  if (is.call(expr) && as.character(expr[[1]]) %in% c(".", "list", "c")) {
    args <- as.list(expr)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    parts <- character(length(args))
    aliases <- character(length(args))
    group_parts <- character(length(args))
    for (i in seq_along(args)) {
      a <- args[[i]]
      if (is.character(a) && length(a) == 1 && !nzchar(nms[i])) {
        # e.g. by = c("cyl", "gear") -- a string naming a column, not a value
        alias <- a
        parts[i] <- as.character(DBI::dbQuoteIdentifier(conn, a))
        group_parts[i] <- parts[i]
      } else {
        sql <- translate_value(a, cols, env, conn, dialect, bit = TRUE)
        alias <- if (nzchar(nms[i])) nms[i] else if (is.symbol(a)) as.character(a) else paste(deparse(a), collapse = " ")
        parts[i] <- if (nzchar(nms[i])) paste0(sql, " AS ", DBI::dbQuoteIdentifier(conn, alias)) else sql
        group_parts[i] <- sql
      }
      aliases[i] <- alias
    }
    return(list(parts = parts, aliases = aliases, group_parts = group_parts))
  }

  if (is.symbol(expr)) {
    nm <- as.character(expr)
    part <- as.character(DBI::dbQuoteIdentifier(conn, nm))
    return(list(parts = part, aliases = nm, group_parts = part))
  }

  stop("duckdt: unsupported `by` expression: ", paste(deparse(expr), collapse = " "), call. = FALSE)
}
