#' Profile a table's columns inside the database
#'
#' The database-side counterpart of [schema_dt()] and [na_dt()]: for every
#' column, how many values are missing, how many are distinct, and the
#' smallest and largest, all computed by the database in a single query over
#' the table. Only the summary comes back to R, so this is how to get to know
#' a table too large to pull in.
#'
#' Counting distinct values is the expensive part: the database has to sort
#' or hash every column. On a very large table, `distinct = FALSE` skips it
#' and leaves a cheaper scan.
#'
#' Columns whose values can't be compared or brought into R (binary columns,
#' and on SQL Server `text`, `ntext`, `xml` and the spatial types) still get
#' their missing count; their other statistics are `NA`.
#'
#' @param x A `"duckdt"` object.
#' @param cols Columns to profile. Defaults to all of them.
#' @param distinct Count distinct values? Default `TRUE`. Missing values are
#'   not counted as a value, as in `uniqueN(x, na.rm = TRUE)`.
#'
#' @return A `data.table` with one row per column: `col`, `type` (its SQL
#'   type), `n_na`, `pct_na`, `n_distinct`, and `min` and `max` (as text, since
#'   columns differ in type). The table's row count is in the `"n_rows"`
#'   attribute.
#' @seealso [schema_dt()] and [na_dt()] for a `data.table` already in R.
#' @examples
#' d <- as.dbdt(datasets::airquality)
#' dbdt_profile(d)
#' @export
duckdt_profile <- function(x, cols = NULL, distinct = TRUE) {
  if (!inherits(x, "duckdt")) {
    stop("duckdt_profile: `x` must be a duckdt object.", call. = FALSE)
  }
  conn <- x$conn
  dialect <- duckdt_dialect(conn)
  types <- duckdt_meta(x)
  if (!nrow(types)) types <- data.frame(column = duckdt_columns(x), type = NA_character_)
  if (!is.null(cols)) {
    unknown <- setdiff(cols, types$column)
    if (length(unknown)) {
      stop("duckdt_profile: no column ", paste(sQuote(unknown), collapse = ", "),
        " in '", duckdt_label(x), "'.", call. = FALSE)
    }
    types <- types[match(cols, types$column), , drop = FALSE]
  }
  if (!nrow(types)) stop("duckdt_profile: no columns to profile.", call. = FALSE)

  mssql <- dialect == "mssql"
  q <- as.character(DBI::dbQuoteIdentifier(conn, types$column))
  comparable <- !duckdt_uncomparable_type(types$type, dialect)
  # T-SQL won't take MIN()/MAX() of a bit column; an int says the same thing.
  v <- ifelse(mssql & tolower(types$type) %in% "bit", paste0("CAST(", q, " AS INT)"), q)
  count <- if (mssql) "COUNT_BIG" else "count"
  alias <- function(stat) as.character(DBI::dbQuoteIdentifier(conn, paste0(stat, "_", seq_along(q))))

  exprs <- c(
    paste0(count, "(*) AS ", DBI::dbQuoteIdentifier(conn, "n_rows")),
    paste0(
      "SUM(", if (mssql) "CAST(", "CASE WHEN ", q, " IS NULL THEN 1 ELSE 0 END",
      if (mssql) " AS BIGINT)", ") AS ", alias("na")
    ),
    if (distinct && any(comparable)) {
      paste0(count, "(DISTINCT ", v[comparable], ") AS ", alias("nd")[comparable])
    },
    if (any(comparable)) paste0("MIN(", v[comparable], ") AS ", alias("min")[comparable]),
    if (any(comparable)) paste0("MAX(", v[comparable], ") AS ", alias("max")[comparable])
  )
  res <- duckdt_get_query(conn, paste0(
    "SELECT ", paste(exprs, collapse = ", "), " FROM ", duckdt_qtbl(x)
  ))

  stat <- function(prefix, as_text = FALSE) {
    vapply(seq_along(q), function(k) {
      nm <- paste0(prefix, "_", k)
      if (!nm %in% names(res)) return(if (as_text) NA_character_ else NA_real_)
      val <- res[[nm]][1]
      if (as_text) duckdt_profile_text(val) else as.numeric(val)
    }, if (as_text) character(1) else numeric(1))
  }
  n_rows <- as.numeric(res[["n_rows"]][1])
  n_na <- stat("na")
  n_na[is.na(n_na)] <- 0
  out <- data.table::data.table(
    col = types$column,
    type = types$type,
    n_na = n_na,
    pct_na = if (n_rows > 0) n_na / n_rows else NA_real_,
    n_distinct = if (distinct) stat("nd") else NA_real_,
    min = stat("min", TRUE),
    max = stat("max", TRUE)
  )
  data.table::setattr(out, "n_rows", n_rows)
  out[]
}

duckdt_profile_text <- function(val) {
  if (is.null(val) || length(val) != 1L || is.na(val)) return(NA_character_)
  if (is.numeric(val) && !is.object(val)) return(as.character(val))
  format(val)
}

# Types MIN(), MAX() and COUNT(DISTINCT) can't take, or whose values R can't
# receive: the binary ones (see binary-cols.R) and, on SQL Server, the LOB,
# XML and CLR types; on DuckDB, nested types.
duckdt_uncomparable_type <- function(type, dialect) {
  base <- tolower(trimws(sub("\\(.*$", "", as.character(type))))
  out <- duckdt_is_binary_type(type, dialect)
  if (dialect == "mssql") {
    out | base %in% c("text", "ntext", "image", "xml", "geometry", "geography",
                      "hierarchyid", "sql_variant")
  } else {
    out | grepl("\\[|^(struct|map|union)", base) | is.na(type)
  }
}
