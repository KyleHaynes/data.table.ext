#' Join two tables inside the database
#'
#' A database-side [merge()]: `x` and `y` are joined in SQL and only the
#' result comes back to R. Either side may be a `"duckdt"` handle or an
#' ordinary `data.frame`/`data.table` -- R-side inputs are staged into a
#' temporary table on the connection first, so the join itself always runs in
#' the engine rather than in R.
#'
#' This is *not* [dbdt_merge()]. `dbdt_join()` reads: it computes a new
#' result set and returns it, leaving both inputs untouched, the way base
#' [merge()] does. `dbdt_merge()` writes: it reconciles `y` **into** `x`'s
#' table in place (update/insert/delete). Reach for this one to combine
#' tables, and for that one to patch a table.
#'
#' `merge(x, y, ...)` on a `"duckdt"` object dispatches here, so the familiar
#' spelling works too.
#'
#' At least one of `x`/`y` must be a `"duckdt"` object -- that is what
#' supplies the connection to run on. If both are, they must be on the same
#' connection. Joining two R data.frames is data.table's job, not this
#' function's.
#'
#' @param x,y The two tables to join: each a `"duckdt"` object or a
#'   `data.frame`/`data.table`.
#' @param by Column(s) to join on, present in both. Defaults to every column
#'   common to both (a natural join, like base [merge()]).
#' @param by.x,by.y Join on differently-named columns. Give both, of equal
#'   length; they pair up positionally. Overrides `by`. As in base [merge()],
#'   the result keeps `by.x`'s names.
#' @param all Keep non-matching rows from both sides (a `FULL` join)? Default
#'   `FALSE` -- an `INNER` join, keeping only matched rows.
#' @param all.x Keep non-matching rows from `x` (a `LEFT` join)? Defaults to
#'   `all`.
#' @param all.y Keep non-matching rows from `y` (a `RIGHT` join)? Defaults to
#'   `all`.
#' @param suffixes Length-2 character vector disambiguating non-join columns
#'   that both sides have, as in base [merge()].
#' @param sort Sort the result by the join columns? Default `TRUE`, matching
#'   base [merge()]. `FALSE` returns the engine's join order, which is faster
#'   and arbitrary.
#'
#' @return A `data.table`.
#' @section Binary columns:
#' Binary columns (`BLOB`/`BIT` on DuckDB, `varbinary`/`binary`/`image` on
#' MS SQL Server) are dropped from the result, since no driver hands them
#' back as an R vector. This happens after `by` is resolved, so a natural
#' join still keys on exactly the columns base [merge()] would -- a binary
#' column used as a join key is kept.
#' @seealso [dbdt_temp()] to keep a filtered subset in the database so it
#'   can be joined; [dbdt_merge()] for the write-path counterpart.
#' @examples
#' d <- as.dbdt(datasets::mtcars, name = "cars")
#' labels <- data.frame(cyl = c(4, 6, 8), label = c("four", "six", "eight"))
#' head(dbdt_join(d, labels, by = "cyl"), 3)
#' @export
duckdt_join <- function(x, y, by = NULL, by.x = NULL, by.y = NULL,
                        all = FALSE, all.x = all, all.y = all,
                        suffixes = c(".x", ".y"), sort = TRUE) {
  conn <- duckdt_join_conn(x, y)
  dialect <- duckdt_dialect(conn)

  x_cols <- duckdt_side_columns(x)
  y_cols <- duckdt_side_columns(y)

  keys <- duckdt_join_keys(by, by.x, by.y, x_cols, y_cols)

  if (length(suffixes) != 2 || !is.character(suffixes)) {
    stop("duckdt_join: `suffixes` must be a character vector of length 2.", call. = FALSE)
  }

  # Stage whichever side is an R object, and make sure both stagings are
  # cleaned up even if the join itself errors.
  x_ref <- duckdt_join_stage_side(conn, x, x_cols, dialect, "x")
  on.exit(duckdt_join_unstage(conn, x_ref, dialect), add = TRUE)
  y_ref <- duckdt_join_stage_side(conn, y, y_cols, dialect, "y")
  on.exit(duckdt_join_unstage(conn, y_ref, dialect), add = TRUE)

  sql <- duckdt_join_sql(
    conn, x_ref$sql, y_ref$sql,
    duckdt_join_project(x, x_cols, keys$x), duckdt_join_project(y, y_cols, keys$y),
    keys,
    all.x = isTRUE(all.x), all.y = isTRUE(all.y),
    suffixes = suffixes, sort = isTRUE(sort)
  )

  # `[]` works around a data.table quirk where setDT() suppresses the next
  # top-level auto-print (see the note in bracket.R).
  data.table::setDT(DBI::dbGetQuery(conn, sql))[]
}

#' @rdname duckdt_join
#' @param ... Passed on to [dbdt_join()].
#' @exportS3Method base::merge
merge.duckdt <- function(x, y, ...) duckdt_join(x, y, ...)

# ---- internals -------------------------------------------------------------

duckdt_join_conn <- function(x, y) {
  x_is <- inherits(x, "duckdt")
  y_is <- inherits(y, "duckdt")
  if (!x_is && !is.data.frame(x)) {
    stop("duckdt_join: `x` must be a duckdt object or a data.frame/data.table.", call. = FALSE)
  }
  if (!y_is && !is.data.frame(y)) {
    stop("duckdt_join: `y` must be a duckdt object or a data.frame/data.table.", call. = FALSE)
  }
  if (!x_is && !y_is) {
    stop(
      "duckdt_join: at least one of `x`/`y` must be a duckdt object -- that is what ",
      "says which database to run the join in. To join two data.frames, use ",
      "`merge()` or data.table's `[` directly.",
      call. = FALSE
    )
  }
  if (x_is && y_is && !identical(x$conn, y$conn)) {
    stop("duckdt_join: `x` and `y` must be on the same connection.", call. = FALSE)
  }
  if (x_is) x$conn else y$conn
}

duckdt_side_columns <- function(v) if (inherits(v, "duckdt")) duckdt_columns(v) else names(v)

# The columns of one side worth putting in the result: everything but the
# binary ones, which can't be handed back to R (see binary-cols.R). Only the
# projection is narrowed -- `keys` is already resolved by the time this runs,
# so dropping a column here can never change which rows the join matches, and
# a key that happens to be binary is kept rather than silently removed from
# the output it names.
duckdt_join_project <- function(v, cols, keys) {
  if (!inherits(v, "duckdt")) return(cols)
  bin <- setdiff(duckdt_binary_cols(v), keys)
  if (!length(bin)) return(cols)
  duckdt_binary_notify(v$tbl, intersect(cols, bin))
  setdiff(cols, bin)
}

# Give the join something to put in its FROM clause. A duckdt side is
# already a table name; an R side is copied into a temporary table (reusing
# duckdt_merge()'s staging, which handles both dialects) and reported back so
# the caller can drop it afterwards.
duckdt_join_stage_side <- function(conn, v, cols, dialect, label) {
  if (inherits(v, "duckdt")) {
    return(list(sql = as.character(duckdt_qtbl(v)), staged = NULL))
  }
  tmp <- duckdt_temp_name(paste0("join_", label))
  duckdt_merge_stage(conn, v, y_is_duckdt = FALSE, cols = cols, tmp_name = tmp, dialect = dialect)
  list(sql = as.character(DBI::dbQuoteIdentifier(conn, tmp)), staged = tmp)
}

duckdt_join_unstage <- function(conn, ref, dialect) {
  if (is.null(ref$staged)) return(invisible(NULL))
  try(duckdt_drop_table(conn, ref$staged, dialect), silent = TRUE)
  invisible(NULL)
}

# Resolve `by` / `by.x`+`by.y` into a positionally-paired key mapping.
duckdt_join_keys <- function(by, by.x, by.y, x_cols, y_cols) {
  if (!is.null(by.x) || !is.null(by.y)) {
    if (is.null(by.x) || is.null(by.y)) {
      stop("duckdt_join: give both `by.x` and `by.y`, or neither.", call. = FALSE)
    }
    if (length(by.x) != length(by.y)) {
      stop("duckdt_join: `by.x` and `by.y` must be the same length.", call. = FALSE)
    }
  } else {
    if (is.null(by)) {
      by <- intersect(x_cols, y_cols)
      if (length(by) == 0) {
        stop("duckdt_join: `x` and `y` share no columns to join `by`; specify `by=`.", call. = FALSE)
      }
    }
    by.x <- by
    by.y <- by
  }
  missing_x <- setdiff(by.x, x_cols)
  missing_y <- setdiff(by.y, y_cols)
  if (length(missing_x) > 0) {
    stop("duckdt_join: join column(s) not found in `x`: ", paste(missing_x, collapse = ", "), call. = FALSE)
  }
  if (length(missing_y) > 0) {
    stop("duckdt_join: join column(s) not found in `y`: ", paste(missing_y, collapse = ", "), call. = FALSE)
  }
  list(x = by.x, y = by.y)
}

duckdt_join_sql <- function(conn, x_sql, y_sql, x_cols, y_cols, keys,
                            all.x, all.y, suffixes, sort) {
  q <- function(nm) as.character(DBI::dbQuoteIdentifier(conn, nm))
  lhs <- "duckdt_x"
  rhs <- "duckdt_y"

  join_type <- if (all.x && all.y) {
    "FULL OUTER JOIN"
  } else if (all.x) {
    "LEFT JOIN"
  } else if (all.y) {
    "RIGHT JOIN"
  } else {
    "INNER JOIN"
  }

  on_clause <- paste(sprintf(
    "%s.%s = %s.%s", lhs, q(keys$x), rhs, q(keys$y)
  ), collapse = " AND ")

  # Key columns keep `by.x`'s names. A join that can produce y-only rows
  # leaves the x-side key NULL on those, so the two sides are coalesced --
  # the same value base merge() reports.
  key_alias <- q(keys$x)
  key_select <- if (all.y) {
    sprintf("COALESCE(%s.%s, %s.%s) AS %s", lhs, q(keys$x), rhs, q(keys$y), key_alias)
  } else {
    sprintf("%s.%s AS %s", lhs, q(keys$x), key_alias)
  }

  x_rest <- setdiff(x_cols, keys$x)
  y_rest <- setdiff(y_cols, keys$y)
  clash <- intersect(x_rest, y_rest)

  x_select <- vapply(x_rest, function(nm) {
    out <- if (nm %in% clash) paste0(nm, suffixes[1]) else nm
    sprintf("%s.%s AS %s", lhs, q(nm), q(out))
  }, character(1), USE.NAMES = FALSE)
  y_select <- vapply(y_rest, function(nm) {
    out <- if (nm %in% clash) paste0(nm, suffixes[2]) else nm
    sprintf("%s.%s AS %s", rhs, q(nm), q(out))
  }, character(1), USE.NAMES = FALSE)

  sql <- paste0(
    "SELECT ", paste(c(key_select, x_select, y_select), collapse = ", "),
    " FROM ", x_sql, " AS ", lhs,
    " ", join_type, " ", y_sql, " AS ", rhs,
    " ON ", on_clause
  )
  if (sort) sql <- paste0(sql, " ORDER BY ", paste(key_alias, collapse = ", "))
  sql
}
