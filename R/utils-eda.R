#' NA summary per column
#'
#' Returns a data.table with one row per column showing the count and percentage
#' of `NA` values.
#'
#' @param dt A data.table.
#'
#' @return A data.table with columns `col`, `n_na`, and `pct_na`.
#' @export
na_dt <- function(dt) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    nr <- nrow(dt)
    cols <- names(dt)
    n_na <- vapply(cols, function(col) sum(is.na(dt[[col]])), integer(1L))
    pct_na <- if (nr > 0L) n_na / nr else rep(NA_real_, length(cols))
    data.table::data.table(
        col    = cols,
        n_na   = n_na,
        pct_na = pct_na
    )
}

#' Frequency table for one column
#'
#' Returns the top `n` most frequent values in a single column as a data.table
#' with counts and percentages, sorted descending by count.
#'
#' @param dt A data.table.
#' @param col Column name (unquoted or character scalar).
#' @param n Maximum number of rows to return. `Inf` returns all.
#'
#' @return A data.table with columns matching `col`, `n`, and `pct`.
#' @export
freq_dt <- function(dt, col, n = 20L) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    col_expr <- substitute(col)
    col_name <- if (is.symbol(col_expr)) as.character(col_expr) else col
    if (!is.character(col_name) || length(col_name) != 1L) {
        stop("'col' must be a column name (unquoted or character scalar).", call. = FALSE)
    }
    if (!(col_name %in% names(dt))) {
        stop(sprintf("Column '%s' not found in 'dt'.", col_name), call. = FALSE)
    }
    n <- suppressWarnings(as.numeric(n[[1L]]))
    if (is.na(n) || n <= 0) {
        stop("'n' must be a positive number.", call. = FALSE)
    }

    counts <- dt[, .N, by = col_name]
    data.table::setorder(counts, -N)
    counts[, pct := N / sum(N)]
    data.table::setnames(counts, "N", "n")
    n_limit <- min(nrow(counts), as.integer(n))
    counts[seq_len(n_limit)][]
}

#' Column schema summary
#'
#' Returns a data.table with one row per column showing class, number of
#' distinct non-NA values, and NA count. Useful for a quick overview of a
#' wide table.
#'
#' @param dt A data.table.
#'
#' @return A data.table with columns `col`, `class`, `n_distinct`, and `n_na`.
#' @export
schema_dt <- function(dt) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    cols <- names(dt)
    cls      <- vapply(cols, function(col) paste(class(dt[[col]]), collapse = "/"), character(1L))
    n_dist   <- vapply(cols, function(col) data.table::uniqueN(dt[[col]], na.rm = TRUE), integer(1L))
    n_na     <- vapply(cols, function(col) sum(is.na(dt[[col]])), integer(1L))
    data.table::data.table(
        col        = cols,
        class      = cls,
        n_distinct = n_dist,
        n_na       = n_na
    )
}

#' Rename columns by name
#'
#' Thin wrapper over `data.table::setnames()` that accepts a named character
#' vector mapping old names to new names, modifying the table by reference.
#'
#' @param dt A data.table.
#' @param renames Named character vector where names are the new column names
#'   and values are the existing column names. For example:
#'   `c(new_name = "old_name", v2 = "value")`.
#'
#' @return Invisibly returns `dt` (modified by reference).
#' @export
rename_dt <- function(dt, renames) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    if (!is.character(renames) || is.null(names(renames))) {
        stop("'renames' must be a named character vector (new_name = 'old_name').", call. = FALSE)
    }
    old <- unname(renames)
    new <- names(renames)
    missing_cols <- setdiff(old, names(dt))
    if (length(missing_cols)) {
        stop(sprintf("Column(s) not found in 'dt': %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
    data.table::setnames(dt, old = old, new = new)
    invisible(dt)
}
