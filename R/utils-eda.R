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
    n <- suppressWarnings(as.numeric(n[1L]))
    if (is.na(n) || n <= 0) {
        stop("'n' must be a positive number.", call. = FALSE)
    }

    counts <- dt[, .N, by = col_name]
    data.table::setorder(counts, -N)
    counts[, pct := N / sum(N)]
    data.table::setnames(counts, "N", "n")
    n_limit <- as.integer(min(nrow(counts), n))
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

#' Print a compact distribution sparkline for a numeric column
#'
#' Buckets `dt[[col]]` into `bins` equal-width bins and renders a unicode
#' bar-height sparkline alongside min/median/max, for a quick distribution
#' scan without plotting.
#'
#' @param dt A data.table.
#' @param col Column name (unquoted or character scalar). Must be numeric.
#' @param bins Integer number of buckets. Default `10L`.
#'
#' @return Invisibly returns a character scalar containing the sparkline.
#' @export
spark_dt <- function(dt, col, bins = 10L) {
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

    v <- dt[[col_name]]
    if (!is.numeric(v)) {
        stop(sprintf("Column '%s' must be numeric.", col_name), call. = FALSE)
    }
    bins <- suppressWarnings(as.integer(bins[1L]))
    if (is.na(bins) || bins < 2L) {
        stop("'bins' must be an integer >= 2.", call. = FALSE)
    }

    v <- v[!is.na(v)]
    if (!length(v)) {
        out <- sprintf("%s: <no non-NA values>", col_name)
        cat(out, "\n", sep = "")
        return(invisible(out))
    }

    blocks <- c("\u2581", "\u2582", "\u2583", "\u2584", "\u2585", "\u2586", "\u2587", "\u2588")
    rng <- range(v)
    if (rng[1L] == rng[2L]) {
        spark <- strrep(blocks[length(blocks)], bins)
    } else {
        breaks <- seq(rng[1L], rng[2L], length.out = bins + 1L)
        bucket <- cut(v, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        counts <- tabulate(bucket, nbins = bins)
        max_count <- max(counts)
        level <- if (max_count == 0L) rep(1L, bins) else pmax(1L, ceiling(counts / max_count * length(blocks)))
        spark <- paste(blocks[level], collapse = "")
    }

    out <- sprintf(
        "%s: %s  [min %s, median %s, max %s]",
        col_name,
        spark,
        format(rng[1L], trim = TRUE),
        format(stats::median(v), trim = TRUE),
        format(rng[2L], trim = TRUE)
    )
    cat(out, "\n", sep = "")
    invisible(out)
}

#' Flag rows containing outlier values
#'
#' Flags rows where any of `cols` (numeric columns) is an outlier under the
#' chosen `method`, returning only the flagged rows plus an `outlier_cols`
#' column listing which column(s) triggered the flag.
#'
#' @param dt A data.table.
#' @param cols Character vector of numeric column names to check. If `NULL`
#'   (default), all numeric columns are checked.
#' @param method Character scalar: `"iqr"` (default) flags values outside
#'   `threshold * IQR` from Q1/Q3. `"zscore"` flags values whose absolute
#'   z-score exceeds `threshold`.
#' @param threshold Numeric scalar. For `"iqr"`, the IQR multiplier (default
#'   `1.5`). For `"zscore"`, the z-score cutoff (default `3`).
#'
#' @return A data.table containing the flagged rows from `dt`, plus an
#'   `outlier_cols` character column listing which column(s) triggered the
#'   flag for that row. Returns a zero-row data.table if none are found.
#' @export
outlier_dt <- function(dt, cols = NULL, method = c("iqr", "zscore"), threshold = NULL) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    method <- match.arg(method)

    is_numeric_col <- vapply(dt, is.numeric, logical(1L))
    if (is.null(cols)) {
        cols <- names(dt)[is_numeric_col]
    } else {
        if (!is.character(cols)) {
            stop("'cols' must be a character vector of column names.", call. = FALSE)
        }
        missing_cols <- setdiff(cols, names(dt))
        if (length(missing_cols)) {
            stop(sprintf("Column(s) not found in 'dt': %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
        }
        non_numeric <- cols[!is_numeric_col[cols]]
        if (length(non_numeric)) {
            stop(sprintf("Column(s) not numeric: %s", paste(non_numeric, collapse = ", ")), call. = FALSE)
        }
    }

    empty_result <- function() {
        ans <- data.table::copy(dt)[0L]
        ans[, outlier_cols := character(0L)]
        ans[]
    }
    if (!length(cols) || !nrow(dt)) {
        return(empty_result())
    }

    if (is.null(threshold)) {
        threshold <- if (identical(method, "iqr")) 1.5 else 3
    }
    threshold <- suppressWarnings(as.numeric(threshold[1L]))
    if (is.na(threshold) || threshold <= 0) {
        stop("'threshold' must be a positive number.", call. = FALSE)
    }

    nr <- nrow(dt)
    flag_mat <- matrix(FALSE, nrow = nr, ncol = length(cols), dimnames = list(NULL, cols))
    for (col in cols) {
        v <- dt[[col]]
        if (identical(method, "iqr")) {
            qs <- stats::quantile(v, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
            iqr <- qs[2L] - qs[1L]
            lower <- qs[1L] - threshold * iqr
            upper <- qs[2L] + threshold * iqr
            flag_mat[, col] <- !is.na(v) & (v < lower | v > upper)
        } else {
            mu <- mean(v, na.rm = TRUE)
            sigma <- stats::sd(v, na.rm = TRUE)
            z <- if (isTRUE(sigma > 0)) abs((v - mu) / sigma) else rep(0, length(v))
            flag_mat[, col] <- !is.na(v) & z > threshold
        }
    }

    row_flagged <- rowSums(flag_mat) > 0L
    if (!any(row_flagged)) {
        return(empty_result())
    }

    ans <- data.table::copy(dt[row_flagged])
    triggered <- apply(flag_mat[row_flagged, , drop = FALSE], 1L, function(r) paste(cols[r], collapse = ", "))
    ans[, outlier_cols := triggered]
    ans[]
}

#' Find candidate key columns
#'
#' Searches column combinations (up to `max_size` columns) for one that
#' uniquely identifies every row of `dt`, useful for spotting a natural key
#' before a join. Reports only minimal candidates: a combination is skipped
#' if a smaller already-found candidate is a subset of it.
#'
#' @param dt A data.table.
#' @param cols Character vector of candidate columns to consider. If `NULL`
#'   (default), all columns are considered.
#' @param max_size Maximum number of columns to combine when searching.
#'   Default `3L`. Search cost grows combinatorially with both `max_size`
#'   and the number of `cols`, so raise this cautiously on wide tables.
#'
#' @return A data.table with one row per candidate key found, columns `cols`
#'   (comma-separated column names) and `n_cols`, ordered by `n_cols`
#'   ascending. Returns a zero-row data.table if no candidate is found.
#' @export
key_dt <- function(dt, cols = NULL, max_size = 3L) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    if (is.null(cols)) {
        cols <- names(dt)
    } else if (!is.character(cols)) {
        stop("'cols' must be a character vector of column names.", call. = FALSE)
    }
    missing_cols <- setdiff(cols, names(dt))
    if (length(missing_cols)) {
        stop(sprintf("Column(s) not found in 'dt': %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
    max_size <- suppressWarnings(as.integer(max_size[1L]))
    if (is.na(max_size) || max_size < 1L) {
        stop("'max_size' must be a positive integer.", call. = FALSE)
    }
    max_size <- min(max_size, length(cols))

    empty_result <- data.table::data.table(cols = character(0L), n_cols = integer(0L))
    nr <- nrow(dt)
    if (nr == 0L || !length(cols)) {
        return(empty_result)
    }

    found <- list()
    for (size in seq_len(max_size)) {
        combos <- utils::combn(cols, size, simplify = FALSE)
        for (combo in combos) {
            is_superset <- vapply(found, function(s) all(s %in% combo), logical(1L))
            if (any(is_superset)) {
                next
            }
            if (data.table::uniqueN(dt, by = combo) == nr) {
                found[[length(found) + 1L]] <- combo
            }
        }
    }

    if (!length(found)) {
        return(empty_result)
    }

    out <- data.table::data.table(
        cols   = vapply(found, paste, character(1L), collapse = ", "),
        n_cols = vapply(found, length, integer(1L))
    )
    data.table::setorder(out, n_cols)
    out[]
}
