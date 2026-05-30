#' Toggle default sample_dt coloring
#'
#' Sets the session default used by `sample_dt()` when `color` is not supplied.
#'
#' @param on Logical scalar. If `TRUE`, sample_dt defaults to colored output.
#'   If `FALSE`, sample_dt defaults to plain output.
#'
#' @return Invisibly returns the new default.
switch_col <- function(on = TRUE) {
    if (!isTRUE(on) && !identical(on, FALSE)) {
        stop("'on' must be TRUE or FALSE.", call. = FALSE)
    }
    options(foam.sample_dt.color = isTRUE(on))
    invisible(isTRUE(on))
}

#' Set one or more columns to NULL by reference
#'
#' Convenience wrapper around `data.table::set()` for deleting columns by
#' reference. This is equivalent to:
#' `data.table::set(x, i = NULL, j = cols, value = NULL)`.
#'
#' @param x A data.table.
#' @param cols Character vector of column names to remove.
#'
#' @return Invisibly returns the modified `x`.
set_null <- function(x, cols) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("Package 'data.table' is required.", call. = FALSE)
    }
    if (!data.table::is.data.table(x)) {
        stop("'x' must be a data.table.", call. = FALSE)
    }
    if (!is.character(cols)) {
        stop("'cols' must be a character vector of column names.", call. = FALSE)
    }
    data.table::set(x, i = NULL, j = cols, value = NULL)
    invisible(x)
}

#' Sample rows from a data.table with optional group expansion
#'
#' Samples from `dt`. If `group` is `NULL`, samples `n` rows.
#' If `group` is provided, samples `n` unique group values and returns all rows
#' for those selected groups. The output is ordered by `group` and tagged for
#' grouped-print separators when used with `enable_dt_print_thousands()`.
#' When `sort_coverage = TRUE` (default), rows are sorted by value commonality
#' (most frequent values first) within each selected group.
#'
#' @param dt A data.table to sample from.
#' @param n Integer sample size. Row count when `group = NULL`; number of
#'   groups when `group` is provided.
#' @param group Optional grouping column, either unquoted (for example `group = d`)
#'   or as a character scalar (for example `group = "d"`).
#' @param color Logical scalar. If `TRUE` (default), annotate sampled output so
#'   print output colors distinct character/factor values by default (distinct
#'   mode). If needed, this will also auto-enable
#'   `enable_dt_print_thousands()` so coloring appears immediately.
#' @param sort_coverage Logical scalar. If `TRUE` (default), sort rows by
#'   commonality within each selected group: rows with more frequent values
#'   appear first. Only applies when `group` is supplied.
#'
#' @return A data.table. If `group` is supplied, returned rows include all
#'   members of selected groups and include print attribute
#'   `".group_print_column"`.
sample_dt <- function(dt, n = 10, group = NULL, color = .sample_dt_color_default(), sort_coverage = TRUE) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("Package 'data.table' is required.", call. = FALSE)
    }
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }

    n <- suppressWarnings(as.integer(n[1L]))
    if (is.na(n) || n <= 0L) {
        stop("'n' must be a positive integer.", call. = FALSE)
    }
    if (!isTRUE(color) && !identical(color, FALSE)) {
        stop("'color' must be TRUE or FALSE.", call. = FALSE)
    }
    if (!isTRUE(sort_coverage) && !identical(sort_coverage, FALSE)) {
        stop("'sort_coverage' must be TRUE or FALSE.", call. = FALSE)
    }

    nr <- nrow(dt)
    if (nr == 0L) {
        return(data.table::copy(dt)[])
    }

    group_expr <- substitute(group)
    has_group <- !identical(group_expr, quote(NULL))

    if (!has_group) {
        n_take <- min(n, nr)
        idx <- sample.int(nr, n_take)
        ans <- dt[idx, ]
        setattr(ans, ".group_print_color_values", isTRUE(color))
        if (isTRUE(color)) {
            setattr(ans, ".group_print_value_mode", "distinct")
            if (!exists("print.data.table", envir = .GlobalEnv, inherits = FALSE)) {
                enable_dt_print_thousands()
            }
        }
        return(ans[])
    }

    group_col <- NULL
    if (is.symbol(group_expr)) {
        group_col <- as.character(group_expr)
    } else if (is.character(group)) {
        group_col <- group[1L]
    }

    if (!is.character(group_col) || length(group_col) != 1L || is.na(group_col) || !nzchar(group_col)) {
        stop("'group' must be a column name (unquoted or character scalar).", call. = FALSE)
    }
    if (!(group_col %in% names(dt))) {
        stop(sprintf("Column '%s' not found in 'dt'.", group_col), call. = FALSE)
    }

    all_groups <- unique(dt[[group_col]])
    ng <- length(all_groups)
    n_take_groups <- min(n, ng)
    selected_groups <- all_groups[sample.int(ng, n_take_groups)]
    ans <- dt[dt[[group_col]] %in% selected_groups, ]
    data.table::setorderv(ans, group_col)
    if (isTRUE(sort_coverage) && nrow(ans) > 1L) {
        coverage_score <- .coverage_sort_score(ans, exclude_cols = group_col)
        ord <- order(ans[[group_col]], -coverage_score, seq_len(nrow(ans)))
        ans <- ans[ord, ]
    }
    setattr(ans, ".group_print_column", group_col)
    setattr(ans, ".group_print_color_values", isTRUE(color))
    if (isTRUE(color)) {
        setattr(ans, ".group_print_value_mode", "distinct")
        if (!exists("print.data.table", envir = .GlobalEnv, inherits = FALSE)) {
            enable_dt_print_thousands()
        }
    }
    ans[]
}

#' Turn every enhancement on with defaults
#'
#' Enables all masking helpers and sets colored grouped sampling output on by
#' default for the current session.
#'
#' @return Invisibly returns `TRUE`.
turn_everyone_on <- function() {
    switch_col(TRUE)
    enable_dt_print_thousands()
    enable_dt_str_mask()
    enable_dt_dput_mask()
    invisible(TRUE)
}
