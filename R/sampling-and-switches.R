#' Toggle default sample_dt coloring
#'
#' Sets the session default used by `sample_dt()` when `color` is not supplied.
#'
#' @param on Logical scalar. If `TRUE`, sample_dt defaults to colored output.
#'   If `FALSE`, sample_dt defaults to plain output.
#'
#' @return Invisibly returns the new default.
#' @import data.table
#' @export
switch_col <- function(on = TRUE) {
    # Accept only a strict TRUE/FALSE toggle.
    if (!isTRUE(on) && !identical(on, FALSE)) {
        stop("'on' must be TRUE or FALSE.", call. = FALSE)
    }
    # Store the default used by sample_dt() when color is omitted.
    options(foam.sample_dt.color = isTRUE(on))
    # Return the applied default invisibly.
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
#' @export
set_null <- function(x, cols) {
    # Require data.table so reference updates are available.
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("Package 'data.table' is required.", call. = FALSE)
    }
    # Ensure the input really is a data.table.
    if (!data.table::is.data.table(x)) {
        stop("'x' must be a data.table.", call. = FALSE)
    }
    # Require a character vector of column names.
    if (!is.character(cols)) {
        stop("'cols' must be a character vector of column names.", call. = FALSE)
    }
    # Remove the requested columns by reference.
    data.table::set(x, i = NULL, j = cols, value = NULL)
    # Return the modified table invisibly.
    invisible(x)
}

.resolve_group_column <- function(group_expr, group) {
    group_col <- NULL
    if (is.symbol(group_expr)) {
        group_col <- as.character(group_expr)
    } else if (is.character(group)) {
        group_col <- group[1L]
    }

    # Reject invalid group values before doing any data work.
    if (!is.character(group_col) || length(group_col) != 1L || is.na(group_col) || !nzchar(group_col)) {
        stop("'group' must be a column name (unquoted or character scalar).", call. = FALSE)
    }

    group_col
}

.group_display_table <- function(dt, group_col = NULL, color, sort_coverage, color_threshold = 500L) {
    color_threshold <- suppressWarnings(as.integer(color_threshold[1L]))
    if (is.na(color_threshold) || color_threshold < 0L) {
        stop("'color_threshold' must be a non-negative integer.", call. = FALSE)
    }

    ans <- data.table::copy(dt)
    if (!is.null(group_col) && nrow(ans) > 1L) {
        # Order by group before any within-group scoring is applied.
        data.table::setorderv(ans, group_col)
        if (isTRUE(sort_coverage)) {
            # Score rows by how common their values are across the selected table.
            coverage_score <- .coverage_sort_score(ans, exclude_cols = group_col)
            # Sort within each group by score, then preserve a stable tie-break.
            ord <- order(ans[[group_col]], -coverage_score, seq_len(nrow(ans)))
            ans <- ans[ord, ]
        }
    }

    # Store the group column name for the print mask when grouped display is requested.
    if (!is.null(group_col)) {
        setattr(ans, ".group_print_column", group_col)
    }

    # Disable all colors if any group exceeds the threshold, or if the full table is too large.
    group_sizes <- if (is.null(group_col)) nrow(ans) else as.integer(table(ans[[group_col]], useNA = "ifany"))
    allow_color <- isTRUE(color)
    if (length(group_sizes) && any(group_sizes > color_threshold, na.rm = TRUE)) {
        allow_color <- FALSE
        setattr(ans, ".group_print_disable_color", TRUE)
    }

    # Preserve the requested color behavior for downstream printing.
    setattr(ans, ".group_print_color_values", allow_color)
    if (isTRUE(allow_color)) {
        # Default to distinct value coloring for grouped output.
        setattr(ans, ".group_print_value_mode", "distinct")
        # Enable the print mask on demand so grouped output is immediately enhanced.
        if (!exists("print.data.table", envir = .GlobalEnv, inherits = FALSE)) {
            enable_dt_print_thousands()
        }
    }

    ans[]
}

#' Evaluate an expression inside `j`
#'
#' Convenience wrapper for evaluating an expression in the calling
#' `data.table` j environment. If the result looks like a column selector, it
#' returns the matching column subset from the current `.SD` table.
#'
#' @param expr An expression to evaluate.
#'
#' @return The value of the evaluated expression, or a selected data.table when
#'   the result is a column selector.
#' @export
e <- function(expr) {
    expr_call <- substitute(expr)
    expr_value <- eval(expr_call, parent.frame())

    if (is.null(expr_value)) {
        return(expr_value)
    }
    if (!is.character(expr_value) && !is.integer(expr_value) && !is.logical(expr_value)) {
        return(expr_value)
    }

    selected_names <- if (is.integer(expr_value)) {
        expr_value
    } else if (is.logical(expr_value)) {
        expr_value
    } else {
        expr_value
    }

    symbol_names <- unique(all.names(expr_call, functions = FALSE))
    table <- NULL
    
    # Search up the call stack to find the data.table being processed.
    # When called from data.table j, the table reference is typically 2-3 frames up.
    for (frame_depth in 1:10) {
        tryCatch({
            frame_env <- parent.frame(frame_depth)
            # Look for common data.table variable names first.
            for (var_name in c("DT", "dt", "data", "x", ".BY", ".SD")) {
                candidate <- tryCatch(get(var_name, envir = frame_env, inherits = FALSE), 
                                    error = function(e) NULL)
                if (data.table::is.data.table(candidate) && nrow(candidate) > 0) {
                    table <- candidate
                    break
                }
            }
            if (!is.null(table)) break
        }, error = function(e) NULL)
    }
    
    # If still not found, search by symbol names from the expression.
    if (is.null(table)) {
        for (symbol_name in symbol_names) {
            if (identical(symbol_name, "x")) {
                next
            }
            candidate <- tryCatch(get(symbol_name, envir = parent.frame(), inherits = TRUE), 
                                error = function(e) NULL)
            if (data.table::is.data.table(candidate)) {
                table <- candidate
                break
            }
        }
    }

    # As last resort, search for any data.table in parent frames.
    if (is.null(table)) {
        for (frame_depth in 1:10) {
            tryCatch({
                parent_env <- parent.frame(frame_depth)
                parent_names <- ls(envir = parent_env, all.names = TRUE)
                for (name in parent_names) {
                    candidate <- tryCatch(get(name, envir = parent_env, inherits = FALSE), 
                                        error = function(e) NULL)
                    if (data.table::is.data.table(candidate) && nrow(candidate) > 0) {
                        table <- candidate
                        break
                    }
                }
                if (!is.null(table)) break
            }, error = function(e) NULL)
        }
    }

    if (is.null(table)) {
        return(expr_value)
    }

    if (is.integer(selected_names)) {
        selected_names <- names(table)[selected_names]
    } else if (is.logical(selected_names)) {
        selected_names <- names(table)[selected_names]
    }

    selected <- tryCatch(
        data.table::as.data.table(setNames(lapply(selected_names, function(col_name) table[[col_name]]), selected_names)),
        error = function(e) NULL
    )
    if (!is.null(selected)) {
        return(selected)
    }

    expr_value
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
#' @export
sample_dt <- function(dt, n = 10, group = NULL, color = .sample_dt_color_default(), sort_coverage = TRUE) {
    # Require data.table so sampling and by-reference helpers are available.
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("Package 'data.table' is required.", call. = FALSE)
    }
    # Ensure the input is a data.table before sampling.
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }

    # Coerce n to a single positive integer.
    n <- suppressWarnings(as.integer(n[1L]))
    if (is.na(n) || n <= 0L) {
        stop("'n' must be a positive integer.", call. = FALSE)
    }
    # Enforce a strict logical flag for color.
    if (!isTRUE(color) && !identical(color, FALSE)) {
        stop("'color' must be TRUE or FALSE.", call. = FALSE)
    }
    # Enforce a strict logical flag for sort_coverage.
    if (!isTRUE(sort_coverage) && !identical(sort_coverage, FALSE)) {
        stop("'sort_coverage' must be TRUE or FALSE.", call. = FALSE)
    }

    # Handle the empty-table case early.
    nr <- nrow(dt)
    if (nr == 0L) {
        return(data.table::copy(dt)[])
    }

    # Detect whether the caller supplied a group column.
    group_expr <- substitute(group)
    has_group <- !identical(group_expr, quote(NULL))

    # Sample rows directly when no grouping is requested.
    if (!has_group) {
        n_take <- min(n, nr)
        # Draw a simple random row sample.
        idx <- sample.int(nr, n_take)
        ans <- dt[idx, ]
        # Preserve grouped-print metadata for downstream print helpers.
        setattr(ans, ".group_print_color_values", isTRUE(color))
        if (isTRUE(color)) {
            # Default to distinct value coloring for non-grouped sampling.
            setattr(ans, ".group_print_value_mode", "distinct")
            # Enable the print mask on demand so the sampled output looks enhanced immediately.
            if (!exists("print.data.table", envir = .GlobalEnv, inherits = FALSE)) {
                enable_dt_print_thousands()
            }
        }
        # Return the sampled table with its attributes intact.
        return(ans[])
    }

    # Resolve the grouping column name from either quoted or unquoted input.
    group_col <- .resolve_group_column(group_expr, group)
    # Ensure the named grouping column exists.
    if (!(group_col %in% names(dt))) {
        stop(sprintf("Column '%s' not found in 'dt'.", group_col), call. = FALSE)
    }

    # Pick the sampled groups first, then return every row from those groups.
    all_groups <- unique(dt[[group_col]])
    ng <- length(all_groups)
    n_take_groups <- min(n, ng)
    # Randomly choose which groups to include in the sample.
    selected_groups <- all_groups[sample.int(ng, n_take_groups)]
    # Filter down to the chosen groups.
    ans <- dt[dt[[group_col]] %in% selected_groups, ]
    .group_display_table(ans, group_col = group_col, color = color, sort_coverage = sort_coverage)
}

#' Display all rows grouped for printing
#'
#' Returns the full table with grouped-print attributes attached so
#' `enable_dt_print_thousands()` can color values by group.
#' If any group exceeds `color_threshold`, color output is disabled for that
#' result.
#'
#' @param dt A data.table to display.
#' @param group Optional grouping column, either unquoted or as a character
#'   scalar. If `NULL`, the full table is colored as one display unit.
#' @param color Logical scalar. If `TRUE` (default), annotate output so print
#'   output colors distinct character/factor values by default.
#' @param sort_coverage Logical scalar. If `TRUE` (default), sort rows by
#'   commonality within each group: rows with more frequent values appear first.
#' @param color_threshold Integer scalar. If any group has more rows than this,
#'   color output is disabled for the returned table.
#'
#' @return A data.table with grouped-print attributes.
#' @export
cdt <- function(dt, group = NULL, color = .sample_dt_color_default(), sort_coverage = TRUE, color_threshold = 500L) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("Package 'data.table' is required.", call. = FALSE)
    }
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    if (!isTRUE(color) && !identical(color, FALSE)) {
        stop("'color' must be TRUE or FALSE.", call. = FALSE)
    }
    if (!isTRUE(sort_coverage) && !identical(sort_coverage, FALSE)) {
        stop("'sort_coverage' must be TRUE or FALSE.", call. = FALSE)
    }

    group_expr <- substitute(group)
    group_col <- NULL
    if (!identical(group_expr, quote(NULL))) {
        group_col <- .resolve_group_column(group_expr, group)
        if (!(group_col %in% names(dt))) {
            stop(sprintf("Column '%s' not found in 'dt'.", group_col), call. = FALSE)
        }
    }

    .group_display_table(dt, group_col = group_col, color = color, sort_coverage = sort_coverage, color_threshold = color_threshold)
}

#' Turn every enhancement on with defaults
#'
#' Enables all masking helpers and sets colored grouped sampling output on by
#' default for the current session.
#'
#' @return Invisibly returns `TRUE`.
#' @export
turn_everyone_on <- function() {
    # Turn on the default color flag for sample_dt().
    switch_col(TRUE)
    # Enable the print mask for clearer tables.
    enable_dt_print_thousands()
    # Enable the str() mask for cleaner structure output.
    enable_dt_str_mask()
    # Enable the dput() mask for cleaner reproducibility output.
    enable_dt_dput_mask()
    # Return success invisibly.
    invisible(TRUE)
}
