#' Enable data.table print masking with row-index commas and class styling
#'
#' Masks `print.data.table` in the global environment for the current R session.
#' The mask delegates to the original data.table printer, then post-processes
#' the printed output to add thousands separators in row indices, preserve
#' alignment, optionally color class tokens (for example `<num>`), and
#' optionally prepend a column-count header.
#'
#' @param big.mark Character scalar used as the thousands separator for row
#'   indices and `str()` row counts.
#' @param color Logical scalar. If `TRUE` (default), color class tokens in the
#'   class row using the `cli` package.
#' @param class_colors Named character vector or list mapping class tokens such
#'   as `"<num>"` to `cli` color helpers (for example `"col_blue"`). If
#'   `NULL`, defaults are used.
#' @param show_ncol Logical scalar. If `TRUE` (default), print `ncol: <value>`
#'   above the table.
#' @param color_group_values Logical scalar. If `TRUE` (default), and grouped
#'   display is active via `".group_print_column"`, color character/factor
#'   values within each group.
#' @param similarity_max_distance Integer scalar edit-distance threshold used
#'   for grouping similar values when `color_group_values = TRUE`.
#' @param similarity_max_relative Numeric scalar relative edit-distance
#'   threshold used alongside `similarity_max_distance`.
#' @param group_value_mode Character scalar: `"distinct"` (default) or
#'   `"similarity"`. In `"distinct"` mode, every distinct string within each
#'   group/column gets its own color.
#' @param force_color Logical scalar. If `TRUE`, force ANSI color output by
#'   temporarily setting `options(cli.num_colors = 256)` during printing.
#' @param group_sep_fmt A `sprintf`-style format string with one `\%s`
#'   placeholder used to format group separator lines. Default:
#'   `"--------- Group: \%s"`.
#'
#' @return Invisibly returns `TRUE`.
#' @export
enable_dt_print_thousands <- function(
    big.mark = ",",
    color = TRUE,
    class_colors = NULL,
    show_ncol = TRUE,
    color_group_values = TRUE,
    similarity_max_distance = 2L,
    similarity_max_relative = 0.30,
    group_value_mode = c("distinct", "similarity"),
    force_color = TRUE,
    group_sep_fmt = "--------- Group: %s"
) {
    group_value_mode <- match.arg(group_value_mode)
    if (!isTRUE(color) && !identical(color, FALSE)) {
        stop("'color' must be TRUE or FALSE.", call. = FALSE)
    }
    if (!isTRUE(show_ncol) && !identical(show_ncol, FALSE)) {
        stop("'show_ncol' must be TRUE or FALSE.", call. = FALSE)
    }
    if (!isTRUE(color_group_values) && !identical(color_group_values, FALSE)) {
        stop("'color_group_values' must be TRUE or FALSE.", call. = FALSE)
    }
    if (!isTRUE(force_color) && !identical(force_color, FALSE)) {
        stop("'force_color' must be TRUE or FALSE.", call. = FALSE)
    }
    if (!is.character(group_sep_fmt) || length(group_sep_fmt) != 1L || is.na(group_sep_fmt)) {
        stop("'group_sep_fmt' must be a character scalar.", call. = FALSE)
    }

    similarity_max_distance <- suppressWarnings(as.integer(similarity_max_distance[1L]))
    if (is.na(similarity_max_distance) || similarity_max_distance < 0L) {
        stop("'similarity_max_distance' must be a non-negative integer.", call. = FALSE)
    }
    similarity_max_relative <- suppressWarnings(as.numeric(similarity_max_relative[1L]))
    if (is.na(similarity_max_relative) || similarity_max_relative < 0 || similarity_max_relative > 1) {
        stop("'similarity_max_relative' must be between 0 and 1.", call. = FALSE)
    }

    if (is.null(class_colors)) {
        class_colors <- .default_dt_class_colors()
    }

    if (isTRUE(color) && !requireNamespace("cli", quietly = TRUE)) {
        warning("Package 'cli' is not installed; printing without class colors.", call. = FALSE)
        color <- FALSE
    }

    original <- get("print.data.table", envir = asNamespace("data.table"))
    .dt_print_mask_state$original <- original

    print.data.table <- function(x,
        topn = getOption("datatable.print.topn"),
        nrows = getOption("datatable.print.nrows"),
        class = getOption("datatable.print.class"),
        row.names = getOption("datatable.print.rownames"),
        col.names = getOption("datatable.print.colnames"),
        print.keys = getOption("datatable.print.keys"),
        trunc.cols = getOption("datatable.print.trunc.cols"),
        show.indices = getOption("datatable.show.indices"),
        quote = FALSE,
        na.print = NULL,
        timezone = FALSE,
        ...) {
        grp_col <- attr(x, ".group_print_column", exact = TRUE)
        group_color_attr <- attr(x, ".group_print_color_values", exact = TRUE)
        group_disable_color_attr <- attr(x, ".group_print_disable_color", exact = TRUE)
        group_color_values <- isTRUE(group_color_attr)
        allow_color <- isTRUE(color) && !isTRUE(group_disable_color_attr)
        group_mode_attr <- attr(x, ".group_print_value_mode", exact = TRUE)
        group_mode <- if (is.character(group_mode_attr) && length(group_mode_attr) == 1L &&
            !is.na(group_mode_attr) && group_mode_attr %in% c("similarity", "distinct")) {
            group_mode_attr
        } else {
            group_value_mode
        }
        if (!is.null(grp_col)) {
            topn <- nrow(x)
            nrows <- Inf
        }

        if (isTRUE(allow_color) && isTRUE(force_color)) {
            old_opts <- options(cli.num_colors = 256)
            on.exit(options(old_opts), add = TRUE)
        }

        similarity_maps <- list()
        if (isTRUE(allow_color) && isTRUE(group_color_values)) {
            similarity_maps <- .build_group_similarity_maps(
                x = x,
                group_col = grp_col,
                palette = .default_ansi256_palette(),
                mode = group_mode,
                max_distance = similarity_max_distance,
                max_relative = similarity_max_relative
            )
        }

        out <- capture.output(
            .dt_print_mask_state$original(
                x,
                topn = topn,
                nrows = nrows,
                class = class,
                row.names = row.names,
                col.names = col.names,
                print.keys = print.keys,
                trunc.cols = trunc.cols,
                show.indices = show.indices,
                quote = quote,
                na.print = na.print,
                timezone = timezone,
                ...
            )
        )

        if (length(out)) {
            out <- .align_dt_row_indices(out, big.mark = big.mark)
            if (!is.null(grp_col)) {
                out <- .insert_group_separators(out, x = x, group_col = grp_col, sep_fmt = group_sep_fmt)
            }
            if (isTRUE(allow_color)) {
                out <- .colorize_duplicate_headers(out, names(x))
                out <- .colorize_dt_class_rows(out, class_colors = class_colors)
            }
            if (isTRUE(allow_color) && isTRUE(group_color_values)) {
                out <- .colorize_group_headers(out)
                out <- .colorize_group_value_rows(out, x = x, group_col = grp_col, similarity_maps = similarity_maps)
            }
            if (isTRUE(show_ncol)) {
                out <- c(sprintf("ncol: %d", ncol(x)), out)
            }
            cat(paste(out, collapse = "\n"), "\n", sep = "")
        }

        invisible(x)
    }

    assign("print.data.table", print.data.table, envir = .GlobalEnv)
    .register_print_data_table_s3(print.data.table)
    invisible(TRUE)
}

#' Disable the masked data.table print method
#'
#' Removes `print.data.table` from the global environment, restoring default
#' dispatch behavior from attached packages.
#'
#' @return Invisibly returns `TRUE`.
#' @export
disable_dt_print_thousands <- function() {
    if (exists("print.data.table", envir = .GlobalEnv, inherits = FALSE)) {
        rm("print.data.table", envir = .GlobalEnv)
    }
    if (exists("original", envir = .dt_print_mask_state, inherits = FALSE)) {
        .register_print_data_table_s3(.dt_print_mask_state$original)
    }
    invisible(TRUE)
}
