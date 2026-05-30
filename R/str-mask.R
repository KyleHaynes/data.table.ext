#' Enable `str()` masking for data.table objects
#'
#' Masks `str` in the global environment. For `data.table` inputs, this mask
#' sets defaults of `list.len = Inf` and `max.level = 1` when those arguments
#' are not explicitly provided, rewrites the header line to a data.table-focused
#' format, normalizes `":List"` to `": List"`, and removes the
#' `.internal.selfref` attribute line from output.
#'
#' Non-`data.table` inputs are delegated to the original `utils::str` without
#' changing defaults.
#'
#' @param big.mark Character scalar used as the thousands separator in the
#'   printed row count in the first line.
#'
#' @return Invisibly returns `TRUE`.
enable_dt_str_mask <- function(big.mark = ",") {
    original <- get("str", envir = asNamespace("utils"))
    .dt_print_mask_state$original_str <- original

    str <- function(object, list.len, max.level, ...) {
        list.len_missing <- missing(list.len)
        max.level_missing <- missing(max.level)

        args <- list(object = object, ...)

        if (inherits(object, "data.table")) {
            if (list.len_missing) {
                list.len <- Inf
            }
            if (max.level_missing) {
                max.level <- 1
            }
        }

        if (!list.len_missing || inherits(object, "data.table")) {
            args$list.len <- list.len
        }
        if (!max.level_missing || inherits(object, "data.table")) {
            args$max.level <- max.level
        }

        if (!inherits(object, "data.table")) {
            return(do.call(.dt_print_mask_state$original_str, args))
        }

        out <- capture.output(do.call(.dt_print_mask_state$original_str, args))
        if (length(out)) {
            out[1L] <- sprintf(
                "A 'data.table': %s rows, %d variables.",
                format(nrow(object), big.mark = big.mark, scientific = FALSE, trim = TRUE),
                ncol(object)
            )
            out <- out[!grepl('^\\s*- attr\\(\\*, "\\.internal\\.selfref"\\)=<externalptr>\\s*$', out)]
            out <- gsub(":List", ": List", out, fixed = TRUE)
            cat(paste(out, collapse = "\n"), "\n", sep = "")
        }
        invisible(object)
    }

    assign("str", str, envir = .GlobalEnv)
    invisible(TRUE)
}

#' Disable the masked `str()` function
#'
#' Removes `str` from the global environment so base `utils::str` is used
#' normally again.
#'
#' @return Invisibly returns `TRUE`.
disable_dt_str_mask <- function() {
    if (exists("str", envir = .GlobalEnv, inherits = FALSE)) {
        rm("str", envir = .GlobalEnv)
    }
    invisible(TRUE)
}
