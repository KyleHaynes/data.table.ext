#' Enable `dput()` masking for data.table objects
#'
#' Masks `dput` in the global environment. For `data.table` inputs, it removes
#' the `.internal.selfref` attribute before delegating to the original
#' `base::dput`, so pointer details are not printed.
#'
#' Non-`data.table` inputs are delegated unchanged.
#'
#' @return Invisibly returns `TRUE`.
#' @export
enable_dt_dput_mask <- function() {
    original <- get("dput", envir = asNamespace("base"))
    .dt_print_mask_state$original_dput <- original

    dput <- function(x, file = "", control = NULL) {
        if (!inherits(x, "data.table")) {
            if (missing(control)) {
                return(.dt_print_mask_state$original_dput(x, file = file))
            }
            return(.dt_print_mask_state$original_dput(x, file = file, control = control))
        }

        x_out <- if (requireNamespace("data.table", quietly = TRUE)) data.table::copy(x) else x
        attr(x_out, ".internal.selfref") <- NULL
        if (missing(control)) {
            return(.dt_print_mask_state$original_dput(x_out, file = file))
        }
        .dt_print_mask_state$original_dput(x_out, file = file, control = control)
    }

    assign("dput", dput, envir = .GlobalEnv)
    invisible(TRUE)
}

#' Disable the masked `dput()` function
#'
#' Removes `dput` from the global environment so base `dput` is used normally
#' again.
#'
#' @return Invisibly returns `TRUE`.
#' @export
disable_dt_dput_mask <- function() {
    if (exists("dput", envir = .GlobalEnv, inherits = FALSE)) {
        rm("dput", envir = .GlobalEnv)
    }
    invisible(TRUE)
}
