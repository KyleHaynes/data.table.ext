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

        # .internal.selfref is a C-level external pointer that can't be removed
        # via attr<- or setattr. Capture the raw dput text and strip it with regex.
        tmp <- textConnection("dput_lines", open = "w", local = TRUE)
        on.exit(try(close(tmp), silent = TRUE), add = TRUE)
        if (missing(control)) {
            .dt_print_mask_state$original_dput(x, file = tmp)
        } else {
            .dt_print_mask_state$original_dput(x, file = tmp, control = control)
        }
        close(tmp)
        on.exit(NULL)

        text <- paste(dput_lines, collapse = "\n")
        # Remove ", .internal.selfref = <pointer: ...>)" at the end of structure()
        text <- gsub(
            ",\\s*\\.internal\\.selfref\\s*=\\s*<pointer:[^>]*>\\s*\\)",
            ")",
            text
        )
        if (identical(file, "")) {
            cat(text, "\n", sep = "")
        } else {
            writeLines(text, con = file)
        }
        invisible(x)
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
