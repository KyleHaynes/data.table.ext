#' Render a data.table as a markdown table
#'
#' Renders `dt` as a GitHub-flavored markdown pipe table and prints it to the
#' console, for pasting directly into docs, PRs, or issues.
#'
#' @param dt A data.table.
#' @param n Maximum number of rows to render. Default `Inf` (all rows).
#'
#' @return Invisibly returns the character scalar that was printed.
#' @examples
#' DT <- data.table::as.data.table(iris)[1:3]
#' md_dt(DT)
#' @export
md_dt <- function(dt, n = Inf) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    txt <- .render_md_table(dt, n = n)
    cat(txt, "\n", sep = "")
    invisible(txt)
}

#' Copy a data.table to the clipboard as a markdown table
#'
#' Renders `dt` as a markdown table (see `md_dt()`) and copies the result to
#' the system clipboard, for pasting into docs, PRs, or chat messages.
#' Supported on Windows and macOS out of the box; on Linux, requires `xclip`
#' or `xsel` to be installed.
#'
#' @param dt A data.table.
#' @param n Maximum number of rows to include. Default `Inf` (all rows).
#' @param clip_fun Function used to write the rendered text to the clipboard,
#'   taking a single character scalar. Defaults to the package's built-in
#'   cross-platform clipboard writer; override for testing or to route output
#'   elsewhere.
#'
#' @return Invisibly returns the character scalar that was copied.
#' @examples
#' DT <- data.table::as.data.table(iris)[1:3]
#' # clip_fun lets you try this without touching the real clipboard
#' copy_dt(DT, clip_fun = function(txt) cat("would copy", nchar(txt), "characters\n"))
#' @export
copy_dt <- function(dt, n = Inf, clip_fun = .write_clipboard) {
    if (!data.table::is.data.table(dt)) {
        stop("'dt' must be a data.table.", call. = FALSE)
    }
    txt <- .render_md_table(dt, n = n)
    clip_fun(txt)
    invisible(txt)
}
