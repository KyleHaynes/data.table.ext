# A minimal JSON writer, just enough to embed a data model in the ERD page.
# Deliberately not a dependency on jsonlite: the only thing serialized here
# is a list of atomic vectors and data frames we generated ourselves.
duckdt_to_json <- function(x) {
  if (is.null(x)) return("null")

  if (is.data.frame(x)) {
    if (nrow(x) == 0) return("[]")
    rows <- vapply(seq_len(nrow(x)), function(i) {
      fields <- vapply(names(x), function(nm) {
        paste0(duckdt_json_string(nm), ":", duckdt_json_scalar(x[[nm]][i]))
      }, character(1))
      paste0("{", paste(fields, collapse = ","), "}")
    }, character(1))
    return(paste0("[", paste(rows, collapse = ","), "]"))
  }

  if (is.list(x)) {
    if (!length(x)) return(if (is.null(names(x))) "[]" else "{}")
    parts <- vapply(x, duckdt_to_json, character(1))
    if (is.null(names(x))) return(paste0("[", paste(parts, collapse = ","), "]"))
    return(paste0("{", paste(
      paste0(vapply(names(x), duckdt_json_string, character(1)), ":", parts),
      collapse = ","
    ), "}"))
  }

  if (length(x) == 1 && is.null(names(x))) return(duckdt_json_scalar(x))
  paste0("[", paste(vapply(x, duckdt_json_scalar, character(1)), collapse = ","), "]")
}

duckdt_json_scalar <- function(x) {
  if (length(x) == 0 || is.na(x)) return("null")
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.numeric(x)) return(format(x, scientific = FALSE, trim = TRUE))
  duckdt_json_string(as.character(x))
}

duckdt_json_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  # The result is embedded in a <script> block, so no character of it may be
  # able to close that block early.
  x <- gsub("<", "\\u003c", x, fixed = TRUE)
  x <- gsub(">", "\\u003e", x, fixed = TRUE)
  x <- gsub("&", "\\u0026", x, fixed = TRUE)
  paste0('"', x, '"')
}
