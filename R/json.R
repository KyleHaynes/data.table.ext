# A minimal JSON writer, just enough to embed a data model in the ERD page.
# Deliberately not a dependency on jsonlite: the only thing serialized here
# is a list of atomic vectors and data frames we generated ourselves.
duckdt_to_json <- function(x) {
  if (is.null(x)) return("null")

  if (is.data.frame(x)) {
    if (nrow(x) == 0) return("[]")
    # Column by column rather than cell by cell: a data model of a large
    # database has tens of thousands of rows, and per-cell escaping made
    # serialising it take longer than reading the catalogue.
    fields <- lapply(names(x), function(nm) {
      paste0(duckdt_json_string(nm), ":", duckdt_json_values(x[[nm]]))
    })
    rows <- paste0("{", do.call(paste, c(fields, sep = ",")), "}")
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
  paste0("[", paste(duckdt_json_values(x), collapse = ","), "]")
}

duckdt_json_scalar <- function(x) {
  if (length(x) == 0) return("null")
  duckdt_json_values(x)
}

# One JSON literal per element of an atomic vector.
duckdt_json_values <- function(x) {
  missing <- is.na(x)
  if (is.logical(x)) {
    out <- ifelse(x, "true", "false")
  } else if (is.numeric(x)) {
    out <- character(length(x))
    # Whole numbers format identically together or apart; anything with a
    # fraction is formatted on its own, so one value's decimals can't pad
    # another's.
    whole <- !missing & is.finite(x) & x == trunc(x) & abs(x) < 1e15
    out[whole] <- format(x[whole], scientific = FALSE, trim = TRUE)
    rest <- !missing & !whole
    out[rest] <- vapply(x[rest], format, character(1), scientific = FALSE, trim = TRUE)
  } else {
    out <- duckdt_json_string(as.character(x))
  }
  out[missing] <- "null"
  out
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
