# Binary columns don't survive the trip into R.
#
# Reading a SQL Server `varbinary`/`binary`/`image` column over ODBC errors
# on fetch, and DuckDB's own `BIT` type fails at prepare time ("Unknown
# column type for prepare: BIT"). Either way the *whole* query errors, so one
# such column makes an otherwise ordinary table unreadable -- you can't even
# head() it to see what is in there.
#
# So the `SELECT *` duckdt expands whenever rows are on their way into R
# leaves binary columns out. That covers DuckDB's `BLOB`, which does come
# back (as a list of raw vectors), so a table reads the same way whichever
# dialect it is on.
#
# Naming a column in `j` overrides all of this: `d[, .(payload)]` selects
# exactly what you asked for, and fails if the driver can't fetch it. Nothing
# here touches what stays in the database -- duckdt_temp(), duckdt_merge()
# and `:=` still see every column.

# The column list to put after SELECT for rows headed into R. "*" whenever
# nothing needs excluding, so the ordinary case emits exactly the SQL it
# always did.
duckdt_star <- function(x, cols = NULL) {
  bin <- duckdt_binary_cols(x)
  if (!length(bin)) return("*")
  if (is.null(cols)) cols <- duckdt_columns(x)
  duckdt_binary_notify(x$tbl, intersect(cols, bin))
  duckdt_select_list(x$conn, setdiff(cols, bin), x$tbl)
}

duckdt_select_list <- function(conn, keep, tbl) {
  if (!length(keep)) {
    stop(sprintf(
      paste0(
        "duckdt: every column of '%s' is a binary type, and binary columns ",
        "cannot be converted to R. Reduce them in the database instead -- ",
        "e.g. `d[, .(n = length(payload))]` or duckdt_temp()."
      ), tbl), call. = FALSE)
  }
  paste(as.character(DBI::dbQuoteIdentifier(conn, keep)), collapse = ", ")
}

duckdt_binary_cols <- function(x) {
  types <- duckdt_column_types(x$conn, x$tbl)
  if (!nrow(types)) return(character(0))
  unique(types$column[duckdt_is_binary_type(types$type, duckdt_dialect(x$conn))])
}

# information_schema.columns is the one column catalogue both DuckDB and MS
# SQL Server expose (DuckDB lists its TEMP tables and registered views there
# too). A backend without it, or a table it can't see, leaves us knowing
# nothing -- which is where duckdt stood before this existed, so fall back to
# excluding nothing rather than erroring.
duckdt_column_types <- function(conn, table) {
  none <- data.frame(column = character(0), type = character(0))
  res <- tryCatch(
    DBI::dbGetQuery(conn, paste0(
      "SELECT column_name AS \"column\", data_type AS \"type\"",
      " FROM information_schema.columns WHERE table_name = ",
      DBI::dbQuoteString(conn, table),
      " ORDER BY ordinal_position"
    )),
    error = function(e) none
  )
  if (!is.data.frame(res) || !nrow(res)) none else res
}

# `data_type` is the bare type name in both catalogues -- "varbinary", not
# "varbinary(max)" -- but strip a length or element suffix anyway so an
# unusual driver can't slip one past. The two lists can't be merged: SQL
# Server's `timestamp` is rowversion (a binary(8)) while DuckDB's TIMESTAMP
# is a datetime, and DuckDB's `BIT` is a bitstring while SQL Server's is a
# boolean.
duckdt_is_binary_type <- function(type, dialect) {
  base <- tolower(trimws(sub("\\(.*$", "", sub("\\[.*$", "", type))))
  if (dialect == "mssql") {
    base %in% c("binary", "varbinary", "image", "timestamp", "rowversion")
  } else {
    base %in% c("blob", "bytea", "binary", "varbinary", "bit")
  }
}

# Once per table per session: enough that the columns don't vanish silently,
# not enough to drown a loop querying the same table over and over. Passing
# `announce = FALSE` claims the slot without saying anything, for callers
# (print.duckdt) that report the skipped columns themselves.
duckdt_binary_notify <- function(tbl, cols, announce = TRUE) {
  key <- paste0("binary_noted:", tbl)
  seen <- isTRUE(duckdt_env[[key]])
  assign(key, TRUE, envir = duckdt_env)
  if (seen || !announce || !length(cols) || isTRUE(getOption("duckdt.quiet"))) {
    return(invisible(FALSE))
  }
  ex <- paste0("d[, .(", cols[1], ")]")
  cli::cli_inform(c(
    "!" = "{cli::qty(length(cols))}Not fetching binary column{?/s} from {.val {tbl}}: {.val {cols}} -- binary columns don't convert to R.",
    ">" = "Name one in {.code j} to fetch it anyway, e.g. {.code {ex}}."
  ))
  invisible(TRUE)
}
