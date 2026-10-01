# Every statement duckdt sends goes through these, so `options(duckdt.trace =
# TRUE)` can show what was sent and how long the database took over it --
# the first thing to look at when a call against a large or remote database
# is slower than expected. They call the DBI generics at call time, so the
# drivers' own methods (and test doubles of them) are what actually run.

duckdt_get_query <- function(conn, sql, ...) {
  duckdt_traced(sql, DBI::dbGetQuery(conn, sql, ...))
}

duckdt_execute <- function(conn, sql, ...) {
  duckdt_traced(sql, DBI::dbExecute(conn, sql, ...))
}

duckdt_write_table <- function(conn, name, value, ...) {
  duckdt_traced(
    sprintf("-- dbWriteTable(%s): %s rows x %d columns", name,
      format(nrow(value), big.mark = ","), ncol(value)),
    DBI::dbWriteTable(conn, name, value, ...)
  )
}

# `expr` is evaluated lazily, inside the timer. The line is written even when
# the statement fails, since a slow failure is as worth seeing as a slow
# success.
duckdt_traced <- function(sql, expr) {
  if (!isTRUE(getOption("duckdt.trace"))) return(expr)
  start <- proc.time()[["elapsed"]]
  on.exit(message(sprintf(
    "[duckdt %6.3fs] %s", proc.time()[["elapsed"]] - start,
    gsub("\\s+", " ", trimws(as.character(sql)))
  )), add = TRUE)
  expr
}
