# A connection that quotes the way DBI's defaults do -- every identifier in
# double quotes, every string in single quotes -- for tests that check the
# exact SQL text. DuckDB's own connection leaves identifiers unquoted where it
# can, which would make those expectations depend on the duckdb version.
ansi_conn <- function() {
  if (!methods::isClass("AnsiTestConn")) {
    methods::setClass("AnsiTestConn", contains = "DBIConnection", slots = c(id = "character"))
  }
  methods::new("AnsiTestConn", id = "ansi")
}
