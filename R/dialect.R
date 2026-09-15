# Internal: which SQL dialect to emit for a given connection. Auto-detected
# from the connection's class so existing DuckDB users see no change; a live
# SQL Server connection opened via odbc::dbConnect() carries the S4 class
# "Microsoft SQL Server" (odbc tags connections with the driver-reported DBMS
# name), which is what we key off of here.
duckdt_dialect <- function(conn) {
  if (inherits(conn, "Microsoft SQL Server")) "mssql" else "duckdb"
}
