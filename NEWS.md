# data.table.ext 0.2.0

* Database helpers are available as `dbdt()`, `as.dbdt()` and `dbdt_*()`.
  All original names, S3 classes and options remain compatible.
* Table and column listing no longer use the unquoted SQL Server reserved
  alias `schema`. Single-table column inspection filters both catalogue
  queries on the server.
* `dbdt_sample(..., method = "fast")` uses SQL Server page sampling to avoid
  randomizing the entire table. It requires a local base table and returns
  at most the requested number of rows, possibly zero; it is not a uniform
  row sample. The default exact random sampling behavior is unchanged.
* SQL Server print previews skip full-table counts by default. Use
  `print(x, count = TRUE)` for an exact total.
