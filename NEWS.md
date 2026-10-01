# data.table.ext (development version)

## SQL Server and large databases

* A handle looks its table up once and keeps its column list, so a query
  sends only the query. On SQL Server, `dbdt()` finds the table with a single
  `OBJECT_ID()` lookup and columns come from `sys.columns`, replacing ODBC's
  `SQLTables`/`SQLColumns` pattern searches and `INFORMATION_SCHEMA`, which
  each `[` call used to pay for twice. `:=` refreshes the column list.
* `dbdt_schema()`, `dbdt_tables()` and so `dbdt_data_model()`, `dbdt_erd()`
  and `dbdt_explorer()` read SQL Server's `sys.*` catalogue views directly.
  With `row_counts = TRUE`, every table's row count comes from partition
  metadata in one query instead of a `count(*)` per table.
* Building, inferring, drawing and serialising a data model is vectorised.
  On a 2,000-table, 50,000-column catalogue, `dbdt_dm_infer_references()`
  went from 306 s to 0.2 s, `dbdt_erd()` from 25 s to 0.4 s and
  `dbdt_dm_mermaid()` from 10 s to 0.1 s, with identical output.
* `print()` on a SQL Server table shows the row count from partition
  metadata, marked `~`, instead of `?`.
* `options(duckdt.trace = TRUE)` prints every statement sent and how long it
  took. New `dbdt_sql()` returns the SQL `d[i, j, by]` would run.
* `dbdt(con, table, schema = "sales")` reaches tables outside the default
  schema.

## Writes

* `:=` changes a table in place: `UPDATE` for an existing column (with `i`
  as the `WHERE` clause), `ALTER TABLE ... ADD` then `UPDATE` for a new one,
  `col := NULL` to drop one. Primary keys, constraints, indexes and (on SQL
  Server) permissions and triggers are kept; previously every assigned column
  rebuilt the whole table. All assignments in a call are translated before
  anything is written and run in one transaction. A new column from an R
  literal gets R's type, and new SQL Server text columns are widened, so a
  later, longer value fits.
* `dbdt_merge()` and `dbdt_join()` stage data under random names, so they can
  no longer replace and drop a table of yours that happens to share the
  derived name. On SQL Server, `dbdt_temp(name = )` refuses to replace a
  table this session didn't create.
* `as.dbdt()`, `dbdt_csv()` and `dbdt_parquet()` close the connection they
  opened if setting up the table fails.

## Queries

* New translations: `ifelse()`, `fifelse()`, `fcase()`, `uniqueN()`,
  `between()`, `%notin%`, `paste0()`, `paste()`, `substr()`, `substring()`,
  `grepl()`, `startsWith()`, `endsWith()`, `trimws()`, `as.numeric()`,
  `as.integer()`, `as.character()`, `as.Date()`, `pmin()`, `pmax()`, `log()`
  with a base, `log2()`, `log10()`, `trunc()`, `^`, `%%`, `%/%`, and the
  date parts `year()`, `month()`, `quarter()`, `mday()`, `wday()`, `yday()`,
  `hour()`, `minute()`, `second()`. `pkg::` prefixes are ignored.
* `order()` in `i` sorts in the database.
* `na.rm` is accepted and dropped instead of producing invalid SQL.
* Date-time values from R are quoted correctly in a query.
* On DuckDB, `log(x)` is now the natural logarithm (it was base 10), and
  `mean()` of a comparison works.
* On SQL Server: comparisons can be used as values (`sum(x > 1)`,
  `.(big = x > 1)`) and bit columns as conditions; `mean()` and `/` no longer
  do integer arithmetic; `round(x)` is valid; `median()` works in `j`; and a
  `%like%` pattern that is plain text becomes `LIKE`, so it works on every
  version instead of needing SQL Server 2025.
* `dbdt_dm_query()` writes `TOP (n)` for SQL Server (`dialect =`, taken from
  the connection), so the Shiny explorer's row limit works there.

## New functions

* `dbdt_profile()`: missing, distinct, min and max for every column, computed
  in the database in one query.
* `dbdt_sql()`: the SQL a query would run.
* `compare_dt()`: every difference between two versions of a `data.table`.

## Documentation

* The README is now an overview pointing into the documentation site, which
  covers every function. `WORKFLOW.md` was folded into the site's
  "A database that outlives the session" section.

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
