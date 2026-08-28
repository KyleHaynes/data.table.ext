# Workflow: saving a data.table to a persistent DuckDB file

A step-by-step walkthrough of the most common `duckdt` workflow: take an R
`data.table` you already have in memory, save it into a DuckDB database file
on disk so it survives past the current R session, then reconnect later and
query it. See the main [README](README.md) for everything else `duckdt` can
do once you have a `"duckdt"` handle.

## 1. Save a data.table to a `.duckdb` file

By default `duckdt` opens an **in-memory** DuckDB database that disappears
when the connection closes. To persist data, open the connection yourself
against a file path with `duckdb::duckdb(dbdir = ...)`, then write your
`data.table` into it with `as.duckdt(..., copy = TRUE)`:

```r
library(data.table)
library(duckdt)

sales <- data.table(
  order_id = 1:5,
  customer = c("Alice", "Bob", "Alice", "Carol", "Bob"),
  amount   = c(120.50, 89.00, 45.25, 300.00, 15.75)
)

con <- DBI::dbConnect(duckdb::duckdb(dbdir = "C:/temp/sales.duckdb"))

d <- as.duckdt(sales, conn = con, name = "sales", copy = TRUE)
#   copy = TRUE -> physically writes the data into the file (not a view)
#   name        -> table name inside the database (defaults to the object's
#                  deparsed name, "sales" here, if you omit it)

DBI::dbDisconnect(con, shutdown = TRUE)
```

`copy = TRUE` is required for a persistent table — the default `copy = FALSE`
registers a zero-copy view over the R object in memory, which only exists
for the current session. Use `overwrite = TRUE` if you're re-running this
against a file that already has a `sales` table.

Forward slashes (`"C:/temp/sales.duckdb"`) work fine on Windows and avoid
having to escape backslashes.

## 2. Reconnect later and see what's there

In a new R session (or later in the same one), open the same file again and
list its tables with [`duckdt_tables()`](README.md#exploring-a-database):

```r
con <- DBI::dbConnect(duckdb::duckdb(dbdir = "C:/temp/sales.duckdb"))

duckdt_tables(con)
#>    schema   name       type
#> 1:   main  sales BASE TABLE
```

## 3. Wrap the table and query it

`duckdt(conn, table_name)` wraps an existing table as a `"duckdt"` handle,
after which `d[i, j, by]` works exactly like data.table. Handles from this
constructor are **read-only by default** -- a guard against accidentally
mutating a table you only meant to explore after reconnecting to a file (see
[Writing data](README.md#writing-data)). Reading/querying is unaffected by
this:

```r
d <- duckdt(con, "sales")

d[, .(total = sum(amount), n = .N), by = customer]
#>    customer  total n
#> 1:    Carol 300.00 1
#> 2:    Alice 165.75 2
#> 3:      Bob 104.75 2

d[amount > 50]
```

Every `[` call runs immediately against the file on disk and returns a real
`data.table` — nothing is loaded into R until you ask for it. Row order isn't
guaranteed (DuckDB tables are unordered without an explicit `ORDER BY`) —
add `order(...)` if you need a specific order, e.g. `d[order(customer)]`.

## 4. Write changes back (optional)

`sales` is a materialized table (`copy = TRUE` in step 1), so `:=` *could*
mutate it in place -- but `d` above is still read-only, so this errors:

```r
d[, amount_incl_tax := amount * 1.1]
#> Error: duckdt: `:=` requires a writable handle. ...
```

Opt in explicitly by re-wrapping with `writable = TRUE` once you actually
mean to write:

```r
d <- duckdt(con, "sales", writable = TRUE)
d[, amount_incl_tax := amount * 1.1]   # now allowed, persisted to the file
```

## 5. Disconnect when done

```r
DBI::dbDisconnect(con, shutdown = TRUE)
```

`shutdown = TRUE` flushes DuckDB's write-ahead log and closes the file
cleanly — always pass it when disconnecting from a file-backed database.

## Getting the whole table back into R

If you eventually want the full table back as a plain in-memory
`data.table` (e.g. to hand off to code that isn't DuckDB-aware):

```r
as.data.table(d)   # SELECT * FROM sales, materialized in R
```
