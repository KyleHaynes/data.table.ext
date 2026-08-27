# duckdt

Query [DuckDB](https://duckdb.org/) using [data.table](https://r-datatable.com/)'s
`d[i, j, by]` syntax. Expressions are translated to SQL and run inside DuckDB — data
only comes back to R as a `data.table` once you materialize a result.

```r
library(duckdt)

d <- as.duckdt(mtcars)      # zero-copy: registers mtcars as a DuckDB view
d[cyl == 6]                          # filter
d[cyl == 6, .(mpg, hp)]              # filter + select
d[, .(avg_mpg = mean(mpg), n = .N), by = cyl]   # group by + aggregate
```

Every `[` call executes immediately in DuckDB and returns a real `data.table`, so you
can keep chaining exactly like you would with data.table itself:

```r
d[cyl == 6][order(-mpg)][1:3]
```

## Getting data in

```r
as.duckdt(mtcars)                     # zero-copy view (read-only)
as.duckdt(mtcars, copy = TRUE)        # physical, writable DuckDB table
duckdt(conn, "existing_table")        # wrap a table/view you already have
duckdt_csv("big_file.csv")            # lazy view over a CSV, out-of-core
duckdt_parquet("data/*.parquet")      # lazy view over Parquet file(s)
```

## Writing data

`:=` mutates a **materialized** table in place (create one with `copy = TRUE`):

```r
d <- as.duckdt(mtcars, copy = TRUE)
d[, kw := hp * 0.7457]                # add a computed column
d[cyl == 6, kw := kw * 1.1]           # update matching rows only
```

`duckdt_merge()` merges a subset (`data.frame`/`data.table`, or another `"duckdt"`
table/view/query result) into a **materialized** table -- entirely inside the
database. `y` is staged into a temporary table on `x`'s connection first, so `x`'s
existing data never round-trips through R; matching rows are updated, unmatched `y`
rows are inserted, and (opt-in) unmatched `x` rows can be deleted:

```r
d <- as.duckdt(mtcars, copy = TRUE)
patch <- data.frame(car = c("Mazda RX4", "New Car"), hp = c(999, 111))
duckdt_merge(d, patch, by = "car")                    # update matches, insert new rows
duckdt_merge(d, patch, by = "car", insert = FALSE)     # update only, skip new rows
duckdt_merge(d, patch, by = "car", delete = TRUE)      # + delete rows not in `patch`
```

On DuckDB this compiles to `UPDATE ... FROM` + an anti-join `INSERT` (+ an anti-join
`DELETE` if `delete = TRUE`), wrapped in a transaction. On MS SQL Server it compiles
to a single native T-SQL `MERGE` statement. If `by` is omitted it defaults to every
column shared between `x` and `y` (a natural join, like base `merge()`) -- pass `by=`
explicitly whenever `y` carries value columns that should be updated rather than
matched on.

## Getting data out

```r
as.data.table(d)   # SELECT * FROM <table>, materialized in R
```

## What's translated

- **`i`**: `==`, `!=`, `<`, `<=`, `>`, `>=`, `&`, `|`, `!`, `%in%`, `%chin%`,
  `%between%`, `%like%`, `%ilike%`, `%flike%`, `%plike%`, `is.na()`, arithmetic,
  and common scalar functions.
- **`j`**: bare column, `.(...)`/`list(...)` for select/rename/compute, aggregates
  (`sum`, `mean`, `min`, `max`, `sd`, `var`, `median`, `.N`, ...).
- **`by`**: bare column, character vector, or `.(...)`/`list(...)`/`c(...)`.

Notes on the `%like%` family: like data.table, these are **regex** matches (not
SQL `LIKE` wildcard syntax) except `%flike%`, which is a literal substring match.
They translate to DuckDB's `regexp_matches()`/`contains()`. DuckDB's regex engine
(RE2) doesn't support PCRE backreferences or lookaround, so `%plike%` is a
best-effort alias of `%like%` rather than true Perl-regex support.

## Peeking and sampling

```r
head(d, 3)          # SELECT * ... LIMIT 3
tail(d, 3)           # SELECT * ... LIMIT 3 OFFSET (nrow - 3)
duckdt_sample(d, 5)  # SELECT * ... USING SAMPLE reservoir(5 ROWS)
```

`tail()` reflects DuckDB's current scan order rather than a guaranteed original
row order, since DuckDB tables are unordered without an explicit `ORDER BY` (see
the row-position caveat below).

## MS SQL Server support

`duckdt` also works against a Microsoft SQL Server connection (e.g. via
`DBI::dbConnect(odbc::odbc(), ...)`) — the dialect is auto-detected from the
connection object, so no extra argument is needed:

```r
con <- DBI::dbConnect(odbc::odbc(), driver = "ODBC Driver 18 for SQL Server", ...)
d <- duckdt(con, "existing_table")
d[cyl == 6, .(avg_hp = mean(hp), n = .N), by = cyl]
```

What's identical to the DuckDB path: `i`/`j`/`by` translation for
comparisons, `&`/`|`/`!`, `%in%`/`%chin%`, `%between%`, `is.na()`,
arithmetic, and most scalar/aggregate functions.

What's dialect-specific:
- `median()` compiles to `PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ...)`
  (T-SQL has no `MEDIAN()` aggregate).
- `head()`/`print()` use `TOP (n)`, `tail()` uses
  `ORDER BY (SELECT NULL) OFFSET ... FETCH NEXT ...`, and `duckdt_sample()`
  uses `ORDER BY NEWID()` (SQL Server has no `LIMIT`/`OFFSET` or exact-row
  `TABLESAMPLE`).
- `%flike%` (literal substring) uses `CHARINDEX(...) > 0`.
- `%like%`/`%ilike%`/`%plike%` (regex matching) compile to T-SQL's native
  `REGEXP_LIKE()` instead of DuckDB's `regexp_matches()`. Both engines use
  Google's RE2 as their regex engine, so the same pattern text works on
  either, and `%plike%` is a best-effort alias of `%like%` on both (RE2
  doesn't support PCRE backreferences/lookaround). **`REGEXP_LIKE()` requires
  SQL Server 2025 (or Azure SQL Database/Managed Instance/Fabric SQL DB) and
  database compatibility level 170+** — against an older SQL Server this
  surfaces as a plain "not a recognized built-in function name" error from
  the server itself.

What's **not** supported against MS SQL Server:
- Zero-copy `as.duckdt(x, copy = FALSE)` — registering an R data frame as a
  view with no copy is a DuckDB-specific mechanism. Use `copy = TRUE`
  against a SQL Server connection instead.
- `duckdt_csv()`/`duckdt_parquet()` — these are built on DuckDB's
  `read_csv_auto`/`read_parquet` table functions and stay DuckDB-only.

`:=` works against SQL Server too, but since T-SQL has neither
`CREATE OR REPLACE TABLE` nor `* EXCLUDE(...)`, it's emulated with an
explicit `SELECT ... INTO` rebuild + `sp_rename`, wrapped in a transaction.
This is not a true atomic replace (e.g. permissions/triggers on the
original table aren't preserved) — the same class of caveat the DuckDB
drop+recreate path already carries.

`duckdt_merge()` works against SQL Server too — since T-SQL has a native
`MERGE` statement, that path is actually simpler than DuckDB's: the staged
subset and a single `MERGE INTO ... USING ... ON (...) WHEN MATCHED ...`
statement, rather than DuckDB's separate `UPDATE`/`INSERT`/`DELETE`.

## Exploring a database

Four functions introspect a connection (works against both DuckDB and MS SQL
Server, via the portable ANSI `information_schema` views) to answer "what
tables are in this database, what do they look like, and how do they
connect" — all accept either a raw `DBI` connection or a `"duckdt"` object.

```r
duckdt_tables(con)         # every table/view: schema, name, type
duckdt_schema(con)         # every column: schema, table, column, type, primary_key
duckdt_schema(con, table = "orders")  # ...or just one table's columns
duckdt_relationships(con)  # declared foreign keys: fk_schema/table/column -> pk_schema/table/column
```

Each returns a plain `data.table`, so they compose with the rest of the
package/data.table normally, e.g. `duckdt_schema(con)[primary_key == TRUE]`
or `duckdt_relationships(con)[pk_table == "orders"]` to see what references a
given table. Foreign keys are only reported when the database actually
declares them as constraints — none of this guesses relationships from
column-naming conventions, and `duckdt_relationships()` returns a zero-row
`data.table` (rather than erroring) if the connected database/version
doesn't expose the constraint views.

`duckdt_erd()` builds on these three to render a Mermaid ER diagram and open
it as a self-contained HTML page in your browser:

```r
duckdt_erd(con)                            # opens a Mermaid ER diagram in the browser
duckdt_erd(con, include_row_counts = TRUE) # add a COUNT(*) per table (can be slow)
```

The returned path also carries the raw Mermaid diagram source as its
`"mermaid"` attribute, so it can be dropped straight into an Rmd/Quarto
```` ```mermaid ```` code chunk instead.

## Not yet supported

- Lazy/chained query building — every `[` runs immediately (by design, see below).
- Row-position indexing in `i` (e.g. `d[1:5]`) — DuckDB tables are unordered, so this
  isn't meaningful without an explicit sort; filter on a column instead.
- Grouped `:=` (`by=` together with a write).
- `duckdt_sample(x, n, replace = TRUE)` — DuckDB's sampling clause doesn't support
  sampling with replacement; only `replace = FALSE` (the default, and only) behavior
  is available.

## Design notes

v1 is deliberately **eager**: every `[` issues one query and returns a `data.table`.
This keeps the mental model identical to plain data.table. A lazy mode (building up a
query across multiple `[` calls before touching R, à la `dtplyr::lazy_dt()`) is a
natural next step if it turns out to matter for your workloads.
