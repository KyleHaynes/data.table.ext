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
