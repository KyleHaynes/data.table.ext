# data.table.ext

`data.table.ext` bundles two families of `data.table` extensions in one package:

1. **Display, sampling & readability utilities** -- cleaner printing, presentation-friendly grouped sampling, friendlier `str()`/`dput()` output, row highlighting, duplicate/outlier detection, markdown export, and more.
2. **`duckdt`: DuckDB querying** -- a `data.table`-style `[i, j, by]` interface backed by [DuckDB](https://duckdb.org/), translating expressions to SQL and executing them inside DuckDB. Also reverse-engineers a database into a data model and draws it as an interactive entity-relationship diagram.

---

## Display, Sampling & Readability

`data.table.ext` is a focused utility package for making `data.table` exploration easier to read, easier to scan, and easier to demo.

It does this in three big ways:

1. It upgrades print ergonomics for large tables.
2. It makes grouped sampling much more presentation-friendly.
3. It smooths `str()` and `dput()` output for `data.table` objects.
4. It evaluates function calls in `j = ` using `e()`.
5. It can color full tables with `cdt()`.
6. It highlights rows of interest with `highlight_dt()`.
7. It surfaces duplicate rows with `dupe_dt()` and outliers with `outlier_dt()`.
8. It finds candidate keys with `key_dt()` and shows column distributions with `spark_dt()`.
9. It exports tables as markdown with `md_dt()`, and to the clipboard with `copy_dt()`.

## Why this package exists

Raw `data.table` output is already very fast and practical, but when you are:

- demoing results to teammates,
- reviewing sampled cohorts,
- debugging mixed-type tables,
- or copying console output into docs,

small readability improvements can save time and simplify your workflow.

`data.table.ext` is opinionated about that readability layer.

## Benefits in practice

### 1) Improved print readability

`enable_dt_print_thousands()` adds row-index thousands separators, keeps alignment stable, and can display a compact `ncol:` header. On grouped samples, it can insert visual group separators and optional value coloring to surface patterns quickly.

This means large outputs are easier to parse at a glance, especially during exploratory work.

### 2) Better grouped sampling for demos and triage

`sample_dt()` supports two modes:

- simple row sampling (`group = NULL`),
- sampled-group expansion (`group = ...`) that returns all rows for sampled groups.

For grouped output, the table is sorted by group and tagged so print output can draw clear separators. This is useful for QA sessions, stakeholder walk-throughs, and quick anomaly checks.
When `sort_coverage = TRUE` (default), rows are additionally sorted by coverage score within each selected group.

### 3) Cleaner object introspection and reproducibility output

- `enable_dt_str_mask()` makes `str()` output for `data.table` more compact and readable.
- `enable_dt_dput_mask()` removes `.internal.selfref` noise from `dput()` output.

You get more signal and less structural clutter.

### 4) One-call setup

When you attach the package with `library(data.table.ext)`, it automatically runs `turn_everyone_on()`.

If you want to run it explicitly again in the same session:

```r
library(data.table.ext)
turn_everyone_on()
```

That call enables print masking, `str()` masking, `dput()` masking, and default coloured grouped sampling.

You also get startup hints with iris examples (formatted via `cli` when available).

## Installation

From a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

Or with `devtools`:

```r
devtools::install_github("KyleHaynes/data.table.ext")
```

## Quick usage

```r
library(data.table)
library(data.table.ext)

# Already auto-enabled on attach, but safe to call again
turn_everyone_on()

DT <- as.data.table(iris)
sample_dt(DT, n = 2, group = Species)
cdt(DT)
str(DT)
dput(DT[1:2])
DT[, e(grep("Sepal", names(DT), value = TRUE))]
```

## Iris benefit walkthrough

```r
library(data.table)
library(data.table.ext)

# Auto-runs turn_everyone_on() on attach
DT <- as.data.table(iris)

# 1) Grouped sampling with grouped separators and clearer scanning
sample_dt(DT, n = 2, group = Species)

# 2) Friendlier structure summary for data.table
str(DT)

# 3) Cleaner dput() output (without .internal.selfref noise)
dput(DT[1:2])
```

## More examples

### Example 0: All functions demo with defaults

```r
library(data.table)
library(data.table.ext)

# All features auto-enabled on attach, but let's demonstrate each function
DT <- as.data.table(iris)

# 1) sample_dt() - grouped sampling with coverage sorting
#    Returns all rows from 2 sampled groups, sorted by group then coverage
sample_dt(DT, n = 2, group = Species)

# 2) Duplicate column highlighting in print output
data.table(x = 1, x = 1, y = 2)

# 3) Coloured type sub-headers by default
DT

# 4) Row-index thousands separator on large tables
data.table(row_id = 1:10000, value = rnorm(10000))

# 5) set_null() - remove columns by reference
DT_copy <- copy(DT)
set_null(DT_copy, "Sepal.Width")
names(DT_copy)

# 6) switch_col() - toggle colour defaults for sampling
switch_col(FALSE)
sample_dt(DT, n = 4)   # No colour by default now

switch_col(TRUE)
sample_dt(DT, n = 4)   # Colour re-enabled

# 7) str() masking - compact data.table introspection
str(DT)

# 8) dput() masking - cleaner reproducible output (no .internal.selfref)
dput(DT[1:2])

# 9) turn_everyone_on() - explicitly re-enable all masks for session
turn_everyone_on()

# 10) Selective disable paths
disable_dt_dput_mask()
disable_dt_str_mask()
disable_dt_print_thousands()

# Now using plain data.table output (masks disabled)
DT
str(DT)
dput(DT[1:2])

# Re-enable for remaining examples
turn_everyone_on()
```

### Example 1: Configure print behavior for large tables

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)[rep(1:.N, 20)]

enable_dt_print_thousands(
    big.mark = ",",
    color = TRUE,
    show_ncol = TRUE,
    color_group_values = TRUE,
    group_value_mode = "distinct"
)

DT
```

### Example 2: Disable coloring but keep row-index commas

```r
enable_dt_print_thousands(
    color = FALSE,
    show_ncol = TRUE
)

as.data.table(iris)[rep(1:.N, 12)]
```

### Example 3: Grouped sampling with unquoted and quoted columns

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# Existing column directly
sample_dt(DT, n = 2, group = "Species")
```

### Example 4: Control color defaults for sampling output

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

switch_col(FALSE)
sample_dt(DT, n = 8)   # default plain output

switch_col(TRUE)
sample_dt(DT, n = 8)   # default coloured output
```

### Example 5: Commonality-based ordering within groups

```r
library(data.table)
library(data.table.ext)

# Create synthetic cohort data with repeated values, intentionally shuffled
cohort <- data.table(
  given_name = c("Cyle", "Kyle", "Sarah", "Kyle", "Saira", "Kyle", "Kylie", "Sara", "Sarah",
                 "Haynes", "Haines", "Haynes", "Hines", "Hines"),
  last_name = c("Hines", "Haynes", "Smith", "Haynes", "Smyth", "Haynes", "Hines", "Smythe", "Smith",
                "123 Main St", "124 Oak Ave", "321 Park Way", "456 Elm Rd", "789 Pine Ln"),
  dob = as.Date(c("1988-01-10", "1990-03-15", "1991-05-18", "1990-03-15", "1994-02-28", "1990-03-15", 
                  "1992-11-03", "1993-09-12", "1991-05-18",
                  "1990-03-15", "1985-07-22", "1991-05-18", "1992-11-03", "1988-01-10")),
  address = c("789 Pine Ln", "123 Main St", "321 Park Way", "123 Main St", "987 Maple St", "124 Oak Ave", 
              "456 Elm Rd", "654 Birch Dr", "321 Park Way",
              "123 Main St", "124 Oak Ave", "321 Park Way", "456 Elm Rd", "789 Pine Ln"),
  cohort_id = c(1, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 1, 1, 1)
)

# sort_coverage=FALSE: original order (scattered, less organized)
sample_dt(cohort, n = 2, group = cohort_id, sort_coverage = FALSE)

# sort_coverage=TRUE (default): rows with more frequent values appear first
# "Kyle" appears 3x, "Haynes" appears 2x, etc. - those rows rank higher and appear at top
sample_dt(cohort, n = 2, group = cohort_id, sort_coverage = TRUE)
```

### Example 6: Cleaner structure and reproducibility output

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

enable_dt_str_mask()
str(DT)

enable_dt_dput_mask()
dput(DT[1:2])
```

### Example 7: Remove columns by reference with set_null()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)
DT[, temp_flag := TRUE]
DT[, temp_score := 1L]

set_null(DT, c("temp_flag", "temp_score"))
names(DT)
```

### Example 8: Turn everything on, then selectively disable

```r
library(data.table)
library(data.table.ext)

turn_everyone_on()

# Work with enhanced output
DT <- as.data.table(iris)
sample_dt(DT, n = 2, group = Species)

# Selective teardown
disable_dt_dput_mask()
disable_dt_str_mask()
disable_dt_print_thousands()
```

### Example 9: Custom class token color mapping

```r
library(data.table)
library(data.table.ext)

enable_dt_print_thousands(
    class_colors = c(
        "<num>" = "col_cyan",
        "<char>" = "col_yellow",
        "<fctr>" = "col_magenta"
    )
)

as.data.table(iris)
```

### Example 10: Similarity-based grouped value coloring

```r
library(data.table)
library(data.table.ext)

DT <- data.table::data.table(
    id = 1:8,
    team = c("North", "North", "South", "South", "East", "East", "West", "West"),
    label = c("alpha", "alpah", "beta", "betta", "gamma", "gama", "delta", "deltta")
)

enable_dt_print_thousands(
    color_group_values = TRUE,
    group_value_mode = "similarity",
    similarity_max_distance = 2,
    similarity_max_relative = 0.34
)

sample_dt(DT, n = 2, group = team)
```

### Example 11: Color the full table with cdt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# No group is needed. The whole table is colored as one display unit.
cdt(DT)

# Large tables stop using color once they cross the threshold.
cdt(as.data.table(iris)[rep(1:.N, 5)])
```

### Example 12: Color grouped output with cdt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# Grouped display keeps the same print treatment as sample_dt(), without sampling.
cdt(DT, group = Species)
```

### Example 13: Select columns with regex using e()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# Select columns matching a pattern and return the subset
DT[, e(grep("Sepal", names(DT), value = TRUE))]

# Select columns that don't match a pattern
DT[, e(grep("Sepal", names(DT), value = TRUE, invert = TRUE))]

# Select columns by index
DT[, e(1:2)]

# Select columns by logical index
DT[, e(c(TRUE, TRUE, FALSE, FALSE, FALSE))]
```

### Example 14: Highlight rows matching a condition

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# Highlight rows matching a condition in red (default)
highlight_dt(DT, Sepal.Length > 7)

# Use a custom cli color
highlight_dt(DT, Species == "setosa", color = "col_cyan")
```

### Example 15: Surface duplicate rows with dupe_dt()

```r
library(data.table)
library(data.table.ext)

DT <- data.table(
    id = c(1, 1, 2, 3, 3, 3),
    name = c("a", "a", "b", "c", "c", "c")
)

# Rows that participate in a duplicate cluster, grouped for print
dupe_dt(DT)

# Duplicates based on a subset of columns
dupe_dt(DT, by = "id")
```

### Example 16: Flag outlier rows with outlier_dt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# IQR-based flagging across all numeric columns (default)
outlier_dt(DT)

# Z-score based flagging on a specific column
outlier_dt(DT, cols = "Sepal.Width", method = "zscore", threshold = 2.5)
```

### Example 17: Find candidate keys with key_dt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)
DT[, id := .I]

# Reports minimal column combinations that uniquely identify every row
key_dt(DT)
```

### Example 18: Scan a column's distribution with spark_dt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

spark_dt(DT, Sepal.Length)
```

### Example 19: Export a table as markdown or to the clipboard

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)[1:5]

# Print a markdown pipe table (handy for docs, PRs, issues)
md_dt(DT)

# Copy the same markdown table to the system clipboard
copy_dt(DT)
```

### Example 20: Quick EDA helpers

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# One row per column: class, distinct count, NA count
schema_dt(DT)

# NA count/percentage per column
na_dt(DT)

# Top values in a column, sorted by frequency
freq_dt(DT, Species)

# Rename columns by reference
rename_dt(DT, c(sepal_length = "Sepal.Length"))
```

## Exported functions

- `enable_dt_print_thousands()`
- `disable_dt_print_thousands()`
- `enable_dt_str_mask()`
- `disable_dt_str_mask()`
- `enable_dt_dput_mask()`
- `disable_dt_dput_mask()`
- `e()`
- `cdt()`
- `sample_dt()`
- `set_null()`
- `switch_col()`
- `turn_everyone_on()`
- `na_dt()`
- `freq_dt()`
- `schema_dt()`
- `rename_dt()`
- `highlight_dt()`
- `dupe_dt()`
- `outlier_dt()`
- `key_dt()`
- `spark_dt()`
- `md_dt()`
- `copy_dt()`

---

## `duckdt`: Querying DuckDB with data.table Syntax

Query [DuckDB](https://duckdb.org/) using [data.table](https://r-datatable.com/)'s
`d[i, j, by]` syntax. Expressions are translated to SQL and run inside DuckDB — data
only comes back to R as a `data.table` once you materialize a result.

It also draws the database: `duckdt_erd(con)` opens an ER diagram of every
table and how they connect, and builds the query for the tables and columns
you tick. See [Exploring a database](#exploring-a-database).

Saving a `data.table` to a persistent DuckDB file and reconnecting to it
later? See [WORKFLOW.md](WORKFLOW.md) for a step-by-step walkthrough.

```r
library(data.table.ext)

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

### List-columns

Against DuckDB connections (not MS SQL Server), `as.duckdt()` round-trips R
list-columns to/from DuckDB's native `LIST` type, both zero-copy and with
`copy = TRUE`:

```r
x <- data.table::data.table(id = 1:2, tags = list(c("a", "b"), "c"))
d <- as.duckdt(x, copy = TRUE)
as.data.table(d)$tags   # list(c("a", "b"), "c") -- reconstructed automatically
```

As with DuckDB's `LIST` type itself, every element within one list-column
must coerce to a single atomic type -- a column mixing an integer-vector
cell with a character-vector cell isn't representable as one `LIST` column.

### Connecting to a DuckDB file on disk

By default (e.g. `as.duckdt(mtcars)` with no `conn`) `duckdt` opens an
in-memory DuckDB database that disappears when the connection closes. To
persist to (or read from) a database file, open the connection yourself with
`duckdb::duckdb(dbdir = ...)` and pass it in:

```r
con <- duckdt_connect("C:/temp/gnafx.duckdb")
#> v Connected to DuckDB: C:/temp/gnafx.duckdb
#> i 2 tables: "addresses", "gnaf"
#> > `duckdt_erd(con)` to explore the tables and how they connect
#> > `duckdt(con, "addresses")` to query one with data.table syntax
```

`duckdt_connect()` is a thin wrapper around
`DBI::dbConnect(duckdb::duckdb(dbdir = ...))` that summarises what you just
opened; pass `read_only = TRUE` for a database you only mean to look at, and
`quiet = TRUE` to skip the summary. Plain `DBI::dbConnect()` works everywhere
in duckdt too.

Forward slashes (`"C:/temp/gnafx.duckdb"`) work fine on Windows and avoid
having to escape backslashes.

List what's in the file with [`duckdt_tables()`](#exploring-a-database), then
wrap the one you want with `duckdt()`:

```r
duckdt_tables(con)
#>    schema       name       type
#> 1:  main    addresses BASE TABLE
#> 2:  main         gnaf BASE TABLE

d <- duckdt(con, "gnaf_addresses")   # wraps the existing table, no copy
d[, .N, by = state]
```

When you're done, close the connection:

```r
duckdt_disconnect(con)   # or DBI::dbDisconnect(con, shutdown = TRUE)
```

## Writing data

`:=` mutates a **materialized, writable** table in place (create one with
`copy = TRUE`):

```r
d <- as.duckdt(mtcars, copy = TRUE)
d[, kw := hp * 0.7457]                # add a computed column
d[cyl == 6, kw := kw * 1.1]           # update matching rows only
```

Handles from `as.duckdt()` are writable immediately, since you just created
that table. But `duckdt(conn, table)` -- wrapping a table that already
existed, e.g. after reconnecting to a file (see
[Connecting to a DuckDB file on disk](#connecting-to-a-duckdb-file-on-disk))
-- defaults to **read-only** as a guard against accidental writes: `:=` and
`duckdt_merge()` both refuse to run until you opt in with `writable = TRUE`:

```r
d <- duckdt(con, "gnaf")                       # read-only by default
d[, kw := hp * 0.7457]                         # errors: requires a writable handle

d <- duckdt(con, "gnaf", writable = TRUE)      # explicit opt-in
d[, kw := hp * 0.7457]                         # now allowed
```

`print()` flags this: a materialized-but-read-only handle prints
`<duckdt> gnaf [... rows] (read-only)`.

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

Note that `duckdt_merge()` **writes**: it reconciles `y` into `x`'s table in
place. To combine two tables into a new result without touching either, see
[Joining tables](#joining-tables).

## Joining tables

`duckdt_join()` is the read-path counterpart to `duckdt_merge()`: it joins two
tables in SQL and returns the result, leaving both inputs untouched, the way
base `merge()` does. Either side may be a `"duckdt"` handle or an ordinary
`data.frame`/`data.table` — R-side inputs are staged into a temporary table on
the connection first, so the join itself always runs in the engine:

```r
d <- duckdt(con, "gnaf_addresses")
f <- duckdt(con, "gnaf_locality_index")

duckdt_join(d, f, by = "locality_name")                 # two database tables
duckdt_join(d, my_data_table, by = "locality_name")     # database table + R table
duckdt_join(my_data_table, f, by = "locality_name")     # ...either way round

merge(d, f, by = "locality_name")                       # same thing, base spelling
```

`by`/`by.x`/`by.y`, `all`/`all.x`/`all.y`, `suffixes` and `sort` all behave as
in base `merge()`: no `all*` is an `INNER JOIN`, `all.x` a `LEFT JOIN`, `all.y`
a `RIGHT JOIN`, `all` a `FULL OUTER JOIN`. At least one side must be a
`"duckdt"` object — that's what says which database to run in — and if both
are, they must share a connection.

### Keeping a subset in the database

`d[i, j, by]` returns a `data.table`, so the rows have *left* the database and
can't be used where a handle is expected:

```r
duckdt_join(d[locality_name == "WORONGARY"], f, by = "locality_name")
# `d[...]` is already a data.table here — it works, but the subset made a
# round trip through R first.
```

`duckdt_temp()` runs the same `i`/`j`/`by` query into a temporary table and
hands back a `"duckdt"` handle, so nothing crosses into R:

```r
sub <- duckdt_temp(d, locality_name == "WORONGARY")
sub
#> <duckdt> duckdt_temp_gnaf_addresses_1 [2 x 3] (temp)

duckdt_join(sub, f, by = "locality_name")   # join runs entirely in DuckDB
duckdt_drop(sub)                            # done with it
```

The handle is materialized and writable, so `:=` and `duckdt_merge()` work
against it too — writes land on the temporary copy and leave the source table
alone. On DuckDB the table lives in the session's `temp` schema and disappears
when the connection closes; on MS SQL Server it's created with
`SELECT ... INTO` as an ordinary table, so call `duckdt_drop()` when you're
finished. As a guard, `duckdt_drop()` refuses handles `duckdt_temp()` didn't
create unless you pass `force = TRUE`.

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

`duckdt_join()` works against SQL Server too — `INNER`/`LEFT`/`RIGHT`/`FULL
OUTER JOIN` are standard SQL. `duckdt_temp()` does differ: T-SQL has no
`CREATE TEMP TABLE ... AS`, so it uses `SELECT ... INTO`, which creates an
ordinary table on the current schema rather than a session-scoped one — drop
it with `duckdt_drop()` when you're done.

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

### Visualising the schema

`duckdt_erd()` builds on those three to write a self-contained HTML page and
open it in your browser — an ER diagram plus a searchable list of every table
and column:

```r
duckdt_erd(con)                            # opens the explorer in your browser
duckdt_erd(con, include_row_counts = TRUE) # add a COUNT(*) per table (can be slow)
duckdt_erd(con, tables = c("orders", "customers"), view = "keys_only")
```

The page is where the "which columns do I actually want" work happens: tick
tables and columns in the sidebar and it redraws the diagram and writes the
code that selects exactly those — `duckdt(con, "orders")[, .(id, total)]` for
a single table, or a `SELECT` with the joins worked out from the schema's
foreign keys for several — with a copy button. It needs no R packages beyond
duckdt; the diagram itself is drawn by Mermaid from a CDN, so with no internet
you get its source instead and everything else still works.

The returned path carries the Mermaid source as its `"mermaid"` attribute (drop
it straight into an Rmd/Quarto ```` ```mermaid ```` chunk) and the data model
as `"data_model"`.

`duckdt_explorer(con)` is the Shiny version of the same thing, and — since it
has a live connection — also previews the rows the query returns. It needs
`shiny`, and uses `DiagrammeR` and `DT` if they're installed.

### The data model underneath

Both of those draw a **data model**: a description of tables, columns, keys and
references, ported from [datamodelr](https://github.com/bergant/datamodelr).
You can build one yourself, edit it, and render it:

```r
dm <- duckdt_data_model(con)     # or a "duckdt" handle, or a named list of data.frames
dm
#> <duckdt data model> 4 tables, 12 columns, 3 references
#>   customers                      3 cols, PK: customer_id
#>   orders                         3 cols, PK: order_id, 1 FK
#>   ...

dm$tables       # table, schema, name, type, n_rows, segment, display
dm$columns      # table, column, type, key, ref, ref_col
dm$references   # table, column -> ref, ref_col
```

Most DuckDB databases — anything built by loading CSV or Parquet files —
declare no foreign keys at all, which leaves the diagram as a set of
disconnected boxes. Two ways to fix that:

```r
# 1. Guess from column naming conventions (customer_id -> customers.customer_id).
#    Conservative: skips ambiguous names, bare `id`, and type mismatches.
dm <- duckdt_dm_infer_references(dm)
duckdt_erd(con, infer_references = TRUE)   # or straight from the connection

# 2. State them exactly.
dm <- duckdt_dm_set_key(dm, "customers", "customer_id")
dm <- duckdt_dm_add_references(dm, orders$customer_id == customers$customer_id)
```

Then zoom in, group and colour tables, and draw it:

```r
duckdt_dm_filter(dm, "orders", depth = 1)          # orders + whatever it touches
duckdt_dm_set_segment(dm, list(sales = c("orders", "order_lines")))
duckdt_dm_set_display(dm, list(accent1 = "customers", hide = "audit_log"))

duckdt_erd(dm)                                      # the interactive page
duckdt_dm_mermaid(dm, view = "keys_only")           # Mermaid source
duckdt_dm_dot(dm, rankdir = "LR")                   # Graphviz DOT (prints via DiagrammeR)
duckdt_dm_render(dm)                                # htmlwidget, for Rmd/Shiny
duckdt_dm_export(dm, "schema.png")                  # image file
```

And build the query for a set of tables without the browser at all —
`duckdt_dm_query()` works out the joins from the model's references:

```r
duckdt_dm_query(dm, c("orders", "customers"),
                columns = list(orders = "total", customers = "name"),
                where = "total > 100")
```

Tables it can't reach by any reference are listed in the result's `"unjoined"`
attribute rather than silently cross-joined.

## Not yet supported

- Lazy/chained query building — every `[` runs immediately (by design, see below).
  `duckdt_temp()` is the escape hatch: it runs an `[i, j, by]` query into a temporary
  table and returns a handle, so a subset can stay in the database and be chained into
  another query.
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
natural next step if it turns out to matter for your workloads. In the meantime
`duckdt_temp()` covers the case that actually bites — a subset you want to feed to
another database-side operation — by materializing it into a temporary table rather
than into R.

## Slides

`inst/slides/duckdt-intro.qmd` is a Quarto **revealjs** deck introducing the
package to a team that doesn't already know it: what it is, the connect →
look → wrap → query → get-out steps, Mermaid diagrams of how the pieces fit,
and live output run against a small made-up address database
(`inst/slides/demo-db.R`).

```sh
cd inst/slides
quarto render duckdt-intro.qmd     # -> duckdt-intro.html, one self-contained file
```

The rendered deck is a single HTML file with everything inlined, so it works
offline and can be emailed as-is. `make-screenshots.R` beside it regenerates
the explorer screenshot the deck uses.

## Credits

The data model and diagram code (`duckdt_data_model()`, the `duckdt_dm_*()`
functions and the Graphviz rendering) is a port of Darko Bergant's
[datamodelr](https://github.com/bergant/datamodelr), MIT licensed, extended
here to reverse-engineer live DuckDB/SQL Server connections, guess undeclared
references, and drive the interactive column picker and query builder.
