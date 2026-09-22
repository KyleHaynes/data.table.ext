# data.table.ext

`data.table.ext` bundles two families of `data.table` extensions in one package:

1. **Display, sampling & readability utilities** -- cleaner printing, presentation-friendly grouped sampling, friendlier `str()`/`dput()` output, row highlighting, duplicate/outlier detection, markdown export, and more.
2. **`dbdt`: database querying** -- a `data.table`-style `[i, j, by]` interface for [DuckDB](https://duckdb.org/) and Microsoft SQL Server. Also reverse-engineers a database into a data model and draws it as an interactive entity-relationship diagram.

The database helpers now use `dbdt()`, `as.dbdt()` and `dbdt_*()` names.
All existing `duckdt` function names remain available as compatible aliases;
the underlying S3 classes and `duckdt.*` options are unchanged.
`dbdt_connect()` still opens DuckDB; for SQL Server, pass your existing
`DBI`/`odbc` connection to `dbdt()` and the other helpers.

Runnable examples for all of it are in the [documentation site](site/); see [Documentation](#documentation).

## Contents

- [Installation](#installation)
- [Getting started](#getting-started)
- [Function index](#function-index)
- [Documentation](#documentation)
- [Display, Sampling & Readability](#display-sampling--readability)
- [`dbdt`: Querying databases with data.table syntax](#dbdt-querying-databases-with-datatable-syntax)
- [Options](#options)
- [Known limitations](#known-limitations)
- [Credits](#credits)

## Installation

From GitHub:

```r
devtools::install_github("KyleHaynes/data.table.ext")
```

Or from a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

`data.table`, `cli`, `DBI` and `duckdb` are installed with it. A few features need an optional package:

| Package | Needed for |
|:---|:---|
| `DiagrammeR` | `dbdt_dm_render()`, and drawing the result of `dbdt_dm_dot()` |
| `DiagrammeRsvg`, `rsvg` | `dbdt_dm_export()` to image files (`rsvg` is needed for anything but SVG) |
| `shiny` | `dbdt_explorer()` |
| `DT` | the row preview in `dbdt_explorer()` |
| `odbc` (or another DBI driver) | [MS SQL Server](#ms-sql-server-support) |

`dbdt_erd()` needs none of them.

## Getting started

```r
library(data.table)
library(data.table.ext)
```

Attaching the package prints the [function index](#function-index) below as a themed tree, and runs `turn_everyone_on()`, which switches on four display enhancements: the `data.table` print mask (thousands separators, a coloured class row, an `ncol:` header, group separators), the `str()` mask, the `dput()` mask, and colour by default in `sample_dt()`, `cdt()` and `dupe_dt()`. Undo them with `disable_dt_print_thousands()`, `disable_dt_str_mask()`, `disable_dt_dput_mask()` and `switch_col(FALSE)`; put them all back with `turn_everyone_on()`.

The banner is a single startup message. `options(duckdt.quiet = TRUE)` before `library()` hides it (the enhancements still switch on), and `suppressPackageStartupMessages(library(data.table.ext))` works too.

The masks are defined in the global environment, so they apply to code you run at the console or in a script, not to code inside other packages. Anything you have defined there as `print.data.table`, `str` or `dput` is replaced.

A first look at each half:

```r
DT <- as.data.table(iris)
sample_dt(DT, n = 2, group = Species)    # display: colour-coded grouped sampling

con <- dbdt_example()                  # dbdt: a small database to try things on
d <- dbdt(con, "orders")
d[, .N, by = customer_id]                # data.table syntax, run as SQL inside DuckDB
dbdt_erd(con)                          # draw the database in your browser
```

## Function index

Every exported function, grouped by theme as in the startup banner.

**Display, sampling & readability**

| Theme | Functions | For |
|:---|:---|:---|
| Session | `turn_everyone_on()`, `enable_dt_print_thousands()`, `disable_dt_print_thousands()`, `enable_dt_str_mask()`, `disable_dt_str_mask()`, `enable_dt_dput_mask()`, `disable_dt_dput_mask()`, `switch_col()` | Switching the display enhancements on and off |
| Sample | `sample_dt()`, `cdt()` | Group-aware sampling, and colour-coded display of a whole table |
| Spot | `highlight_dt()`, `dupe_dt()`, `outlier_dt()` | Surfacing rows of interest |
| Profile | `schema_dt()`, `na_dt()`, `freq_dt()`, `spark_dt()`, `key_dt()` | A first look at a table |
| Columns | `e()`, `rename_dt()`, `set_null()` | Selecting, renaming and dropping columns |
| Share | `md_dt()`, `copy_dt()` | Markdown export, to the console or the clipboard |

**`dbdt`: `data.table` syntax on DuckDB and SQL Server**

| Theme | Functions | For |
|:---|:---|:---|
| Connect | `dbdt_connect()`, `dbdt_disconnect()`, `dbdt_example()` | Opening and closing a database; an example one |
| Load | `as.dbdt()`, `dbdt()`, `dbdt_csv()`, `dbdt_parquet()` | Getting data in: from R, from files, from an existing table |
| Query | `d[i, j, by]`, `head()`, `tail()`, `dbdt_sample()` | Querying, peeking and sampling |
| Join & write | `dbdt_join()` (`merge()`), `dbdt_merge()`, `dbdt_temp()`, `dbdt_drop()`, `:=` | Joining, patching and changing tables inside the database |
| Explore | `dbdt_tables()`, `dbdt_schema()`, `dbdt_relationships()`, `dbdt_erd()`, `dbdt_explorer()` | Finding your way around a database |
| Model | `dbdt_data_model()`, `is_dbdt_data_model()`, `dbdt_dm_set_key()`, `dbdt_dm_add_references()`, `dbdt_dm_add_reference()`, `dbdt_dm_infer_references()`, `dbdt_dm_set_segment()`, `dbdt_dm_set_display()`, `dbdt_dm_filter()`, `dbdt_dm_query()` | Building, editing and querying a data model |
| Draw | `dbdt_dm_mermaid()`, `dbdt_dm_dot()`, `dbdt_dm_render()`, `dbdt_dm_export()`, `dbdt_dm_palette()`, `dbdt_dm_color_scheme()`, `dbdt_dm_add_colors()`, `dbdt_dm_get_color_scheme()`, `dbdt_dm_set_color_scheme()` | Drawing a model, and its colours |

`head()`, `tail()`, `merge()`, `dim()`, `names()` and `as.data.table()` work on a `"duckdt"` handle as S3 methods. The [documentation site](site/) has a line on every function.

## Documentation

- **[Documentation site](site/)**: a [Quarto](https://quarto.org/) website with runnable examples. Getting started, printing and sampling, data checks and helpers, the `dbdt` guides (connect and load, query, join/write/merge, explore a database, SQL Server), a function reference, and a limitations page. Build it from the repository root:

  ```sh
  quarto render site      # writes site/_site
  quarto preview site     # live preview while you edit
  ```

  Rendering runs every example, so the package must be installed first (`install.packages(".", repos = NULL, type = "source")`), along with `knitr` and, for the diagram examples, `DiagrammeR`. `quarto publish gh-pages site` publishes it to GitHub Pages.
- **[WORKFLOW.md](WORKFLOW.md)**: saving a `data.table` to a persistent DuckDB file and reconnecting later.
- **Slides**: `inst/slides/duckdt-intro.qmd`, a Quarto deck introducing `duckdt` (see [Slides](#slides)).
- **Help pages**: `?dbdt_join`, or `help(package = "data.table.ext")` for all of them.

---

## Display, Sampling & Readability

These utilities make `data.table` exploration easier to read, easier to scan, and easier to demo. In brief:

1. It upgrades print ergonomics for large tables.
2. It makes grouped sampling much more presentation-friendly.
3. It smooths `str()` and `dput()` output for `data.table` objects.
4. It evaluates function calls in `j = ` using `e()`.
5. It can color full tables with `cdt()`.
6. It highlights rows of interest with `highlight_dt()`.
7. It surfaces duplicate rows with `dupe_dt()` and outliers with `outlier_dt()`.
8. It finds candidate keys with `key_dt()` and shows column distributions with `spark_dt()`.
9. It exports tables as markdown with `md_dt()`, and to the clipboard with `copy_dt()`.

### Why this package exists

Raw `data.table` output is already very fast and practical, but when you are:

- demoing results to teammates,
- reviewing sampled cohorts,
- debugging mixed-type tables,
- or copying console output into docs,

small readability improvements can save time and simplify your workflow.

`data.table.ext` is opinionated about that readability layer.

### Benefits in practice

#### 1) Improved print readability

`enable_dt_print_thousands()` adds row-index thousands separators, keeps alignment stable, and can display a compact `ncol:` header. On grouped samples, it can insert visual group separators and optional value coloring to surface patterns quickly.

This means large outputs are easier to parse at a glance, especially during exploratory work.

#### 2) Better grouped sampling for demos and triage

`sample_dt()` supports two modes:

- simple row sampling (`group = NULL`),
- sampled-group expansion (`group = ...`) that returns all rows for sampled groups.

For grouped output, the table is sorted by group and tagged so print output can draw clear separators. This is useful for QA sessions, stakeholder walk-throughs, and quick anomaly checks.
When `sort_coverage = TRUE` (default), rows are additionally sorted by coverage score within each selected group.

#### 3) Cleaner object introspection and reproducibility output

- `enable_dt_str_mask()` makes `str()` output for `data.table` more compact and readable.
- `enable_dt_dput_mask()` removes `.internal.selfref` noise from `dput()` output.

You get more signal and less structural clutter.

#### 4) One-call setup

When you attach the package with `library(data.table.ext)`, it automatically runs `turn_everyone_on()`.

If you want to run it explicitly again in the same session:

```r
library(data.table.ext)
turn_everyone_on()
```

That call enables print masking, `str()` masking, `dput()` masking, and default coloured grouped sampling.

You also get a startup banner that lists the package's functions by theme (see [Getting started](#getting-started)). `options(duckdt.quiet = TRUE)` before `library()` hides it.

### Quick usage

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

### Iris benefit walkthrough

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

### More examples

#### Example 0: All functions demo with defaults

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

#### Example 1: Configure print behavior for large tables

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

#### Example 2: Disable coloring but keep row-index commas

```r
enable_dt_print_thousands(
    color = FALSE,
    show_ncol = TRUE
)

as.data.table(iris)[rep(1:.N, 12)]
```

#### Example 3: Grouped sampling with unquoted and quoted columns

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# Existing column directly
sample_dt(DT, n = 2, group = "Species")
```

#### Example 4: Control color defaults for sampling output

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

switch_col(FALSE)
sample_dt(DT, n = 8)   # default plain output

switch_col(TRUE)
sample_dt(DT, n = 8)   # default coloured output
```

#### Example 5: Commonality-based ordering within groups

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

#### Example 6: Cleaner structure and reproducibility output

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

enable_dt_str_mask()
str(DT)

enable_dt_dput_mask()
dput(DT[1:2])
```

#### Example 7: Remove columns by reference with set_null()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)
DT[, temp_flag := TRUE]
DT[, temp_score := 1L]

set_null(DT, c("temp_flag", "temp_score"))
names(DT)
```

#### Example 8: Turn everything on, then selectively disable

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

#### Example 9: Custom class token color mapping

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

#### Example 10: Grouped value colouring modes

In grouped output, values are coloured within each group. The default mode, `"distinct"`, gives every distinct value its own colour. The other, `"similarity"`, gives near-identical strings one shared colour (an edit distance within `similarity_max_distance`, or within `similarity_max_relative` of the longer string's length), which makes typos easy to spot.

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

> **Caveat:** a mode stored on a table wins over the session setting, and `sample_dt()`, `cdt()` and `dupe_dt()` store `"distinct"` on the tables they return. So the `"similarity"` setting above does not currently change how their output prints: `alpha` and `alpah` still get different colours. See [Known limitations](#known-limitations).

#### Example 11: Color the full table with cdt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# No group is needed. The whole table is colored as one display unit.
cdt(DT)

# Large tables stop using color once they cross the threshold.
cdt(as.data.table(iris)[rep(1:.N, 5)])
```

#### Example 12: Color grouped output with cdt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# Grouped display keeps the same print treatment as sample_dt(), without sampling.
cdt(DT, group = Species)
```

#### Example 13: Select columns with regex using e()

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

> **Caveat:** `e()` rebuilds the columns from the whole table, so a filter in `i` is not applied: `dt[x == 1, e(c("x", "y"))]` returns every row. To select columns *and* rows, use `dt[x == 1, .SD, .SDcols = c("x", "y")]`.

#### Example 14: Highlight rows matching a condition

```r
library(data.table)
library(data.table.ext)

# Fifteen rows, five per species. Long tables print only their first and last
# five rows, and only printed rows can show a highlight.
DT <- as.data.table(iris)[c(1:5, 51:55, 101:105)]

# Highlight rows matching a condition in red (default)
highlight_dt(DT, Sepal.Length > 6.5)

# Use a custom cli color
highlight_dt(DT, Species == "setosa", color = "col_cyan")
```

#### Example 15: Surface duplicate rows with dupe_dt()

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

#### Example 16: Flag outlier rows with outlier_dt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

# IQR-based flagging across all numeric columns (default)
outlier_dt(DT)

# Z-score based flagging on a specific column
outlier_dt(DT, cols = "Sepal.Width", method = "zscore", threshold = 2.5)
```

#### Example 17: Find candidate keys with key_dt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)
DT[, id := .I]

# Reports minimal column combinations that uniquely identify every row
key_dt(DT)
```

#### Example 18: Scan a column's distribution with spark_dt()

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)

spark_dt(DT, Sepal.Length)
```

#### Example 19: Export a table as markdown or to the clipboard

```r
library(data.table)
library(data.table.ext)

DT <- as.data.table(iris)[1:5]

# Print a markdown pipe table (handy for docs, PRs, issues)
md_dt(DT)

# Copy the same markdown table to the system clipboard
copy_dt(DT)
```

#### Example 20: Quick EDA helpers

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

---

## `dbdt`: Querying databases with data.table syntax

Query [DuckDB](https://duckdb.org/) using [data.table](https://r-datatable.com/)'s
`d[i, j, by]` syntax. Expressions are translated to SQL and run inside DuckDB — data
only comes back to R as a `data.table` once you materialize a result.

It also draws the database: `dbdt_erd(con)` opens an ER diagram of every
table and how they connect, and builds the query for the tables and columns
you tick. See [Exploring a database](#exploring-a-database).

Saving a `data.table` to a persistent DuckDB file and reconnecting to it
later? See [WORKFLOW.md](WORKFLOW.md) for a step-by-step walkthrough.

New here? `con <- dbdt_example()` builds a small in-memory database (four
tables of a toy shop, with keys declared) to try everything below on, and
`dbdt_erd(con)` draws it.

```r
library(data.table.ext)

d <- as.dbdt(mtcars)      # zero-copy: registers mtcars as a DuckDB view
d[cyl == 6]                          # filter
d[cyl == 6, .(mpg, hp)]              # filter + select
d[, .(avg_mpg = mean(mpg), n = .N), by = cyl]   # group by + aggregate
```

Every `[` call executes immediately in DuckDB and returns a real `data.table`, so you
can keep chaining exactly like you would with data.table itself:

```r
d[cyl == 6][order(-mpg)][1:3]
```

### Getting data in

```r
as.dbdt(mtcars)                     # zero-copy view (read-only)
as.dbdt(mtcars, copy = TRUE)        # physical, writable DuckDB table
dbdt(conn, "existing_table")        # wrap a table/view you already have
dbdt_csv("big_file.csv")            # lazy view over a CSV, out-of-core
dbdt_parquet("data/*.parquet")      # lazy view over Parquet file(s)
```

Row names are not carried into DuckDB (`mtcars` keeps its car names in its
row names, so they are dropped). Put them in a column first:
`as.dbdt(data.table(car = rownames(mtcars), mtcars))`.

`dbdt_csv()` and `dbdt_parquet()` name the view after the file unless you
pass `name =`; DuckDB will not replace an existing table with a view, so give a
name when the file's name matches a table already in the database.

#### List-columns

Against DuckDB connections (not MS SQL Server), `as.dbdt()` round-trips R
list-columns to/from DuckDB's native `LIST` type, both zero-copy and with
`copy = TRUE`:

```r
x <- data.table::data.table(id = 1:2, tags = list(c("a", "b"), "c"))
d <- as.dbdt(x, copy = TRUE)
as.data.table(d)$tags   # list(c("a", "b"), "c") -- reconstructed automatically
```

As with DuckDB's `LIST` type itself, every element within one list-column
must coerce to a single atomic type -- a column mixing an integer-vector
cell with a character-vector cell isn't representable as one `LIST` column.

#### Connecting to a DuckDB file on disk

By default (e.g. `as.dbdt(mtcars)` with no `conn`) `duckdt` opens an
in-memory DuckDB database that disappears when the connection closes. To
persist to (or read from) a database file, open the connection yourself with
`duckdb::duckdb(dbdir = ...)` and pass it in:

```r
con <- dbdt_connect("C:/temp/gnafx.duckdb")
#> v Connected to DuckDB: C:/temp/gnafx.duckdb
#> i 2 tables: "addresses", "gnaf"
#> > `dbdt_erd(con)` to explore the tables and how they connect
#> > `dbdt(con, "addresses")` to query one with data.table syntax
```

`dbdt_connect()` is a thin wrapper around
`DBI::dbConnect(duckdb::duckdb(dbdir = ...))` that summarises what you just
opened; pass `read_only = TRUE` for a database you only mean to look at, and
`quiet = TRUE` to skip the summary. Plain `DBI::dbConnect()` works everywhere
in duckdt too.

Forward slashes (`"C:/temp/gnafx.duckdb"`) work fine on Windows and avoid
having to escape backslashes.

List what's in the file with [`dbdt_tables()`](#exploring-a-database), then
wrap the one you want with `dbdt()`:

```r
dbdt_tables(con)
#>    schema       name       type
#> 1:  main    addresses BASE TABLE
#> 2:  main         gnaf BASE TABLE

d <- dbdt(con, "gnaf_addresses")   # wraps the existing table, no copy
d[, .N, by = state]
```

When you're done, close the connection:

```r
dbdt_disconnect(con)   # or DBI::dbDisconnect(con, shutdown = TRUE)
```

### Writing data

`:=` mutates a **materialized, writable** table in place (create one with
`copy = TRUE`):

```r
d <- as.dbdt(mtcars, copy = TRUE)
d[, kw := hp * 0.7457]                # add a computed column
d[cyl == 6, kw := kw * 1.1]           # update matching rows only
```

Handles from `as.dbdt()` are writable immediately, since you just created
that table. But `dbdt(conn, table)` -- wrapping a table that already
existed, e.g. after reconnecting to a file (see
[Connecting to a DuckDB file on disk](#connecting-to-a-duckdb-file-on-disk))
-- defaults to **read-only** as a guard against accidental writes: `:=` and
`dbdt_merge()` both refuse to run until you opt in with `writable = TRUE`:

```r
d <- dbdt(con, "gnaf")                       # read-only by default
d[, kw := hp * 0.7457]                         # errors: requires a writable handle

d <- dbdt(con, "gnaf", writable = TRUE)      # explicit opt-in
d[, kw := hp * 0.7457]                         # now allowed
```

`print()` flags this: a materialized-but-read-only handle prints
`<duckdt> gnaf [... rows] (read-only)`.

> **Before you use `:=` on a table that matters.** `:=` rebuilds the table
> (`CREATE OR REPLACE TABLE ... AS SELECT`) so DuckDB can infer the new column's
> type. That drops the table's primary key and `NOT NULL` constraints; it cannot
> run on a table that another table references (the old table cannot be dropped
> while something depends on it); and each assigned column is written in its own
> step, so if a later expression fails, the earlier columns stay written. `:=`
> also returns the handle, so the console prints it after each assignment --
> wrap the call in `invisible()` to keep quiet. See
> [Known limitations](#known-limitations).

`dbdt_merge()` merges a subset (`data.frame`/`data.table`, or another `"duckdt"`
table/view/query result) into a **materialized** table -- entirely inside the
database. `y` is staged into a temporary table on `x`'s connection first, so `x`'s
existing data never round-trips through R; matching rows are updated, unmatched `y`
rows are inserted, and (opt-in) unmatched `x` rows can be deleted:

```r
cars <- data.table(car = rownames(mtcars), mtcars)   # a `car` column to match on
d <- as.dbdt(cars, copy = TRUE)
patch <- data.frame(car = c("Mazda RX4", "New Car"), hp = c(999, 111))
dbdt_merge(d, patch, by = "car")                    # update matches, insert new rows
dbdt_merge(d, patch, by = "car", insert = FALSE)     # update only, skip new rows
dbdt_merge(d, patch, by = "car", delete = TRUE)      # + delete rows not in `patch`
```

On DuckDB this compiles to `UPDATE ... FROM` + an anti-join `INSERT` (+ an anti-join
`DELETE` if `delete = TRUE`), wrapped in a transaction. On MS SQL Server it compiles
to a single native T-SQL `MERGE` statement. If `by` is omitted it defaults to every
column shared between `x` and `y` (a natural join, like base `merge()`) -- pass `by=`
explicitly whenever `y` carries value columns that should be updated rather than
matched on.

Note that `dbdt_merge()` **writes**: it reconciles `y` into `x`'s table in
place. To combine two tables into a new result without touching either, see
[Joining tables](#joining-tables).

### Joining tables

`dbdt_join()` is the read-path counterpart to `dbdt_merge()`: it joins two
tables in SQL and returns the result, leaving both inputs untouched, the way
base `merge()` does. Either side may be a `"duckdt"` handle or an ordinary
`data.frame`/`data.table` — R-side inputs are staged into a temporary table on
the connection first, so the join itself always runs in the engine:

```r
d <- dbdt(con, "gnaf_addresses")
f <- dbdt(con, "gnaf_locality_index")

dbdt_join(d, f, by = "locality_name")                 # two database tables
dbdt_join(d, my_data_table, by = "locality_name")     # database table + R table
dbdt_join(my_data_table, f, by = "locality_name")     # ...either way round

merge(d, f, by = "locality_name")                       # same thing, base spelling
```

`by`/`by.x`/`by.y`, `all`/`all.x`/`all.y`, `suffixes` and `sort` all behave as
in base `merge()`: no `all*` is an `INNER JOIN`, `all.x` a `LEFT JOIN`, `all.y`
a `RIGHT JOIN`, `all` a `FULL OUTER JOIN`. At least one side must be a
`"duckdt"` object — that's what says which database to run in — and if both
are, they must share a connection. Keys are compared with SQL's `=`, so missing
keys never match (base `merge()` would match `NA` to `NA`); `dbdt_merge()` has
the same rule.

#### Keeping a subset in the database

`d[i, j, by]` returns a `data.table`, so the rows have *left* the database and
can't be used where a handle is expected:

```r
dbdt_join(d[locality_name == "WORONGARY"], f, by = "locality_name")
# `d[...]` is already a data.table here — it works, but the subset made a
# round trip through R first.
```

`dbdt_temp()` runs the same `i`/`j`/`by` query into a temporary table and
hands back a `"duckdt"` handle, so nothing crosses into R:

```r
sub <- dbdt_temp(d, locality_name == "WORONGARY")
sub
#> <duckdt> duckdt_temp_gnaf_addresses_1 [2 x 3] (temp)

dbdt_join(sub, f, by = "locality_name")   # join runs entirely in DuckDB
dbdt_drop(sub)                            # done with it
```

The handle is materialized and writable, so `:=` and `dbdt_merge()` work
against it too — writes land on the temporary copy and leave the source table
alone. On DuckDB the table lives in the session's `temp` schema and disappears
when the connection closes; on MS SQL Server it's created with
`SELECT ... INTO` as an ordinary table, so call `dbdt_drop()` when you're
finished. As a guard, `dbdt_drop()` refuses handles `dbdt_temp()` didn't
create unless you pass `force = TRUE`.

### Getting data out

```r
as.data.table(d)   # SELECT * FROM <table>, materialized in R
```

### What's translated

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

Functions: `mean`, `sd`, `var`, `length`, `toupper`, `tolower` and `nchar` are
renamed to their DuckDB equivalents (`avg`, `stddev`, `variance`, `count`, `upper`,
`lower`, `length`); any other call is sent to DuckDB **by name**, upper-cased, so
`year()`, `substr()`, `coalesce()` and other DuckDB scalar functions work.

R values: a name that isn't a column is looked up in the calling environment, so
variables and function arguments work in `i` and `j`. Only *values* travel that
way -- a call to an R function with no DuckDB namesake (`as.Date()`, `ifelse()`)
is sent to DuckDB as written and fails. Work such values out first:
`cutoff <- as.Date("2024-03-05"); d[ordered_on >= cutoff]`.

Also worth knowing:

- `j` is a bare column or `.()`/`list()`. A bare call such as `d[, mean(x)]` is
  an error; write `d[, .(m = mean(x))]`.
- `order()` inside `i` (and row positions such as `d[1:5]`) are not supported --
  DuckDB tables have no row order. Sort the returned `data.table`:
  `d[cyl == 6][order(-mpg)]`.
- SQL aggregates already skip `NULL`, so `mean(x)` behaves like R's
  `mean(x, na.rm = TRUE)`; the `na.rm` argument itself is not translated and is
  an error.

### Binary columns

No driver hands a binary column (`BLOB`/`BIT` on DuckDB; `varbinary`, `binary` and
`image` on SQL Server) back as an R vector, and asking for one fails the whole
query. When rows are on their way into R, `duckdt` leaves binary columns out and
says so -- `print()` lists them as `Not fetched (binary)`, and `head()`, `tail()`,
`dbdt_sample()`, `as.data.table()` and `dbdt_join()` skip them too. Name one in
`j` to fetch it anyway (`d[, .(payload)]`); anything that stays in the database --
`:=`, `dbdt_temp()`, `dbdt_merge()` -- still sees every column, and you can
reduce a binary column in the database (`d[, .(bytes = octet_length(payload))]`).

### Peeking and sampling

```r
head(d, 3)          # SELECT * ... LIMIT 3
tail(d, 3)           # SELECT * ... LIMIT 3 OFFSET (nrow - 3)
dbdt_sample(d, 5)  # SELECT * ... USING SAMPLE reservoir(5 ROWS)
```

`tail()` reflects DuckDB's current scan order rather than a guaranteed original
row order, since DuckDB tables are unordered without an explicit `ORDER BY` (see
the row-position caveat below).

### MS SQL Server support

`dbdt()` also works against a Microsoft SQL Server connection (e.g. via
`DBI::dbConnect(odbc::odbc(), ...)`) — the dialect is auto-detected from the
connection object, so no extra argument is needed:

```r
con <- DBI::dbConnect(odbc::odbc(), driver = "ODBC Driver 18 for SQL Server", ...)
d <- dbdt(con, "existing_table")
d[cyl == 6, .(avg_hp = mean(hp), n = .N), by = cyl]
```

What's identical to the DuckDB path: `i`/`j`/`by` translation for
comparisons, `&`/`|`/`!`, `%in%`/`%chin%`, `%between%`, `is.na()`,
arithmetic, and most scalar/aggregate functions.

For quick exploration of large SQL Server tables:

```r
dbdt_tables(con)
dbdt_schema(con, table = "existing_table")  # filter metadata on the server
head(d, 100)                               # first rows, no random sort
dbdt_sample(d, 100, method = "fast")        # approximate sample of pages
print(d)                                  # preview without counting all rows
print(d, count = TRUE)                     # explicitly request an exact count
```

Fast sampling uses `TABLESAMPLE SYSTEM`, then randomizes the sampled rows and
returns at most `n`. It can return fewer rows (including zero); rows on the
same page are sampled together, so this is not a uniform row sample. It needs
a local base table, not a view. The default `method = "random"` retains exact
sampling up to the table size, using `ORDER BY NEWID()` on SQL Server, which
can be expensive on large tables. `nrow()`, `dim()` and `tail()` still request
an exact row count. SQL Server print previews display `?` for an uncounted total.

What's dialect-specific:
- `median()` compiles to `PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ...)`
  (T-SQL has no `MEDIAN()` aggregate). T-SQL also requires an `OVER` clause on
  `PERCENTILE_CONT`, which is not yet generated, so `median()` isn't reliable
  against SQL Server yet.
- `head()`/`print()` use `TOP (n)`, `tail()` uses
  `ORDER BY (SELECT NULL) OFFSET ... FETCH NEXT ...`, and `dbdt_sample()`
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
- Zero-copy `as.dbdt(x, copy = FALSE)` — registering an R data frame as a
  view with no copy is a DuckDB-specific mechanism. Use `copy = TRUE`
  against a SQL Server connection instead.
- `dbdt_csv()`/`dbdt_parquet()` — these are built on DuckDB's
  `read_csv_auto`/`read_parquet` table functions and stay DuckDB-only.

`:=` works against SQL Server too, but since T-SQL has neither
`CREATE OR REPLACE TABLE` nor `* EXCLUDE(...)`, it's emulated with an
explicit `SELECT ... INTO` rebuild + `sp_rename`, wrapped in a transaction.
This is not a true atomic replace (e.g. permissions/triggers on the
original table aren't preserved) — the same class of caveat the DuckDB
drop+recreate path already carries.

`dbdt_merge()` works against SQL Server too — since T-SQL has a native
`MERGE` statement, that path is actually simpler than DuckDB's: the staged
subset and a single `MERGE INTO ... USING ... ON (...) WHEN MATCHED ...`
statement, rather than DuckDB's separate `UPDATE`/`INSERT`/`DELETE`.

`dbdt_join()` works against SQL Server too — `INNER`/`LEFT`/`RIGHT`/`FULL
OUTER JOIN` are standard SQL. `dbdt_temp()` does differ: T-SQL has no
`CREATE TEMP TABLE ... AS`, so it uses `SELECT ... INTO`, which creates an
ordinary table on the current schema rather than a session-scoped one — drop
it with `dbdt_drop()` when you're done.

### Exploring a database

Four functions introspect a connection (works against both DuckDB and MS SQL
Server, via the portable ANSI `information_schema` views) to answer "what
tables are in this database, what do they look like, and how do they
connect" — all accept either a raw `DBI` connection or a `"duckdt"` object.

```r
dbdt_tables(con)         # every table/view: schema, name, type
dbdt_schema(con)         # every column: schema, table, column, type, primary_key
dbdt_schema(con, table = "orders")  # ...or just one table's columns
dbdt_relationships(con)  # declared foreign keys: fk_schema/table/column -> pk_schema/table/column
```

Each returns a plain `data.table`, so they compose with the rest of the
package/data.table normally, e.g. `dbdt_schema(con)[primary_key == TRUE]`
or `dbdt_relationships(con)[pk_table == "orders"]` to see what references a
given table. Foreign keys are only reported when the database actually
declares them as constraints — none of this guesses relationships from
column-naming conventions, and `dbdt_relationships()` returns a zero-row
`data.table` (rather than erroring) if the connected database/version
doesn't expose the constraint views.

#### Visualising the schema

`dbdt_erd()` builds on those three to write a self-contained HTML page and
open it in your browser — an ER diagram plus a searchable list of every table
and column:

```r
dbdt_erd(con)                            # opens the explorer in your browser
dbdt_erd(con, include_row_counts = TRUE) # add a COUNT(*) per table (can be slow)
dbdt_erd(con, tables = c("orders", "customers"), view = "keys_only")
```

The page is where the "which columns do I actually want" work happens: tick
tables and columns in the sidebar and it redraws the diagram and writes the
code that selects exactly those — `dbdt(con, "orders")[, .(id, total)]` for
a single table, or a `SELECT` with the joins worked out from the schema's
foreign keys for several — with a copy button. It needs no R packages beyond
duckdt; the diagram itself is drawn by Mermaid from a CDN, so with no internet
you get its source instead and everything else still works.

The returned path carries the Mermaid source as its `"mermaid"` attribute (drop
it straight into an Rmd/Quarto ```` ```mermaid ```` chunk) and the data model
as `"data_model"`.

`dbdt_explorer(con)` is the Shiny version of the same thing, and — since it
has a live connection — also previews the rows the query returns. It needs
`shiny`, and uses `DiagrammeR` and `DT` if they're installed.

#### The data model underneath

Both of those draw a **data model**: a description of tables, columns, keys and
references, ported from [datamodelr](https://github.com/bergant/datamodelr).
You can build one yourself, edit it, and render it:

```r
dm <- dbdt_data_model(con)     # or a "duckdt" handle, or a named list of data.frames
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
dm <- dbdt_dm_infer_references(dm)
dbdt_erd(con, infer_references = TRUE)   # or straight from the connection

# 2. State them exactly.
dm <- dbdt_dm_set_key(dm, "customers", "customer_id")
dm <- dbdt_dm_add_references(dm, orders$customer_id == customers$customer_id)
dm <- dbdt_dm_add_reference(dm, "orders", "customer_id", "customers", "customer_id")  # same, from strings
```

`is_dbdt_data_model(x)` tests whether an object is a model.

Then zoom in, group and colour tables, and draw it:

```r
dbdt_dm_filter(dm, "orders", depth = 1)          # orders + whatever it touches
dbdt_dm_set_segment(dm, list(sales = c("orders", "order_lines")))
dbdt_dm_set_display(dm, list(accent1 = "customers", hide = "audit_log"))

dbdt_erd(dm)                                      # the interactive page
dbdt_dm_mermaid(dm, view = "keys_only")           # Mermaid source
dbdt_dm_dot(dm, rankdir = "LR")                   # Graphviz DOT (prints via DiagrammeR)
dbdt_dm_render(dm)                                # htmlwidget, for Rmd/Shiny
dbdt_dm_export(dm, "schema.png")                  # image file
```

And build the query for a set of tables without the browser at all —
`dbdt_dm_query()` works out the joins from the model's references:

```r
dbdt_dm_query(dm, c("orders", "customers"),
                columns = list(orders = "total", customers = "name"),
                where = "total > 100")
```

Tables it can't reach by any reference are listed in the result's `"unjoined"`
attribute rather than silently cross-joined.

##### Diagram colours

Table colours come from a *scheme*: a named list of four-colour *palettes*. The
built-in scheme has `"default"`, `"accent1"` to `"accent7"`, and border-less
`"accent1nb"` to `"accent7nb"`. Add your own, then pick it per table with
`dbdt_dm_set_display()`:

```r
fresh <- dbdt_dm_palette(
    line_color = "#1a7f64", header_bgcolor = "#22a37f",
    header_font = "#FFFFFF", bgcolor = "#E6F5F0"
)
dbdt_dm_add_colors(dbdt_dm_color_scheme(fresh = fresh))   # add to the scheme in use
dm <- dbdt_dm_set_display(dm, list(fresh = "customers"))

dbdt_dm_get_color_scheme()     # the scheme in use (kept in the `duckdt.dm_scheme` option)
# dbdt_dm_set_color_scheme(dbdt_dm_color_scheme(fresh = fresh))   # or replace it wholesale
```

### Not yet supported

- Lazy/chained query building — every `[` runs immediately (by design, see below).
  `dbdt_temp()` is the escape hatch: it runs an `[i, j, by]` query into a temporary
  table and returns a handle, so a subset can stay in the database and be chained into
  another query.
- Row-position indexing in `i` (e.g. `d[1:5]`) and `order()` in `i` — DuckDB tables are
  unordered, so neither is meaningful without an explicit sort; filter on a column, or
  sort the returned `data.table`.
- Grouped `:=` (`by=` together with a write).
- `dbdt_sample(x, n, replace = TRUE)` — DuckDB's sampling clause doesn't support
  sampling with replacement; only `replace = FALSE` (the default, and only) behavior
  is available.
- A bare function call in `j` (`d[, mean(x)]`; write `d[, .(m = mean(x))]`), `na.rm =`,
  and R functions with no DuckDB namesake such as `ifelse()` and `as.Date()`.

### Design notes

v1 is deliberately **eager**: every `[` issues one query and returns a `data.table`.
This keeps the mental model identical to plain data.table. A lazy mode (building up a
query across multiple `[` calls before touching R, à la `dtplyr::lazy_dt()`) is a
natural next step if it turns out to matter for your workloads. In the meantime
`dbdt_temp()` covers the case that actually bites — a subset you want to feed to
another database-side operation — by materializing it into a temporary table rather
than into R.

### Slides

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

---

## Options

| Option | Default | Effect |
|:---|:---|:---|
| `duckdt.quiet` | unset | `TRUE`, set **before** attaching the package, hides the startup banner. It also silences the one-off note that binary columns were left out of a table's results. |
| `duckdt.dm_scheme` | the built-in scheme | The colour scheme for data model diagrams. Set by `dbdt_dm_set_color_scheme()` and `dbdt_dm_add_colors()`. |
| `duckdt.mermaid_src` | Mermaid 11 from the jsDelivr CDN | Where the `dbdt_erd()` page loads Mermaid from. Point it at a local copy or an intranet URL to draw diagrams without internet access. |
| `foam.sample_dt.color` | `TRUE` | The session default for `color` in `sample_dt()`, `cdt()` and `dupe_dt()`. Set it with `switch_col()`. (The name is a legacy of the package's earlier name.) |
| `datatable.print.*` | `data.table`'s own | The print mask reads `datatable.print.topn`, `nrows`, `class`, `rownames`, `colnames`, `keys` and `trunc.cols`, and `datatable.show.indices`. |

## Known limitations

The [documentation site](site/) has the full list, with runnable demonstrations
(`site/limitations.qmd`). The ones most likely to surprise:

- **`:=` rebuilds the table.** It drops primary keys and `NOT NULL` constraints,
  cannot run on a table another table references, is not atomic across several
  assigned columns, and prints the handle after each assignment. See
  [Writing data](#writing-data).
- **Missing keys never match** in `dbdt_join()` or `dbdt_merge()` (SQL `=`
  semantics), where base `merge()` would match `NA` to `NA`.
- **Do not re-wrap a temporary table** with `dbdt()`: it loses its temporary
  status, and a `:=` through the new handle can create a permanent table of the
  same name. Use the handle `dbdt_temp()` returned. Merges stage their subset in
  a temporary table named `<table>__duckdt_merge_tmp` (and SQL Server `:=` uses
  `<table>__duckdt_tmp`), which is replaced and dropped without a check, so do not
  name your own tables that way.
- **`e()` ignores the row filter in `i`**: `dt[x == 1, e(c("x", "y"))]` returns
  every row. Use `.SD` with `.SDcols`.
- **`group_value_mode = "similarity"`** does not change how `sample_dt()`, `cdt()`
  and `dupe_dt()` print, because they store `"distinct"` on their results and a
  mode stored on a table wins over the session setting (see
  [Example 10](#example-10-grouped-value-colouring-modes)).
- **The print mask truncates at 11 rows.** With the mask on, a table of more than
  11 rows shows its first and last five rows, where plain `data.table` shows up to
  100. Raise `topn` (`print(DT, topn = 50)` or `options(datatable.print.topn = 50)`)
  to see more. Grouped output always prints every row.
- **The masks live in the global environment.** Enabling them replaces any
  `print.data.table`, `str` or `dput` you defined there, and disabling them removes
  whatever is defined without restoring it.
- **SQL Server** support is tested against the SQL that is emitted, not a live
  server; `median()` needs an `OVER` clause that is not yet generated and the
  regular-expression operators need SQL Server 2025. See
  [MS SQL Server support](#ms-sql-server-support).

## Credits

The data model and diagram code (`dbdt_data_model()`, the `duckdt_dm_*()`
functions and the Graphviz rendering) is a port of Darko Bergant's
[datamodelr](https://github.com/bergant/datamodelr), MIT licensed, extended
here to reverse-engineer live DuckDB/SQL Server connections, guess undeclared
references, and drive the interactive column picker and query builder.
