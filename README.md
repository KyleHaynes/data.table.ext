# duckdt

Query [DuckDB](https://duckdb.org/) using [data.table](https://r-datatable.com/)'s
`d[i, j, by]` syntax. Expressions are translated to SQL and run inside DuckDB — data
only comes back to R as a `data.table` once you materialize a result.

It also draws the database: `duckdt_erd(con)` opens an ER diagram of every
table and how they connect, and builds the query for the tables and columns
you tick. See [Exploring a database](#exploring-a-database).

Saving a `data.table` to a persistent DuckDB file and reconnecting to it
later? See [WORKFLOW.md](WORKFLOW.md) for a step-by-step walkthrough.

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
