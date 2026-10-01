# data.table.ext

`data.table.ext` bundles two families of `data.table` extensions in one package:

1. **Display, sampling & readability utilities**: cleaner printing, presentation-friendly grouped sampling, friendlier `str()`/`dput()` output, row highlighting, duplicate and outlier detection, table comparison, markdown export, and more.
2. **`dbdt`: database querying**: a `data.table`-style `[i, j, by]` interface for [DuckDB](https://duckdb.org/) and Microsoft SQL Server. The query runs in the database and only the result comes back to R. It also reverse-engineers a database into a data model and draws it as an interactive entity-relationship diagram.

The database helpers are named `dbdt()`, `as.dbdt()` and `dbdt_*()`. The older `duckdt` names remain as identical aliases, and the S3 classes and `duckdt.*` options are unchanged. `dbdt_connect()` opens DuckDB; for SQL Server, pass your existing `DBI`/`odbc` connection to `dbdt()` and the other helpers.

The full guide, with runnable examples of everything, is the [documentation site](site/). This page is the overview.

## Contents

- [Installation](#installation)
- [Getting started](#getting-started)
- [Function index](#function-index)
- [On SQL Server](#on-sql-server)
- [Documentation](#documentation)
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
| `odbc` (or another DBI driver) | [MS SQL Server](#on-sql-server) |

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
sample_dt(DT, n = 2, group = Species)    # colour-coded grouped sampling
schema_dt(DT)                            # one row per column: class, distinct, NA
dupe_dt(DT)                              # every row that has a duplicate

con <- dbdt_example()                    # a small database to try things on
d <- dbdt(con, "orders")
d[, .N, by = customer_id]                # data.table syntax, run as SQL in the database
d[order(-ordered_on), .(order_id, ordered_on)]
dbdt_profile(d)                          # every column summarised, in the database
dbdt_erd(con)                            # draw the database in your browser
```

## Function index

Every exported function, grouped by theme as in the startup banner.

**Display, sampling & readability**

| Theme | Functions | For |
|:---|:---|:---|
| Session | `turn_everyone_on()`, `enable_dt_print_thousands()`, `disable_dt_print_thousands()`, `enable_dt_str_mask()`, `disable_dt_str_mask()`, `enable_dt_dput_mask()`, `disable_dt_dput_mask()`, `switch_col()` | Switching the display enhancements on and off |
| Sample | `sample_dt()`, `cdt()` | Group-aware sampling, and colour-coded display of a whole table |
| Spot | `highlight_dt()`, `dupe_dt()`, `outlier_dt()` | Surfacing rows of interest |
| Profile | `schema_dt()`, `na_dt()`, `freq_dt()`, `spark_dt()`, `key_dt()`, `compare_dt()` | A first look at a table, and what changed between two versions of one |
| Columns | `e()`, `rename_dt()`, `set_null()` | Selecting, renaming and dropping columns |
| Share | `md_dt()`, `copy_dt()` | Markdown export, to the console or the clipboard |

**`dbdt`: `data.table` syntax on DuckDB and SQL Server**

| Theme | Functions | For |
|:---|:---|:---|
| Connect | `dbdt_connect()`, `dbdt_disconnect()`, `dbdt_example()` | Opening and closing a database; an example one |
| Load | `as.dbdt()`, `dbdt()`, `dbdt_csv()`, `dbdt_parquet()` | Getting data in: from R, from files, from an existing table |
| Query | `d[i, j, by]`, `head()`, `tail()`, `dbdt_sample()`, `dbdt_profile()`, `dbdt_sql()` | Querying, sorting, peeking, sampling and profiling; seeing the SQL |
| Join & write | `dbdt_join()` (`merge()`), `dbdt_merge()`, `dbdt_temp()`, `dbdt_drop()`, `:=` | Joining, patching and changing tables inside the database |
| Explore | `dbdt_tables()`, `dbdt_schema()`, `dbdt_relationships()`, `dbdt_erd()`, `dbdt_explorer()` | Finding your way around a database |
| Model | `dbdt_data_model()`, `is_dbdt_data_model()`, `dbdt_dm_set_key()`, `dbdt_dm_add_references()`, `dbdt_dm_add_reference()`, `dbdt_dm_infer_references()`, `dbdt_dm_set_segment()`, `dbdt_dm_set_display()`, `dbdt_dm_filter()`, `dbdt_dm_query()` | Building, editing and querying a data model |
| Draw | `dbdt_dm_mermaid()`, `dbdt_dm_dot()`, `dbdt_dm_render()`, `dbdt_dm_export()`, `dbdt_dm_palette()`, `dbdt_dm_color_scheme()`, `dbdt_dm_add_colors()`, `dbdt_dm_get_color_scheme()`, `dbdt_dm_set_color_scheme()` | Drawing a model, and its colours |

`head()`, `tail()`, `merge()`, `dim()`, `names()` and `as.data.table()` work on a `"duckdt"` handle as S3 methods. The [function reference](site/reference.qmd) has a line on every function and links to where each is covered.

Inside a query, `i`, `j` and `by` understand comparisons, `%in%`, `%like%` and friends, `order()` for sorting, aggregates including `uniqueN()` and `median()`, and the common R and `data.table` functions: `ifelse()`/`fifelse()`/`fcase()`, `paste0()`, `substr()`, `grepl()`, `as.integer()`, `year()`/`month()`/`wday()` and more. The [query guide](site/duckdt-query.qmd#functions) lists them.

## On SQL Server

```r
con <- DBI::dbConnect(odbc::odbc(), driver = "ODBC Driver 18 for SQL Server", ...)
d <- dbdt(con, "orders", schema = "sales")
d[status == "open", .(n = .N, value = sum(total)), by = region]
```

The dialect is detected from the connection. Large databases are where the details matter, so `duckdt` keeps catalogue work to a minimum: a handle looks its table up once by `OBJECT_ID()` and keeps its column list, `print()` reads the row count from partition metadata instead of counting, and `dbdt_schema()`, `dbdt_erd()` and the data model read the `sys.*` catalogue views rather than the much slower `INFORMATION_SCHEMA`. `:=` updates a table in place, keeping its keys, indexes and permissions.

When a call is slow, `options(duckdt.trace = TRUE)` prints every statement sent and the time the server took, and `dbdt_sql()` gives you a query's SQL to look at its plan in SQL Server Management Studio. [MS SQL Server](site/duckdt-mssql.qmd) covers the rest, including what differs from DuckDB.

## Documentation

- **[Documentation site](site/)**: a [Quarto](https://quarto.org/) website with runnable examples. It covers getting started and [real-world examples](site/examples.qmd) (reconnecting to a warehouse file, an unfamiliar database, cleaning a messy export). It has guides to printing and sampling, data checks, and the `dbdt` pages: [connecting and loading](site/duckdt-connect.qmd) (including [keeping a database between sessions](site/duckdt-connect.qmd#persist)), [querying](site/duckdt-query.qmd), [joining and writing](site/duckdt-join-write.qmd), [exploring a database](site/duckdt-explore.qmd) and [SQL Server](site/duckdt-mssql.qmd). It also has a [function reference](site/reference.qmd), [measured benchmarks](site/benchmarks.qmd) and a [limitations](site/limitations.qmd) page. Build it from the repository root:

  ```sh
  quarto render site      # writes site/_site
  quarto preview site     # live preview while you edit
  ```

  Rendering runs every example, so the package must be installed first (`install.packages(".", repos = NULL, type = "source")`), along with `knitr` and, for the diagram examples, `DiagrammeR`. `quarto publish gh-pages site` publishes it to GitHub Pages.
- **Help pages**: `?dbdt_join`, or `help(package = "data.table.ext")` for all of them.
- **Slides**: `inst/slides/duckdt-intro.qmd` is a Quarto revealjs deck introducing `dbdt` to a team that doesn't know it yet, with live output from a small made-up address database (`inst/slides/demo-db.R`). `quarto render duckdt-intro.qmd` in that folder writes one self-contained HTML file that works offline.

## Options

| Option | Default | Effect |
|:---|:---|:---|
| `duckdt.quiet` | unset | `TRUE`, set **before** attaching the package, hides the startup banner. It also silences the one-off note that binary columns were left out of a table's results. |
| `duckdt.trace` | unset | `TRUE` prints every SQL statement `duckdt` sends, with the time the database took over it. |
| `duckdt.dm_scheme` | the built-in scheme | The colour scheme for data model diagrams. Set by `dbdt_dm_set_color_scheme()` and `dbdt_dm_add_colors()`. |
| `duckdt.mermaid_src` | Mermaid 11 from the jsDelivr CDN | Where the `dbdt_erd()` page loads Mermaid from. Point it at a local copy or an intranet URL to draw diagrams without internet access. |
| `foam.sample_dt.color` | `TRUE` | The session default for `color` in `sample_dt()`, `cdt()` and `dupe_dt()`. Set it with `switch_col()`. (The name is a legacy of the package's earlier name.) |
| `datatable.print.*` | `data.table`'s own | The print mask reads `datatable.print.topn`, `nrows`, `class`, `rownames`, `colnames`, `keys` and `trunc.cols`, and `datatable.show.indices`. |

## Known limitations

The [limitations page](site/limitations.qmd) has the full list, with runnable demonstrations. The ones most likely to surprise:

- **Every `[` runs immediately** and returns a `data.table`; there is no lazy query building. [`dbdt_temp()`](site/duckdt-join-write.qmd#temp) keeps a subset in the database when you need one.
- **Missing keys never match** in `dbdt_join()` or `dbdt_merge()` (SQL `=` semantics), where base `merge()` would match `NA` to `NA`.
- **Aggregates skip missing values**, as `na.rm = TRUE` would, and `uniqueN()` doesn't count `NA`.
- **Do not re-wrap a temporary table** with `dbdt()`: it loses its temporary status. Keep the handle `dbdt_temp()` returned.
- **`e()` ignores the row filter in `i`**: `dt[x == 1, e(c("x", "y"))]` returns every row. Use `.SD` with `.SDcols`.
- **The print mask truncates at 11 rows**, where plain `data.table` shows up to 100. Raise `topn` (`print(DT, topn = 50)` or `options(datatable.print.topn = 50)`) to see more.
- **The masks live in the global environment.** Enabling them replaces any `print.data.table`, `str` or `dput` you defined there, and disabling them removes whatever is defined without restoring it.
- **SQL Server** support is tested against the SQL that is emitted, not a live server. Regular expressions need `REGEXP_LIKE()` (SQL Server 2025); plain-text `%like%` patterns work on any version.

## Credits

The data model and diagram code (`dbdt_data_model()`, the `duckdt_dm_*()`
functions and the Graphviz rendering) is a port of Darko Bergant's
[datamodelr](https://github.com/bergant/datamodelr), MIT licensed, extended
here to reverse-engineer live DuckDB/SQL Server connections, guess undeclared
references, and drive the interactive column picker and query builder.
