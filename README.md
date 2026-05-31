# data.table.ext

`data.table.ext` is a focused utility package for making `data.table` exploration easier to read, easier to scan, and easier to demo.

It does this in three big ways:

1. It upgrades print ergonomics for large tables.
2. It makes grouped sampling much more presentation-friendly.
3. It smooths `str()` and `dput()` output for `data.table` objects.
4. It evaluates function calls in `j = ` using `e()`.

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

### Example 11: Select columns with regex using e()valuate

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

## Exported functions

- `enable_dt_print_thousands()`
- `disable_dt_print_thousands()`
- `enable_dt_str_mask()`
- `disable_dt_str_mask()`
- `enable_dt_dput_mask()`
- `disable_dt_dput_mask()`
- `e()`
- `sample_dt()`
- `set_null()`
- `switch_col()`
- `turn_everyone_on()`
