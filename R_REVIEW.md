**R-script review — 16 September 2026**

Reviewed the package's R code, tests, benchmark, and slide helper scripts. The original test suite passed, but targeted examples exposed failures in missing-value handling, grouping, compound relationships, serialization, and write behavior. The changes below address bounded defects; the remaining findings describe work that needs a separate design pass.

**Improvements implemented**

| Area | Previous behavior | Result |
| --- | --- | --- |
| Membership translation (`R/translate.R`) | Empty sets emitted invalid `IN ()`; missing values were lost from matches and negated matches. | `%in%` and `%chin%` handle empty sets and missing values explicitly. Tests include character, date, and negated filters. |
| Named grouping (`R/translate.R`, `R/bracket.R`) | `by = .(bucket = expression)` put `AS bucket` inside `GROUP BY`. | Group expressions and selected aliases are rendered separately; both normal queries and temporary tables work. |
| SQL function fallback (`R/translate.R`) | Functions outside `FN_MAP`, such as `sqrt()`, failed during R vector indexing. | The intended SQL function fallback is reached. |
| Query generation (`R/query-builder.R`) | Disconnected tables were omitted from joins but still appeared in SELECT; compound join aliases used recursive list indexing. | Disconnected tables are consistently excluded and reported through `unjoined`; compound joins render and execute. |
| Foreign keys (`R/introspect.R`) | A two-column foreign key produced four pairings. | DuckDB joins column positions; SQL Server uses exact column pairs from its system catalog. |
| Frequencies (`R/utils-eda.R`) | Documented `n = Inf` and large finite limits overflowed integer conversion. | Limits are bounded before conversion; empty results preserve their types. |
| Duplicate detection (`R/sampling-and-switches.R`) | An existing `.dupe_n` column was overwritten and removed. | Duplicate detection preserves that column and leaves the caller's table untouched. |
| `dput()` mask (`R/dput-mask.R`) | Pointer removal depended on attribute order, leaving unparsable output when custom attributes followed the pointer. | Serialization removes the pointer from a copy and preserves the original object, other attributes, file output, and invisible return value. |
| Preview and sampling (`R/head-tail.R`) | Both requested full dimensions before fetching rows, potentially scanning a file-backed view unnecessarily. | Ordinary positive `head()` and sampling skip the preliminary count. Large finite preview limits avoid integer overflow. |
| Supporting scripts | Scripts referenced the old `duckdt` package; top-level `on.exit()` lacked function scope; the slide demo deleted an existing database file. | Scripts reference `data.table.ext`, cleanup runs within `local()`, and the demo refuses existing files and closes its connection if setup fails. |

Foreign-key mapping was checked against the [DuckDB information schema](https://duckdb.org/docs/current/sql/meta/information_schema) and [Microsoft's catalog definition](https://github.com/MicrosoftDocs/sql-docs/blob/live/docs/relational-databases/system-catalog-views/sys-foreign-key-columns-transact-sql.md). The SQL Server catalog query was executed against a DuckDB fixture with reversed referenced-column positions; this is not a live SQL Server integration test.

**Remaining findings, in priority order**

1. **High: mutation can strip constraints and leave partial writes.** `R/mutate.R`, `duckdt_mutate()` and the two rebuild helpers, replace the entire table for each assigned column. In an isolated DuckDB database, a table with a primary key and NOT NULL constraints had three catalog constraints before `:=` and zero afterwards. A multi-column assignment whose second expression could not resolve a symbol retained the first write. Rebuilding once per column also multiplies work on large tables. Prefer schema-preserving updates where possible, plan all assignments before writing, and make the whole operation transactional. Type-changing assignments and simultaneous RHS evaluation need an explicit contract. DuckDB documents that [CREATE OR REPLACE drops and recreates the table](https://duckdb.org/docs/current/sql/statements/create_table).

2. **High: staging names can overwrite unrelated database objects.** `R/merge.R` derives a fixed `<table>__duckdt_merge_tmp` name, and `R/mutate.R` uses `<table>__duckdt_tmp` for SQL Server rebuilds. A pre-existing DuckDB temporary table named `constrained__duckdt_merge_tmp` was replaced and then deleted by a merge into `constrained`. SQL Server staging and `duckdt_create_temp()` can drop ordinary tables with the chosen name. Generate unique names, create without replacement, and clean up only objects the operation created. Collision checks must precede any drop.

3. **High: `e()` can discard the caller's row selection.** `R/sampling-and-switches.R`, `e()`, searches frames for a table and reconstructs columns from that full table. With `dt <- data.table(x = 1:3, y = 4:6)`, `dt[x == 1, e(c("x", "y"))]` returns all three rows. Grouped evaluation is also exposed to this approach. Prefer an explicit current-subset interface over searching arbitrary frames. As an immediate user workaround, `dt[x == 1, .SD, .SDcols = c("x", "y")]` preserves the subset. This remains unfixed because data.table can leave `.SD` empty when it is not referenced in `j`; simply preferring `.SD` inside the helper is insufficient.

4. **High: rewrapping a temporary table loses its physical table identity.** `R/class-duckdt.R`, `duckdt()`, detects materialization but does not preserve temporary status. Rewrapping a `duckdt_temp()` table with `writable = TRUE` and assigning through it created a permanent table of the same name; reads continued returning the unchanged temporary table. Resolve catalog/schema identity and temporary status when constructing handles. Keep physical temporary status separate from the ownership marker that authorizes `duckdt_drop()`.

5. **Medium: remaining R/SQL missing-value differences need a documented policy.** `duckdt_join_sql()` and the merge execution helpers use `=` for keys. Two NA keys produced no inner-join row in the reproduction, unlike base/data.table merge behavior; an upsert with a missing key can consequently insert instead of update. `translate_call_default()` also sends `na.rm = TRUE` to SQL as an extra function argument: `mean(x, na.rm = TRUE)` becomes an invalid two-argument `avg()`. Decide and test missing-key matching and aggregate missingness across both backends; the membership fix does not resolve these other cases.

6. **Medium: some SQL Server translations are validated only as strings and are invalid SQL.** `translate_call_default()` emits `PERCENTILE_CONT(...) WITHIN GROUP (...)` without the required `OVER` clause, and its test currently expects that exact output. Microsoft's [PERCENTILE_CONT documentation](https://learn.microsoft.com/en-us/sql/t-sql/functions/percentile-cont-transact-sql?view=sql-server-ver17) requires a window; grouped aggregation needs a query-level transformation, not just appending `OVER ()`. Separately, `duckdt_dm_query()` always appends `LIMIT`, including for the SQL Server explorer. Add a live SQL Server test job and carry the dialect through model query generation.

7. **Medium: global display masks do not preserve pre-existing user bindings.** The enable functions overwrite `print.data.table`, `str`, and `dput` in `.GlobalEnv`; disable functions remove them without checking ownership or restoring previous bindings. Store prior bindings and restore only package-owned replacements. `.onAttach()` also emits CLI output outside `packageStartupMessage()`, causing the package-check note about startup messages that cannot be suppressed.

8. **Medium: connection creation needs failure cleanup.** `as.duckdt()`, `duckdt_from_reader()`, and `duckdt_example()` create connections before operations that can fail, without closing newly owned connections on those failure paths. Track connection ownership and transfer it only after successful setup; never close a caller-supplied connection. The slide demo now follows this pattern, but the exported helpers still need it.

The first four findings were reproduced against local objects or isolated in-memory databases. The SQL Server limitation is based on emitted SQL and official documentation; no live SQL Server was available. None of the remaining write-path findings has been silently treated as fixed by this patch.

**Further performance work worth measuring**

- `key_dt()` constructs all combinations at each size before skipping supersets of known keys. Generate candidates incrementally and prune earlier for wide tables.
- Group display performs additional copies after row subsetting; color-map construction can process more rows than the final truncated print displays. Profile allocation and printed-row selection before changing ownership behavior.
- Query operations repeatedly fetch column names and column types. Reuse metadata within one operation first; cross-operation caching would need invalidation for `:=`, external DDL, and schema changes.
- The benchmark's small-input timings can be zero at the clock's resolution, producing infinite throughput. Use adaptive repeated measurements before interpreting those figures as a performance comparison.

**Validation**

Focused regression tests were run against the original implementation and failed before the fixes. The final full suite passed: 262 tests, 636 expectations, zero failures, errors, test warnings, or skips. The benchmark was smoke-tested with 100 and 1,000 rows and one repetition; cleanup of its temporary directory was verified. The screenshot script was inspected but not executed with a browser.

Final `R CMD check --no-manual --no-build-vignettes`: zero errors, zero warnings, and one existing note about startup output that cannot be suppressed. `git diff --check` passed. Generated relationship documentation was refreshed with roxygen2.

The initial package check stopped because the inherited `C.UTF-8` locale was invalid for this Windows R installation. Running child processes with `LC_ALL=C` and `LANG=C` resolved that environment issue. R 4.5.0 also reports that several installed dependencies were built under R 4.5.3.

The pre-existing untracked `test.qmd` was left untouched.
