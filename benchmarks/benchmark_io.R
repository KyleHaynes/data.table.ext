# Benchmark: duckdt I/O vs saveRDS/readRDS, fwrite/fread, and other fast I/O
# packages (arrow/parquet, fst, qs -- whichever are installed).
#
# Compares round-trip write/read timings for a data.table across:
#   - base R:     saveRDS()       / readRDS()
#   - data.table: fwrite()        / fread()
#   - arrow:      write_parquet() / read_parquet()          (if installed)
#   - fst:        write_fst()     / read_fst()               (if installed)
#   - qs:         qsave()         / qread()                  (if installed)
#   - duckdt:     as.duckdt(..., copy = TRUE) / as.data.table(duckdt(...))
#                 i.e. writing/reading a table in a persistent .duckdb file,
#                 the workflow documented in WORKFLOW.md
#
# The benchmark data is all-character (string) columns of mixed cardinality
# -- text parsing/encoding is where these formats differ most, whereas an
# all-numeric table would mostly just measure memcpy speed.
#
# Usage:
#   Rscript benchmarks/benchmark_io.R
#
# Row counts / repetitions can be overridden (handy for a quick smoke test):
#   DUCKDT_BENCH_SIZES=1000,5000 DUCKDT_BENCH_REPS=1 Rscript benchmarks/benchmark_io.R

# Give on.exit() a function scope when this file is run with Rscript.
local({
suppressPackageStartupMessages({
  library(data.table)
  library(DBI)
  library(duckdb)
  library(data.table.ext)
})

has_arrow <- requireNamespace("arrow", quietly = TRUE)
has_fst   <- requireNamespace("fst", quietly = TRUE)
has_qs    <- requireNamespace("qs", quietly = TRUE)

sizes <- as.numeric(strsplit(Sys.getenv("DUCKDT_BENCH_SIZES", "10000,10000000"), ",")[[1]])
reps_override <- Sys.getenv("DUCKDT_BENCH_REPS", "")

# --- data generation: 10 string columns of mixed cardinality ---------------

rand_strings <- function(n, len) {
  cols <- replicate(len, sample(letters, n, replace = TRUE), simplify = FALSE)
  Reduce(paste0, cols)
}

make_data <- function(n) {
  set.seed(1)
  data.table(
    id           = sprintf("ID%010d", seq_len(n)),               # unique
    category     = sample(sprintf("cat_%02d", 1:20), n, replace = TRUE),
    region       = sample(c("APAC", "EMEA", "AMER", "LATAM"), n, replace = TRUE),
    status       = sample(c("active", "inactive", "pending", "closed"), n, replace = TRUE),
    country_code = sample(sprintf("C%03d", 1:50), n, replace = TRUE),
    product_code = rand_strings(n, 6),                            # high cardinality
    tag          = sample(sprintf("tag_%03d", 1:100), n, replace = TRUE),
    email_domain = sample(c("gmail.com", "outlook.com", "yahoo.com", "company.com", "example.org"), n, replace = TRUE),
    description  = rand_strings(n, 12),                           # high cardinality, longer
    notes        = rand_strings(n, 20)                            # high cardinality, longest
  )
}

# --- timing helper -----------------------------------------------------------

time_it <- function(f, reps) {
  times <- numeric(reps)
  for (i in seq_len(reps)) {
    gc(FALSE)
    t0 <- proc.time()[["elapsed"]]
    f()
    times[i] <- proc.time()[["elapsed"]] - t0
  }
  stats::median(times)
}

to_mb <- function(bytes) bytes / (1024^2)

# --- benchmark one dataset size -----------------------------------------------

bench_size <- function(n, reps, tmpdir) {
  message(sprintf("== n = %s rows, reps = %d ==", format(n, big.mark = ",", scientific = FALSE), reps))
  dt <- make_data(n)

  rds_path     <- file.path(tmpdir, "bench.rds")
  csv_path     <- file.path(tmpdir, "bench.csv")
  parquet_path <- file.path(tmpdir, "bench.parquet")
  fst_path     <- file.path(tmpdir, "bench.fst")
  qs_path      <- file.path(tmpdir, "bench.qs")
  duckdb_path  <- file.path(tmpdir, "bench.duckdb")

  rows <- list()
  add <- function(format, op, secs, size_bytes) {
    rows[[length(rows) + 1]] <<- data.table(
      format = format, op = op, n = n, seconds = secs,
      rows_per_sec = n / secs,
      mb_per_sec = to_mb(size_bytes) / secs,
      file_mb = to_mb(size_bytes)
    )
  }

  # base R --------------------------------------------------------------------
  add("saveRDS/readRDS", "write", time_it(function() saveRDS(dt, rds_path), reps), file.size(rds_path))
  add("saveRDS/readRDS", "read",  time_it(function() readRDS(rds_path), reps), file.size(rds_path))

  # data.table ------------------------------------------------------------------
  add("fwrite/fread", "write", time_it(function() fwrite(dt, csv_path), reps), file.size(csv_path))
  add("fwrite/fread", "read",  time_it(function() fread(csv_path), reps), file.size(csv_path))

  # arrow (parquet) ---------------------------------------------------------------
  if (has_arrow) {
    add("arrow (parquet)", "write", time_it(function() arrow::write_parquet(dt, parquet_path), reps), file.size(parquet_path))
    add("arrow (parquet)", "read",  time_it(function() as.data.table(arrow::read_parquet(parquet_path)), reps), file.size(parquet_path))
  }

  # fst -----------------------------------------------------------------------------
  if (has_fst) {
    add("fst", "write", time_it(function() fst::write_fst(dt, fst_path), reps), file.size(fst_path))
    add("fst", "read",  time_it(function() fst::read_fst(fst_path, as.data.table = TRUE), reps), file.size(fst_path))
  }

  # qs --------------------------------------------------------------------------------
  if (has_qs) {
    add("qs", "write", time_it(function() qs::qsave(dt, qs_path), reps), file.size(qs_path))
    add("qs", "read",  time_it(function() qs::qread(qs_path), reps), file.size(qs_path))
  }

  # duckdt (persistent .duckdb file -- see WORKFLOW.md) --------------------------------
  if (file.exists(duckdb_path)) file.remove(duckdb_path)
  add("duckdt (.duckdb)", "write", time_it(function() {
    con <- suppressMessages(dbConnect(duckdb::duckdb(dbdir = duckdb_path)))
    on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
    as.duckdt(dt, conn = con, name = "bench", copy = TRUE, overwrite = TRUE)
  }, reps), file.size(duckdb_path))

  add("duckdt (.duckdb)", "read", time_it(function() {
    con <- suppressMessages(dbConnect(duckdb::duckdb(dbdir = duckdb_path)))
    on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
    as.data.table(duckdt(con, "bench"))
  }, reps), file.size(duckdb_path))

  rbindlist(rows)
}

# --- run -----------------------------------------------------------------------------

tmpdir <- tempfile("duckdt-bench-")
dir.create(tmpdir)
on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

results <- rbindlist(lapply(sizes, function(n) {
  reps <- if (nzchar(reps_override)) as.integer(reps_override) else if (n <= 1e5) 4L else 2L
  bench_size(n, reps, tmpdir)
}))

results[, `:=`(
  seconds = round(seconds, 3),
  rows_per_sec = round(rows_per_sec),
  mb_per_sec = round(mb_per_sec, 1),
  file_mb = round(file_mb, 1)
)]

setorder(results, n, op, format)
print(results)

invisible(results)
})
