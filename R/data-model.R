#' Build a data model from a database, or from data frames
#'
#' A "data model" is a portable description of a set of tables, their
#' columns, and the references between them -- the same idea (and largely the
#' same shape) as the object used by the
#' [datamodelr](https://github.com/bergant/datamodelr) package, which this is
#' a port of. It is the thing [duckdt_erd()], [duckdt_dm_mermaid()],
#' [duckdt_dm_dot()] and [duckdt_explorer()] all draw.
#'
#' Reverse-engineering a live connection uses [duckdt_tables()],
#' [duckdt_schema()] and [duckdt_relationships()], so it works against both
#' DuckDB and MS SQL Server. Only foreign keys the database actually declares
#' as constraints are picked up; DuckDB databases built from CSV/Parquet
#' loads usually declare none, so see [duckdt_dm_infer_references()] (or
#' `infer_references = TRUE`) to guess them from column names, and
#' [duckdt_dm_add_references()] to state them by hand.
#'
#' @param x A `DBI` connection, a `"duckdt"` object, a named list of data
#'   frames, or a column-info `data.frame` (with at least `table` and
#'   `column` columns, optionally `type`, `key`, `ref`, `ref_col`).
#' @param tables Optionally, a character vector restricting the model to
#'   these tables. Matched against the bare table name or `"schema.name"`.
#' @param infer_references Also guess references from column naming
#'   conventions, via [duckdt_dm_infer_references()]. Default `FALSE`.
#' @param row_counts Run a `SELECT count(*)` per table and record it on the
#'   model. Default `FALSE`, since this can be slow on a large or remote
#'   database; a failure on any single table records `NA` rather than
#'   failing the whole call.
#' @param ... Passed on to methods.
#'
#' @return An object of class `"duckdt_data_model"`: a list of three
#'   `data.table`s.
#'   \describe{
#'     \item{`tables`}{One row per table: `table` (the identity used
#'       everywhere else -- the bare name, or `"schema.name"` if that name
#'       occurs in more than one schema), `schema`, `name`, `type`,
#'       `n_rows`, `segment` (cluster to draw it in) and `display` (colour
#'       palette to draw it with).}
#'     \item{`columns`}{One row per column: `table`, `column`, `type`,
#'       `key` (0 = not a key, otherwise its position in the primary key),
#'       `ref` (referenced table, or `NA`), `ref_col` (referenced column).}
#'     \item{`references`}{One row per referencing column: `table`,
#'       `column`, `ref`, `ref_col`, `ref_id` (rows sharing an id form one
#'       compound reference) and `ref_col_num`.}
#'   }
#' @seealso [duckdt_erd()] to explore one in the browser,
#'   [duckdt_explorer()] for the Shiny version.
#' @examples
#' con <- duckdt_connect(quiet = TRUE)
#' DBI::dbExecute(con, "CREATE TABLE customers (id INTEGER PRIMARY KEY, name VARCHAR)")
#' DBI::dbExecute(con, "CREATE TABLE orders (
#'   id INTEGER PRIMARY KEY, customer_id INTEGER REFERENCES customers(id), total DOUBLE)")
#'
#' dm <- duckdt_data_model(con)
#' dm
#' dm$references
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' @export
duckdt_data_model <- function(x, ...) UseMethod("duckdt_data_model")

#' @rdname duckdt_data_model
#' @export
duckdt_data_model.duckdt <- function(x, ...) duckdt_data_model(x$conn, ...)

#' @rdname duckdt_data_model
#' @export
duckdt_data_model.default <- function(x, tables = NULL, infer_references = FALSE,
                                      row_counts = FALSE, ...) {
  conn <- duckdt_unwrap_conn(x)
  if (!duckdt_is_connection(conn)) {
    stop(
      "duckdt: don't know how to build a data model from an object of class ",
      paste(class(x), collapse = "/"),
      ". Pass a DBI connection, a duckdt handle, or a named list of data frames.",
      call. = FALSE
    )
  }

  tbl <- as.data.frame(duckdt_tables(conn), stringsAsFactors = FALSE)
  if (nrow(tbl) == 0) {
    stop("duckdt: no tables/views found on this connection.", call. = FALSE)
  }
  tbl$table <- duckdt_dm_table_ids(tbl$schema, tbl$name)

  if (!is.null(tables)) {
    keep <- tbl$table %in% tables | tbl$name %in% tables |
      paste0(tbl$schema, ".", tbl$name) %in% tables
    if (!any(keep)) {
      stop("duckdt: none of `tables` were found on this connection.", call. = FALSE)
    }
    tbl <- tbl[keep, , drop = FALSE]
  }

  tbl$n_rows <- NA_real_
  if (isTRUE(row_counts)) {
    for (i in seq_len(nrow(tbl))) {
      qname <- paste0(
        DBI::dbQuoteIdentifier(conn, tbl$schema[i]), ".",
        DBI::dbQuoteIdentifier(conn, tbl$name[i])
      )
      tbl$n_rows[i] <- tryCatch(
        as.numeric(DBI::dbGetQuery(conn, paste0("SELECT count(*) AS n FROM ", qname))$n[1]),
        error = function(e) NA_real_
      )
    }
  }

  cols <- as.data.frame(duckdt_schema(conn), stringsAsFactors = FALSE)
  cols$table_id <- duckdt_dm_table_ids(cols$schema, cols$table, ids = tbl)
  cols <- cols[!is.na(cols$table_id) & cols$table_id %in% tbl$table, , drop = FALSE]
  cols <- cols[order(match(cols$table_id, tbl$table), cols$ordinal_position), , drop = FALSE]

  columns <- data.frame(
    table = cols$table_id,
    column = cols$column,
    type = cols$type,
    key = 0L,
    ref = NA_character_,
    ref_col = NA_character_,
    stringsAsFactors = FALSE
  )
  for (t in unique(columns$table)) {
    idx <- which(columns$table == t & cols$primary_key)
    if (length(idx)) columns$key[idx] <- seq_along(idx)
  }

  fks <- as.data.frame(duckdt_relationships(conn), stringsAsFactors = FALSE)
  if (nrow(fks) > 0) {
    fk_id <- duckdt_dm_table_ids(fks$fk_schema, fks$fk_table, ids = tbl)
    pk_id <- duckdt_dm_table_ids(fks$pk_schema, fks$pk_table, ids = tbl)
    known <- !is.na(fk_id) & !is.na(pk_id) & fk_id %in% tbl$table & pk_id %in% tbl$table
    for (i in which(known)) {
      row <- columns$table == fk_id[i] & columns$column == fks$fk_column[i]
      columns$ref[row] <- pk_id[i]
      columns$ref_col[row] <- fks$pk_column[i]
    }
  }

  dm <- duckdt_dm_new(
    tables = data.frame(
      table = tbl$table, schema = tbl$schema, name = tbl$name, type = tbl$type,
      n_rows = tbl$n_rows, segment = NA_character_, display = NA_character_,
      stringsAsFactors = FALSE
    ),
    columns = columns
  )
  if (isTRUE(infer_references)) dm <- duckdt_dm_infer_references(dm)
  dm
}

#' @rdname duckdt_data_model
#' @export
duckdt_data_model.list <- function(x, ...) {
  if (!length(x)) stop("duckdt: empty list -- nothing to model.", call. = FALSE)
  if (!all(vapply(x, is.data.frame, logical(1)))) {
    stop("duckdt: every element of the list must be a data frame.", call. = FALSE)
  }
  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    stop(
      "duckdt: the list of data frames must be named -- names become table names.",
      call. = FALSE
    )
  }
  columns <- do.call(rbind, lapply(names(x), function(nm) {
    df <- x[[nm]]
    data.frame(
      table = nm,
      column = names(df),
      type = vapply(df, function(col) paste(class(col), collapse = ", "), character(1)),
      stringsAsFactors = FALSE
    )
  }))
  rownames(columns) <- NULL
  duckdt_data_model(columns, ...)
}

#' @rdname duckdt_data_model
#' @export
duckdt_data_model.data.frame <- function(x, ...) {
  if (!all(c("table", "column") %in% names(x))) {
    stop(
      "duckdt: a column-info data frame needs at least `table` and `column` columns. ",
      "To model a set of data frames instead, pass them as a named list.",
      call. = FALSE
    )
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (is.null(x$type)) x$type <- NA_character_
  if (is.null(x$key)) x$key <- 0L
  if (is.null(x$ref)) x$ref <- NA_character_
  if (is.null(x$ref_col)) x$ref_col <- NA_character_
  if (is.logical(x$key)) x$key <- as.integer(x$key)
  x$key[is.na(x$key)] <- 0L

  tabs <- unique(x$table)
  duckdt_dm_new(
    tables = data.frame(
      table = tabs, schema = NA_character_, name = tabs, type = "BASE TABLE",
      n_rows = NA_real_, segment = NA_character_, display = NA_character_,
      stringsAsFactors = FALSE
    ),
    columns = x[, c("table", "column", "type", "key", "ref", "ref_col")]
  )
}

#' Is this a duckdt data model?
#'
#' @param x Object to test.
#' @return `TRUE` or `FALSE`.
#' @export
is_duckdt_data_model <- function(x) inherits(x, "duckdt_data_model")

#' @export
print.duckdt_data_model <- function(x, n = 10L, ...) {
  cat(sprintf(
    "<duckdt data model> %d tables, %d columns, %d references\n",
    nrow(x$tables), nrow(x$columns),
    if (is.null(x$references)) 0L else length(unique(x$references$ref_id))
  ))
  tabs <- utils::head(as.data.frame(x$tables), n)
  cols <- as.data.frame(x$columns)
  for (i in seq_len(nrow(tabs))) {
    tab_cols <- cols[cols$table == tabs$table[i], , drop = FALSE]
    keys <- tab_cols$column[tab_cols$key > 0]
    n_ref <- sum(!is.na(tab_cols$ref))
    cat(sprintf(
      "  %-28s %3d cols%s%s%s\n",
      tabs$table[i], nrow(tab_cols),
      if (length(keys)) paste0(", PK: ", paste(keys, collapse = "+")) else "",
      if (n_ref) sprintf(", %d FK", n_ref) else "",
      if (is.na(tabs$n_rows[i])) "" else sprintf(", %s rows", format(tabs$n_rows[i], big.mark = ","))
    ))
  }
  if (nrow(x$tables) > n) cat(sprintf("  ... and %d more\n", nrow(x$tables) - n))
  invisible(x)
}

# ---- internals -------------------------------------------------------------

# A table's identity in the model: its bare name, or "schema.name" when that
# bare name is ambiguous across schemas. `ids` re-uses the mapping already
# computed for a model's `tables`, so columns/foreign keys agree with it.
duckdt_dm_table_ids <- function(schema, name, ids = NULL) {
  if (!is.null(ids)) {
    lookup <- stats::setNames(ids$table, paste0(ids$schema, "\r", ids$name))
    return(unname(lookup[paste0(schema, "\r", name)]))
  }
  dup <- duplicated(name) | duplicated(name, fromLast = TRUE)
  ifelse(dup & !is.na(schema), paste0(schema, ".", name), name)
}

duckdt_dm_new <- function(tables, columns) {
  columns$key[is.na(columns$key)] <- 0L
  structure(
    list(
      tables = data.table::setDT(as.data.frame(tables))[],
      columns = data.table::setDT(as.data.frame(columns))[],
      references = duckdt_dm_build_references(columns)
    ),
    class = "duckdt_data_model"
  )
}

# Rebuild $references from $columns' ref/ref_col -- called after any edit.
duckdt_dm_build_references <- function(columns) {
  empty <- data.frame(
    table = character(), column = character(), ref = character(),
    ref_col = character(), ref_id = integer(), ref_col_num = integer(),
    stringsAsFactors = FALSE
  )
  columns <- as.data.frame(columns, stringsAsFactors = FALSE)
  has_ref <- !is.na(columns$ref) & nzchar(columns$ref)
  if (!any(has_ref)) return(data.table::setDT(empty)[])

  refs <- columns[has_ref, c("table", "column", "ref", "ref_col"), drop = FALSE]
  rownames(refs) <- NULL

  # Primary key columns of every table, in key order -- used both to group
  # compound references and to fill in an omitted `ref_col`.
  keyed <- columns[columns$key > 0, , drop = FALSE]
  keyed <- keyed[order(keyed$key), , drop = FALSE]
  key_cols <- split(keyed$column, keyed$table)

  # Consecutive columns of one table pointing at the same table are chunked
  # into one reference, as wide as that table's primary key (datamodelr's
  # rule: two single-column FKs to the same table stay two references).
  refs$ref_id <- NA_integer_
  refs$ref_col_num <- NA_integer_
  id <- 0L
  i <- 1L
  while (i <= nrow(refs)) {
    width <- max(1L, length(key_cols[[refs$ref[i]]]))
    j <- i
    while (j < nrow(refs) && (j - i + 1L) < width &&
           refs$table[j + 1L] == refs$table[i] && refs$ref[j + 1L] == refs$ref[i]) {
      j <- j + 1L
    }
    id <- id + 1L
    refs$ref_id[i:j] <- id
    refs$ref_col_num[i:j] <- seq_len(j - i + 1L)
    i <- j + 1L
  }

  no_col <- is.na(refs$ref_col)
  if (any(no_col)) {
    refs$ref_col[no_col] <- vapply(which(no_col), function(k) {
      target <- key_cols[[refs$ref[k]]]
      if (length(target) >= refs$ref_col_num[k]) target[refs$ref_col_num[k]] else NA_character_
    }, character(1))
  }

  data.table::setDT(refs)[]
}

duckdt_dm_check <- function(dm) {
  if (!is_duckdt_data_model(dm)) {
    stop("duckdt: expected a data model object from duckdt_data_model().", call. = FALSE)
  }
  invisible(dm)
}

# DBI connections are S4, so they don't inherit from anything in S3 terms.
duckdt_is_connection <- function(x) methods::is(x, "DBIConnection")
