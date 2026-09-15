#' Mark columns as a table's primary key
#'
#' Databases that declare their primary keys already have them in the model;
#' this is for the ones that don't (a DuckDB file loaded from CSV/Parquet,
#' say), and for models built from plain data frames.
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()].
#' @param table Table name.
#' @param column Column name(s). For a compound key, pass them in key order.
#'
#' @return The data model, with the key set (and `$references` rebuilt, since
#'   a reference with no explicit `ref_col` points at the key).
#' @examples
#' dm <- duckdt_data_model(list(people = data.frame(person_id = 1L, name = "a")))
#' dm <- duckdt_dm_set_key(dm, "people", "person_id")
#' dm$columns
#' @export
duckdt_dm_set_key <- function(dm, table, column) {
  duckdt_dm_check(dm)
  cols <- as.data.frame(dm$columns)
  hit <- cols$table == table & cols$column %in% column
  if (!any(hit)) {
    stop(sprintf("duckdt: no column %s in table '%s'.",
      paste(sQuote(column), collapse = "/"), table), call. = FALSE)
  }
  cols$key[cols$table == table] <- 0L
  cols$key[hit] <- match(cols$column[hit], column)
  duckdt_dm_rebuild(dm, cols)
}

#' Add a reference (foreign key) to a data model
#'
#' DuckDB databases built by loading files rarely declare foreign keys, so
#' [duckdt_data_model()] often comes back with nothing to draw between the
#' boxes. These two add them after the fact:
#' `duckdt_dm_add_references()` takes the readable
#' `table$column == other_table$other_column` form (a port of datamodelr's
#' `dm_add_references()`), and `duckdt_dm_add_reference()` takes plain
#' strings, for when the names are in variables.
#'
#' Adding a reference also marks the referenced column as a key, since that
#' is what being referenced means.
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()].
#' @param ... One or more expressions of the form
#'   `table$column == ref_table$ref_column`. Nothing is evaluated -- the
#'   names are read off the expression -- so the tables need not exist as R
#'   objects.
#' @param table,column The referencing table and column(s).
#' @param ref,ref_col The referenced table and column(s). `ref_col` defaults
#'   to `ref`'s primary key.
#'
#' @return The data model, with the reference(s) added.
#' @seealso [duckdt_dm_infer_references()] to guess them from column names.
#' @examples
#' dm <- duckdt_data_model(list(
#'   orders = data.frame(id = 1L, customer_id = 1L),
#'   customers = data.frame(id = 1L, name = "a")
#' ))
#' dm <- duckdt_dm_add_references(dm, orders$customer_id == customers$id)
#' dm$references
#' @export
duckdt_dm_add_references <- function(dm, ...) {
  duckdt_dm_check(dm)
  exprs <- as.list(substitute(list(...)))[-1]
  for (e in exprs) {
    parts <- duckdt_dm_parse_ref(e)
    dm <- duckdt_dm_add_reference(
      dm, parts$table, parts$column, parts$ref, parts$ref_col
    )
  }
  dm
}

#' @rdname duckdt_dm_add_references
#' @export
duckdt_dm_add_reference <- function(dm, table, column, ref, ref_col = NULL) {
  duckdt_dm_check(dm)
  cols <- as.data.frame(dm$columns)
  if (!table %in% cols$table) {
    stop(sprintf("duckdt: no table '%s' in this data model.", table), call. = FALSE)
  }
  if (!ref %in% cols$table) {
    stop(sprintf("duckdt: no referenced table '%s' in this data model.", ref), call. = FALSE)
  }
  missing_cols <- setdiff(column, cols$column[cols$table == table])
  if (length(missing_cols)) {
    stop(sprintf("duckdt: table '%s' has no column %s.", table,
      paste(sQuote(missing_cols), collapse = ", ")), call. = FALSE)
  }
  if (!is.null(ref_col)) {
    missing_ref <- setdiff(ref_col, cols$column[cols$table == ref])
    if (length(missing_ref)) {
      stop(sprintf("duckdt: referenced table '%s' has no column %s.", ref,
        paste(sQuote(missing_ref), collapse = ", ")), call. = FALSE)
    }
    if (length(ref_col) != length(column)) {
      stop("duckdt: `column` and `ref_col` must be the same length.", call. = FALSE)
    }
  }

  hit <- cols$table == table & cols$column %in% column
  cols$ref[hit] <- ref
  cols$ref_col[hit] <- if (is.null(ref_col)) NA_character_ else ref_col[match(cols$column[hit], column)]

  if (!is.null(ref_col)) {
    # Being referenced makes it a key, if it wasn't one already.
    if (!any(cols$key[cols$table == ref] > 0)) {
      key_rows <- cols$table == ref & cols$column %in% ref_col
      cols$key[key_rows] <- match(cols$column[key_rows], ref_col)
    }
  }
  duckdt_dm_rebuild(dm, cols)
}

#' Guess references from column naming conventions
#'
#' Most DuckDB databases don't declare foreign keys, so
#' [duckdt_data_model()] has nothing to draw between the boxes. This fills
#' that gap the way a human reading the schema would: a column is treated as
#' referencing table `t` when its name is `t`'s primary key (`customer_id`
#' pointing at `customers.customer_id`), or the table name glued to that key
#' (`customer_id` or `customerid` pointing at `customers.id`), with a naive
#' plural/singular fallback so `customers` and `customer` both match.
#'
#' It is a guess, deliberately a conservative one. Columns that already have
#' a reference are left alone, a column that could point at more than one
#' table is skipped as ambiguous, a bare `id` is never treated as pointing
#' anywhere, and candidate pairs whose types are from different families
#' (integer vs. text, say) are rejected. Check the result -- print the model,
#' or look at `dm$references` -- rather than assuming it got everything
#' right, and state anything it missed with [duckdt_dm_add_references()].
#'
#' Tables need primary keys for this to have anything to aim at. If the
#' database declares none, set them first with [duckdt_dm_set_key()].
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()].
#' @param quiet Suppress the message summarising what was inferred.
#'
#' @return The data model, with inferred references added. The columns it
#'   added are recorded in the `"inferred"` attribute of `$references`.
#' @seealso [duckdt_dm_add_references()] to state references exactly.
#' @examples
#' dm <- duckdt_data_model(list(
#'   customers = data.frame(customer_id = 1L, name = "a"),
#'   orders = data.frame(order_id = 1L, customer_id = 1L)
#' ))
#' dm <- duckdt_dm_set_key(dm, "customers", "customer_id")
#' dm <- duckdt_dm_set_key(dm, "orders", "order_id")
#' dm <- duckdt_dm_infer_references(dm, quiet = TRUE)
#' dm$references
#' @export
duckdt_dm_infer_references <- function(dm, quiet = FALSE) {
  duckdt_dm_check(dm)
  cols <- as.data.frame(dm$columns)

  # What each table can be pointed at by: its single-column primary key.
  keyed <- cols[cols$key == 1L, , drop = FALSE]
  single_key <- keyed[!keyed$table %in% cols$table[cols$key > 1L], , drop = FALSE]
  if (!nrow(single_key)) {
    if (!quiet) {
      cli::cli_inform(c(
        "!" = "No single-column primary keys to reference -- nothing inferred.",
        "i" = "Set them with {.fn duckdt_dm_set_key} first."
      ))
    }
    return(dm)
  }

  candidates <- lapply(seq_len(nrow(single_key)), function(i) {
    tab <- single_key$table[i]
    key <- single_key$column[i]
    bare <- sub("^.*\\.", "", tab)
    stems <- unique(c(bare, duckdt_singular(bare)))
    list(
      table = tab, column = key, type = single_key$type[i],
      names = unique(tolower(c(key, paste0(stems, "_", key), paste0(stems, key))))
    )
  })

  added <- character()
  for (i in seq_len(nrow(cols))) {
    if (!is.na(cols$ref[i])) next
    name <- tolower(cols$column[i])
    if (name == "id") next
    hits <- Filter(function(cand) {
      cand$table != cols$table[i] &&
        name %in% cand$names &&
        duckdt_type_family(cols$type[i]) == duckdt_type_family(cand$type)
    }, candidates)
    if (length(hits) != 1L) next
    cols$ref[i] <- hits[[1]]$table
    cols$ref_col[i] <- hits[[1]]$column
    added <- c(added, sprintf("%s$%s -> %s$%s",
      cols$table[i], cols$column[i], hits[[1]]$table, hits[[1]]$column))
  }

  if (!quiet) {
    if (length(added)) {
      cli::cli_inform(c(
        "v" = "Inferred {length(added)} reference{?s} from column names:",
        stats::setNames(added, rep(" ", length(added))),
        "i" = "These are guesses -- state exact ones with {.fn duckdt_dm_add_references}."
      ))
    } else {
      cli::cli_inform(c("i" = "No references could be inferred from column names."))
    }
  }

  dm <- duckdt_dm_rebuild(dm, cols)
  attr(dm$references, "inferred") <- added
  dm
}

#' Group tables into segments
#'
#' Segments are drawn as labelled clusters in [duckdt_dm_dot()] /
#' [duckdt_dm_render()], and as colour-coded groups in [duckdt_erd()] --
#' useful for saying "these six tables are the ordering side of the schema
#' and those four are reference data".
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()].
#' @param segments A named list: names are segment names, values are
#'   character vectors of table names.
#'
#' @return The data model, with `$tables$segment` set.
#' @examples
#' dm <- duckdt_data_model(list(a = data.frame(x = 1), b = data.frame(y = 2)))
#' dm <- duckdt_dm_set_segment(dm, list(core = "a", lookup = "b"))
#' dm$tables
#' @export
duckdt_dm_set_segment <- function(dm, segments) {
  duckdt_dm_check(dm)
  tabs <- as.data.frame(dm$tables)
  for (s in names(segments)) {
    tabs$segment[tabs$table %in% segments[[s]]] <- s
  }
  dm$tables <- data.table::setDT(tabs)[]
  dm
}

#' Set how tables are displayed
#'
#' Each table can be given a colour palette name (see
#' [duckdt_dm_color_scheme()] for the built-in ones: `"accent1"` ...
#' `"accent7"`, and their border-less `"accent1nb"` ... variants), or the
#' special value `"hide"` to leave it out of diagrams entirely.
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()].
#' @param display A named list: names are palette names (or `"hide"`),
#'   values are character vectors of table names.
#'
#' @return The data model, with `$tables$display` set.
#' @examples
#' dm <- duckdt_data_model(list(a = data.frame(x = 1), b = data.frame(y = 2)))
#' dm <- duckdt_dm_set_display(dm, list(accent1 = "a", hide = "b"))
#' dm$tables
#' @export
duckdt_dm_set_display <- function(dm, display) {
  duckdt_dm_check(dm)
  tabs <- as.data.frame(dm$tables)
  for (s in names(display)) {
    tabs$display[tabs$table %in% display[[s]]] <- s
  }
  dm$tables <- data.table::setDT(tabs)[]
  dm
}

#' Zoom a data model in on some tables
#'
#' Big schemas are unreadable all at once. This keeps only the tables you
#' name -- plus, with `depth`, whatever they connect to within that many
#' reference hops, which is usually what you actually wanted.
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()].
#' @param tables Character vector of table names to keep.
#' @param depth Also keep tables within this many reference hops of
#'   `tables`. Default `0` (just the named tables).
#' @param columns Optionally, a named list of character vectors -- names are
#'   table names, values the columns to keep for that table. Tables not named
#'   keep all their columns.
#'
#' @return A data model containing only the kept tables/columns, and only the
#'   references between them.
#' @examples
#' dm <- duckdt_data_model(list(
#'   orders = data.frame(id = 1L, customer_id = 1L),
#'   customers = data.frame(id = 1L, name = "a"),
#'   unrelated = data.frame(z = 1)
#' ))
#' dm <- duckdt_dm_add_references(dm, orders$customer_id == customers$id)
#' duckdt_dm_filter(dm, "orders", depth = 1)
#' @export
duckdt_dm_filter <- function(dm, tables, depth = 0L, columns = NULL) {
  duckdt_dm_check(dm)
  all_tables <- as.data.frame(dm$tables)$table
  unknown <- setdiff(tables, all_tables)
  if (length(unknown)) {
    stop(sprintf("duckdt: no table %s in this data model.",
      paste(sQuote(unknown), collapse = ", ")), call. = FALSE)
  }

  keep <- tables
  refs <- as.data.frame(dm$references)
  if (depth > 0 && nrow(refs)) {
    for (i in seq_len(depth)) {
      neighbours <- c(
        refs$ref[refs$table %in% keep],
        refs$table[refs$ref %in% keep]
      )
      keep <- union(keep, neighbours)
    }
  }

  tabs <- as.data.frame(dm$tables)
  cols <- as.data.frame(dm$columns)
  tabs <- tabs[tabs$table %in% keep, , drop = FALSE]
  cols <- cols[cols$table %in% keep, , drop = FALSE]

  if (!is.null(columns)) {
    drop <- rep(FALSE, nrow(cols))
    for (tab in names(columns)) {
      drop <- drop | (cols$table == tab & !cols$column %in% columns[[tab]])
    }
    cols <- cols[!drop, , drop = FALSE]
  }

  # A reference whose other end is gone can't be drawn.
  gone <- !is.na(cols$ref) & !cols$ref %in% keep
  cols$ref[gone] <- NA_character_
  cols$ref_col[gone] <- NA_character_

  duckdt_dm_new(tabs, cols)
}

# ---- internals -------------------------------------------------------------

duckdt_dm_rebuild <- function(dm, columns) {
  dm$columns <- data.table::setDT(columns)[]
  dm$references <- duckdt_dm_build_references(columns)
  dm
}

# `table$column == ref_table$ref_column` -> the four names, unevaluated.
duckdt_dm_parse_ref <- function(e) {
  bad <- function() {
    stop("duckdt: references are written as `table$column == ref_table$ref_column`.",
      call. = FALSE)
  }
  e <- as.list(e)
  if (length(e) != 3L || !identical(as.character(e[[1]]), "==")) bad()
  side <- function(s) {
    s <- as.list(s)
    if (length(s) != 3L || !identical(as.character(s[[1]]), "$")) bad()
    c(as.character(s[[2]]), as.character(s[[3]]))
  }
  lhs <- side(e[[2]])
  rhs <- side(e[[3]])
  list(table = lhs[1], column = lhs[2], ref = rhs[1], ref_col = rhs[2])
}

# Naive de-pluralisation, only ever used to match a table name against a
# column name -- "customers" -> "customer", "addresses" -> "address".
duckdt_singular <- function(x) {
  x <- sub("ies$", "y", x)
  x <- sub("([sxz]|ch|sh)es$", "\\1", x)
  sub("([^s])s$", "\\1", x)
}

# Coarse type families, so an inferred reference doesn't join an integer id
# to a free-text column. Covers DuckDB's and SQL Server's spellings, plus the
# R classes used by models built from data frames.
duckdt_type_family <- function(type) {
  if (is.na(type)) return("unknown")
  t <- tolower(type)
  if (grepl("int|serial|^bigint|hugeint", t)) "integer"
  else if (grepl("char|text|string|uuid|factor", t)) "text"
  else if (grepl("dec|numeric|real|double|float|money", t)) "numeric"
  else if (grepl("date|time", t)) "datetime"
  else if (grepl("bool|logical|bit$", t)) "boolean"
  else "other"
}
