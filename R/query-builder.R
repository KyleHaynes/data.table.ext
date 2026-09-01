#' Build a SELECT statement from a data model
#'
#' Given the tables (and columns) you want, this works out how to join them
#' from the model's references and writes the SQL -- the same query the
#' [duckdt_erd()] page and [duckdt_explorer()] show you as you tick boxes,
#' available on its own so you can run or edit it.
#'
#' Tables are joined (with `LEFT JOIN`) along the references in the model,
#' starting from the first one you name. A table with no reference path to
#' the others can't be joined automatically: it is left out of the `FROM`
#' clause and reported in the `"unjoined"` attribute, rather than silently
#' becoming a cross join.
#'
#' @param dm A `"duckdt_data_model"` from [duckdt_data_model()], or anything
#'   [duckdt_data_model()] accepts.
#' @param tables Character vector of tables to select from. The first is the
#'   one everything else joins to.
#' @param columns Optionally, a named list mapping table name to the columns
#'   wanted from it. Tables not named contribute all their columns; a table
#'   mapped to `character(0)` contributes none.
#' @param where Optional SQL predicate, inserted as the `WHERE` clause
#'   verbatim.
#' @param limit Optional row limit.
#'
#' @return A length-1 character vector of SQL, with the tables that could not
#'   be joined in its `"unjoined"` attribute.
#' @examples
#' dm <- duckdt_data_model(list(
#'   orders = data.frame(id = 1L, customer_id = 1L, total = 1),
#'   customers = data.frame(id = 1L, name = "a")
#' ))
#' dm <- duckdt_dm_add_references(dm, orders$customer_id == customers$id)
#' cat(duckdt_dm_query(dm, c("orders", "customers"),
#'                     columns = list(orders = "total", customers = "name")))
#' @export
duckdt_dm_query <- function(dm, tables, columns = NULL, where = NULL, limit = NULL) {
  dm <- duckdt_as_data_model(dm)
  tabs <- as.data.frame(dm$tables)
  unknown <- setdiff(tables, tabs$table)
  if (length(unknown)) {
    stop(sprintf("duckdt: no table %s in this data model.",
      paste(sQuote(unknown), collapse = ", ")), call. = FALSE)
  }
  if (!length(tables)) stop("duckdt: name at least one table.", call. = FALSE)

  all_cols <- as.data.frame(dm$columns)
  picked <- lapply(tables, function(t) {
    have <- all_cols$column[all_cols$table == t]
    if (is.null(columns) || is.null(columns[[t]])) have else intersect(have, columns[[t]])
  })
  names(picked) <- tables

  alias <- duckdt_query_aliases(tabs[match(tables, tabs$table), , drop = FALSE])
  plan <- duckdt_join_plan(dm, tables)

  # Disambiguate a column name that several selected tables have.
  counts <- table(unlist(picked))
  select <- unlist(lapply(c(plan$order, plan$unjoined), function(t) {
    vapply(picked[[t]], function(col) {
      expr <- paste0(alias[[t]], ".", duckdt_quote(col))
      if (counts[[col]] > 1) {
        expr <- paste0(expr, " AS ",
          duckdt_quote(paste0(tabs$name[tabs$table == t], "_", col)))
      }
      paste0("  ", expr)
    }, character(1), USE.NAMES = FALSE)
  }))
  if (!length(select)) select <- "  *"

  sql <- paste0(
    "SELECT\n", paste(select, collapse = ",\n"),
    "\nFROM ", duckdt_qualified(tabs, plan$order[1]), " AS ", alias[[plan$order[1]]]
  )
  for (j in plan$joins) {
    on <- paste(sprintf(
      "%s.%s = %s.%s",
      alias[[j$link$table]], duckdt_quote(j$link$column),
      alias[[j$link$ref]], duckdt_quote(j$link$ref_col)
    ), collapse = "\n   AND ")
    sql <- paste0(
      sql, "\nLEFT JOIN ", duckdt_qualified(tabs, j$table), " AS ", alias[[j$table]],
      "\n  ON ", on
    )
  }
  if (!is.null(where) && nzchar(where)) sql <- paste0(sql, "\nWHERE ", where)
  if (!is.null(limit)) sql <- paste0(sql, "\nLIMIT ", as.integer(limit))

  attr(sql, "unjoined") <- plan$unjoined
  sql
}

# ---- internals -------------------------------------------------------------

duckdt_quote <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')

duckdt_qualified <- function(tabs, table) {
  i <- match(table, tabs$table)
  if (is.na(tabs$schema[i])) {
    duckdt_quote(tabs$name[i])
  } else {
    paste0(duckdt_quote(tabs$schema[i]), ".", duckdt_quote(tabs$name[i]))
  }
}

duckdt_query_aliases <- function(tabs) {
  used <- character()
  out <- list()
  for (i in seq_len(nrow(tabs))) {
    base <- tolower(substr(gsub("[^A-Za-z0-9]", "", tabs$name[i]), 1, 1))
    if (!nzchar(base)) base <- "t"
    alias <- base
    n <- 1L
    while (alias %in% used) {
      n <- n + 1L
      alias <- paste0(base, n)
    }
    used <- c(used, alias)
    out[[tabs$table[i]]] <- alias
  }
  out
}

# Order `tables` so each one after the first joins to a table already placed,
# and report the ones no reference reaches.
duckdt_join_plan <- function(dm, tables) {
  refs <- as.data.frame(dm$references)
  refs <- refs[refs$table %in% tables & refs$ref %in% tables, , drop = FALSE]
  # A reference with no known target column can be drawn but not joined on.
  incomplete <- unique(refs$ref_id[is.na(refs$ref_col)])
  refs <- refs[!refs$ref_id %in% incomplete, , drop = FALSE]

  placed <- tables[1]
  joins <- list()
  used <- rep(FALSE, length(unique(refs$ref_id)))
  names(used) <- unique(refs$ref_id)

  repeat {
    progressed <- FALSE
    for (id in names(used)[!used]) {
      link <- refs[refs$ref_id == id, , drop = FALSE]
      from_in <- link$table[1] %in% placed
      to_in <- link$ref[1] %in% placed
      if (from_in == to_in) next
      nxt <- if (from_in) link$ref[1] else link$table[1]
      placed <- c(placed, nxt)
      joins[[length(joins) + 1L]] <- list(table = nxt, link = link)
      used[[id]] <- TRUE
      progressed <- TRUE
    }
    if (!progressed) break
  }

  list(order = placed, joins = joins, unjoined = setdiff(tables, placed))
}
