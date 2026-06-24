.dt_print_mask_state <- new.env(parent = emptyenv())

.default_dt_class_colors <- function() {
    c(
        "<num>" = "col_blue",
        "<int>" = "col_blue",
        "<i64>" = "col_br_blue",
        "<char>" = "col_green",
        "<fctr>" = "col_magenta",
        "<Date>" = "col_yellow",
        "<POSct>" = "col_yellow",
        "<POSc>" = "col_yellow",
        "<IDat>" = "col_br_yellow",
        "<lgcl>" = "col_cyan",
        "<list>" = "col_br_white",
        "<raw>" = "col_red",
        "<cplx>" = "col_br_magenta",
        "<expr>" = "col_br_cyan",
        "<ord>" = "col_br_magenta"
    )
}

.resolve_cli_color_fun <- function(spec) {
    if (is.function(spec)) {
        return(spec)
    }
    if (!is.character(spec) || length(spec) != 1L || is.na(spec)) {
        return(NULL)
    }

    fun_name <- spec
    if (!grepl("^col_", fun_name)) {
        fun_name <- paste0("col_", fun_name)
    }

    if (!exists(fun_name, envir = asNamespace("cli"), mode = "function", inherits = FALSE)) {
        return(NULL)
    }
    get(fun_name, envir = asNamespace("cli"), inherits = FALSE)
}

.register_print_data_table_s3 <- function(method) {
    register <- get("registerS3method", envir = asNamespace("base"), inherits = FALSE)
    register("print", "data.table", method, envir = asNamespace("base"))
}

.sample_dt_color_default <- function() {
    getOption("foam.sample_dt.color", TRUE)
}

.coverage_sort_score <- function(x, exclude_cols = NULL) {
    if (!nrow(x)) {
        return(integer(0L))
    }
    cols <- setdiff(names(x), exclude_cols)
    if (!length(cols)) {
        return(integer(nrow(x)))
    }

    score <- numeric(nrow(x))
    for (col in cols) {
        display_v <- .display_value_strings(x[[col]])
        if (all(is.na(display_v))) {
            next
        }
        
        # Count frequency of each value in this column
        freq_table <- table(display_v)
        freq_map <- as.numeric(freq_table[display_v])
        freq_map[is.na(display_v)] <- 0
        
        # Add frequency-weighted score for this column
        score <- score + freq_map
    }
    score
}

.count_class_tokens <- function(line) {
    m <- gregexpr("<[^>]+>", line, perl = TRUE)[[1L]]
    if (length(m) == 1L && m[1L] == -1L) 0L else length(m)
}

.strip_ansi_codes <- function(x) {
    gsub("\033\\[[0-9;]*m", "", x, perl = TRUE)
}

#' Identify a data.table class/type row (for example `<num>  <fctr>`).
#' Unlike a plain token count, this also matches a trailing column block
#' that has only a single column (and so only one `<...>` token), by
#' requiring that nothing besides whitespace remains once tokens (and any
#' ANSI color codes already applied around them) are removed.
.is_class_line <- function(line) {
    if (!is.character(line) || length(line) != 1L || is.na(line)) {
        return(FALSE)
    }
    plain <- .strip_ansi_codes(line)
    if (.count_class_tokens(plain) < 1L) {
        return(FALSE)
    }
    !nzchar(trimws(gsub("<[^>]+>", "", plain, perl = TRUE)))
}

#' Drop the repeated class/type row (for example `<num>`) that data.table
#' prints under the column-name row of every wrapped column block, keeping
#' only the one under the first block's header.
.strip_repeated_class_rows <- function(lines) {
    if (!length(lines)) {
        return(lines)
    }
    is_class <- vapply(lines, .is_class_line, logical(1L))
    if (sum(is_class) <= 1L) {
        return(lines)
    }
    drop_idx <- which(is_class)[-1L]
    lines[-drop_idx]
}

.as_group_label <- function(x) {
    if (length(x) == 0L || is.null(x) || is.na(x)) {
        return("<NA>")
    }
    if (is.factor(x)) {
        return(as.character(x))
    }
    as.character(x)
}

.default_similarity_palette <- function() {
    c(
        "col_blue",
        "col_green",
        "col_magenta",
        "col_cyan",
        "col_yellow",
        "col_br_blue",
        "col_br_green",
        "col_br_magenta",
        "col_br_cyan",
        "col_br_yellow"
    )
}

.default_ansi256_palette <- function() {
    cols <- setdiff(16:231, c(16:21, 52:57, 232:255))
    cols
}

.ansi_colorize_256 <- function(text, color_code) {
    if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(text)) {
        return(text)
    }
    sprintf("\033[38;5;%dm%s\033[39m", as.integer(color_code), text)
}

.display_value_strings <- function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXlt", "difftime"))) {
        return(format(x))
    }
    if (is.numeric(x) || is.integer(x) || is.logical(x)) {
        return(format(x, scientific = FALSE, trim = TRUE, justify = "none"))
    }
    if (is.factor(x) || is.character(x)) {
        return(as.character(x))
    }
    if (is.complex(x)) {
        return(format(x))
    }
    rep(NA_character_, length(x))
}

.detect_column_spans <- function(header_line, col_names) {
    if (!length(col_names) || !is.character(header_line) || length(header_line) != 1L) {
        return(NULL)
    }

    starts <- integer(length(col_names))
    search_from <- 1L
    for (i in seq_along(col_names)) {
        pos <- regexpr(col_names[[i]], substr(header_line, search_from, nchar(header_line)), fixed = TRUE)[1L]
        if (pos < 1L) {
            return(NULL)
        }
        starts[[i]] <- search_from + pos - 1L
        search_from <- starts[[i]] + nchar(col_names[[i]])
    }

    ends <- c(starts[-1L] - 1L, nchar(header_line))
    data.frame(col = col_names, start = starts, end = ends, stringsAsFactors = FALSE)
}

.normalize_similarity_value <- function(x) {
    x <- tolower(x)
    gsub("[^a-z0-9]", "", x)
}

.similarity_clusters <- function(values, max_distance = 2L, max_relative = 0.30) {
    n <- length(values)
    if (n == 0L) {
        return(integer(0L))
    }
    if (n == 1L) {
        return(1L)
    }

    parent <- seq_len(n)
    find_root <- function(i) {
        while (parent[i] != i) {
            parent[i] <<- parent[parent[i]]
            i <- parent[i]
        }
        i
    }
    union_root <- function(i, j) {
        ri <- find_root(i)
        rj <- find_root(j)
        if (ri != rj) {
            parent[rj] <<- ri
        }
    }

    for (i in seq_len(n - 1L)) {
        for (j in (i + 1L):n) {
            d <- adist(values[[i]], values[[j]], partial = FALSE, ignore.case = TRUE)[1L]
            denom <- max(nchar(values[[i]], type = "chars"), nchar(values[[j]], type = "chars"), 1L)
            rel <- d / denom
            if (d <= max_distance || rel <= max_relative) {
                union_root(i, j)
            }
        }
    }

    roots <- vapply(seq_len(n), find_root, integer(1L))
    as.integer(match(roots, unique(roots)))
}

.build_group_similarity_maps <- function(
    x,
    group_col,
    palette,
    mode = c("similarity", "distinct"),
    max_distance = 2L,
    max_relative = 0.30
) {
    mode <- match.arg(mode)
    if (!nrow(x)) {
        return(list())
    }
    has_group_col <- is.character(group_col) && length(group_col) == 1L && (group_col %in% names(x))

    if (!length(palette)) {
        return(list())
    }

    color_cols <- names(x)
    if (!length(color_cols)) {
        return(list())
    }

    group_vals <- if (isTRUE(has_group_col)) as.character(x[[group_col]]) else rep("__all__", nrow(x))
    group_vals[is.na(group_vals)] <- "<NA>"
    split_idx <- split(seq_len(nrow(x)), group_vals)

    out <- vector("list", length(split_idx))
    names(out) <- names(split_idx)

    for (g in names(split_idx)) {
        idx <- split_idx[[g]]
        col_maps <- list()

        for (col in color_cols) {
            v <- x[[col]][idx]
            display_v <- .display_value_strings(v)
            if (all(is.na(display_v))) {
                next
            }
            keep <- which(!is.na(display_v) & nzchar(display_v))
            if (!length(keep)) {
                next
            }

            uniq_vals <- unique(display_v[keep])
            if (identical(mode, "similarity")) {
                norm_vals <- .normalize_similarity_value(uniq_vals)
                cluster_id <- .similarity_clusters(norm_vals, max_distance = max_distance, max_relative = max_relative)
            } else {
                cluster_id <- seq_along(uniq_vals)
            }
            color_id <- ((cluster_id - 1L) %% length(palette)) + 1L

            col_maps[[col]] <- list(
                values = uniq_vals,
                colors = unname(palette[color_id])
            )
        }

        out[[g]] <- col_maps
    }

    out
}

.split_print_header_tokens <- function(line) {
    if (!is.character(line) || length(line) != 1L || is.na(line)) {
        return(character(0L))
    }
    toks <- regmatches(line, gregexpr("\\S+", line, perl = TRUE))[[1L]]
    if (length(toks) == 1L && toks[1L] == -1L) {
        return(character(0L))
    }
    gsub('^"|"$', "", toks)
}

#' For each printed row line, determine which columns (in left-to-right
#' order) appear on that physical line. data.table wraps wide tables into
#' multiple side-by-side column blocks, each repeating its own header/class
#' line and the full set of rows, so a single global column order cannot be
#' assumed when coloring values per line.
.dt_print_line_columns <- function(lines) {
    n <- length(lines)
    out <- vector("list", n)
    current_cols <- NULL
    for (i in seq_len(n)) {
        line <- lines[[i]]
        if (.is_class_line(line) && i > 1L) {
            current_cols <- .split_print_header_tokens(lines[[i - 1L]])
            next
        }
        if (isTRUE(.format_dt_row_index(line)$is_row)) {
            out[[i]] <- current_cols
        }
    }
    out
}

.colorize_group_value_rows <- function(lines, x, group_col, similarity_maps, line_columns = NULL) {
    if (!length(lines) || !nrow(x) || !length(similarity_maps)) {
        return(lines)
    }
    has_group_col <- is.character(group_col) && length(group_col) == 1L && (group_col %in% names(x))

    out <- lines
    group_vals <- if (isTRUE(has_group_col)) as.character(x[[group_col]]) else rep("__all__", nrow(x))
    group_vals[is.na(group_vals)] <- "<NA>"
    all_cols <- names(x)

    for (i in seq_along(out)) {
        p <- .format_dt_row_index(out[[i]])
        if (!isTRUE(p$is_row)) {
            next
        }

        rn <- suppressWarnings(as.integer(gsub(",", "", p$raw_label, fixed = TRUE)))
        if (is.na(rn) || rn < 1L || rn > nrow(x)) {
            next
        }

        g <- group_vals[[rn]]
        g_map <- similarity_maps[[g]]
        if (is.null(g_map) || !length(g_map)) {
            next
        }

        line <- out[[i]]
        row_sep <- regexpr(":", line, fixed = TRUE)[1L]
        if (row_sep < 1L) {
            next
        }
        cursor <- row_sep + 1L
        line_cols <- if (!is.null(line_columns) && i <= length(line_columns)) line_columns[[i]] else NULL
        block_cols <- if (length(line_cols)) line_cols else all_cols
        for (col in block_cols) {
            if (!(col %in% names(g_map))) {
                next
            }
            raw_val <- .display_value_strings(x[[col]])[[rn]]
            if (is.null(raw_val) || is.na(raw_val) || !nzchar(raw_val)) {
                next
            }

            vinfo <- g_map[[col]]
            pos <- match(raw_val, vinfo$values)
            if (is.na(pos)) {
                next
            }

            search_line <- substr(line, cursor, nchar(line))
            found <- regexpr(raw_val, search_line, fixed = TRUE)[1L]
            if (found < 1L) {
                next
            }

            start <- cursor + found - 1L
            end <- start + nchar(raw_val, type = "chars") - 1L
            colored <- .ansi_colorize_256(raw_val, vinfo$colors[[pos]])
            line <- paste0(substr(line, 1L, start - 1L), colored, substr(line, end + 1L, nchar(line)))
            cursor <- start + nchar(colored, type = "chars")
        }
        out[[i]] <- line
    }

    out
}

.colorize_dt_class_rows <- function(lines, class_colors) {
    if (!length(lines)) {
        return(lines)
    }

    is_class_line <- vapply(lines, .is_class_line, logical(1L))
    if (!any(is_class_line)) {
        return(lines)
    }

    out <- lines
    idx <- which(is_class_line)
    for (i in idx) {
        line <- out[i]
        toks <- regmatches(line, gregexpr("<[^>]+>", line, perl = TRUE))[[1L]]
        if (length(toks) == 1L && toks[1L] == -1L) {
            next
        }
        for (tok in unique(toks)) {
            spec <- unname(class_colors[names(class_colors) %in% tok][1L])
            if (is.null(spec)) {
                next
            }
            color_fun <- .resolve_cli_color_fun(spec)
            if (is.null(color_fun)) {
                next
            }
            line <- gsub(tok, color_fun(tok), line, fixed = TRUE)
        }
        out[i] <- line
    }
    out
}

.colorize_group_headers <- function(lines) {
    if (!length(lines)) {
        return(lines)
    }
    out <- lines
    # Matches the default sep_fmt pattern; custom formats that omit "Group:" won't be colored.
    idx <- which(grepl("^\\s*-{3,}\\s*Group:\\s", out, perl = TRUE))
    if (!length(idx)) {
        return(out)
    }

    col_fun <- get("col_br_cyan", envir = asNamespace("cli"), inherits = FALSE)
    out[idx] <- vapply(out[idx], col_fun, character(1L))
    out
}

.colorize_duplicate_headers <- function(lines, col_names) {
    if (!length(lines) || !length(col_names)) {
        return(lines)
    }

    dups <- unique(col_names[duplicated(col_names) | duplicated(col_names, fromLast = TRUE)])
    dups <- dups[nzchar(dups)]
    if (!length(dups)) {
        return(lines)
    }

    red <- get("col_red", envir = asNamespace("cli"), inherits = FALSE)
    out <- lines
    dups_display <- unique(c(dups, paste0('"', dups, '"')))

    for (i in seq_along(out)) {
        line <- out[i]

        is_data_row <- isTRUE(.format_dt_row_index(line)$is_row)
        if (
            is_data_row ||
            grepl("^\\s*---\\s*$", line, perl = TRUE) ||
            grepl("<[^>]+>", line, perl = TRUE) ||
            grepl("^\\s*(ncol:|Key:|Indices?:|---\\s*Group:)", line, perl = TRUE)
        ) {
            next
        }

        parts <- regmatches(line, gregexpr("\\s+|\\S+", line, perl = TRUE))[[1L]]
        if (!length(parts)) {
            next
        }

        is_token <- !grepl("^\\s+$", parts)
        if (!any(is_token)) {
            next
        }

        toks <- parts[is_token]
        toks <- vapply(
            toks,
            function(tok) {
                if (tok %in% dups_display) red(tok) else tok
            },
            character(1L)
        )
        parts[is_token] <- toks
        out[i] <- paste0(parts, collapse = "")
    }

    out
}

.format_dt_row_index <- function(line, big.mark = ",") {
    m <- regexec("^([[:space:]]*)([0-9][0-9,]*)(:.*)$", line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) != 4L) {
        return(list(is_row = FALSE, line = line, label = ""))
    }

    idx <- suppressWarnings(as.numeric(gsub(",", "", hit[3L], fixed = TRUE)))
    if (is.na(idx)) {
        return(list(is_row = FALSE, line = line, label = ""))
    }

    formatted <- prettyNum(
        idx,
        big.mark = big.mark,
        preserve.width = "none",
        scientific = FALSE,
        trim = TRUE
    )
    list(
        is_row = TRUE,
        line = line,
        raw_label = hit[3L],
        label = formatted,
        rest = hit[4L]
    )
}

.align_dt_row_indices <- function(lines, big.mark = ",") {
    parsed <- lapply(lines, .format_dt_row_index, big.mark = big.mark)
    is_row <- vapply(parsed, function(x) isTRUE(x$is_row), logical(1L))

    if (!any(is_row)) {
        return(lines)
    }

    old_max_width <- max(nchar(vapply(parsed[is_row], `[[`, character(1L), "raw_label"), type = "width"))
    max_width <- max(nchar(vapply(parsed[is_row], `[[`, character(1L), "label"), type = "width"))
    delta <- max_width - old_max_width
    out <- lines
    row_idx <- which(is_row)

    for (i in row_idx) {
        p <- parsed[[i]]
        pad <- max_width - nchar(p$label, type = "width")
        out[i] <- paste0(strrep(" ", pad), p$label, p$rest)
    }

    if (delta > 0L) {
        non_row_idx <- which(!is_row)
        shiftable <- non_row_idx[grepl("^[[:space:]]+", out[non_row_idx])]
        if (length(shiftable)) {
            out[shiftable] <- paste0(strrep(" ", delta), out[shiftable])
        }
    }

    out
}

.insert_group_separators <- function(lines, x, group_col, sep_fmt = "--------- Group: %s") {
    if (!length(lines) || !is.character(group_col) || length(group_col) != 1L) {
        return(lines)
    }
    if (!(group_col %in% names(x))) {
        return(lines)
    }

    vals <- x[[group_col]]
    out <- character(0L)
    prev_group <- NULL
    seen_rows <- integer(0L)

    for (line in lines) {
        p <- .format_dt_row_index(line)
        if (!isTRUE(p$is_row)) {
            out <- c(out, line)
            next
        }

        rn <- suppressWarnings(as.integer(gsub(",", "", p$raw_label, fixed = TRUE)))
        if (is.na(rn) || rn < 1L || rn > length(vals)) {
            out <- c(out, line)
            next
        }

        this_group <- .as_group_label(vals[[rn]])
        if (!(rn %in% seen_rows) && (is.null(prev_group) || !identical(this_group, prev_group))) {
            out <- c(out, sprintf(sep_fmt, this_group))
        }
        out <- c(out, line)
        prev_group <- this_group
        if (!(rn %in% seen_rows)) {
            seen_rows <- c(seen_rows, rn)
        }
    }

    out
}
