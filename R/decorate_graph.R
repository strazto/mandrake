# Target Column Extraction and Decoration =====

extract_target_column_names <- function(target, cache) {
  out <- target %>% drake::readd(character_only = TRUE, cache = cache)

  out %<>% names()

  out
}


#' Extract Column Names for targets in Drake Plan
#'
#' Given a [drake::drake_plan()], extract the column names of each target, and store it
#' in a new column.
#'
#' @inheritParams drake::drake_graph_info
#' @param colname_out the name given to the list-column containing each target's columns
#' @family graph_decoration
#' @export
extract_column_names <- function(plan, cache, group = NULL, clusters = NULL,
                                 colname_out = "target_column_list", ...) {

  # Handle missingness for grouping data
  if (rlang::is_empty(group)) group <- "target"
  if (rlang::is_empty(clusters)) clusters <- plan %>% dplyr::pull(group) %>% unique()

  tmp_col = "temporary_join_col__"

  plan %<>%
    dplyr::mutate(
      !!tmp_col := dplyr::if_else(
        is.na(!!rlang::sym(group)),
        target,
        !!rlang::sym(group)
      ))

  out <- plan

  # Within the group, subset it, if it should cluster
  out %<>%
    dplyr::group_by(!!rlang::sym(tmp_col)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  out %<>%
    dplyr::select(target, !!group, !!tmp_col) %>%
    dplyr::mutate(!! colname_out := purrr::map(
      target, extract_target_column_names, cache = cache)
      ) %>%
    dplyr::select(!!tmp_col, !!colname_out)

  out %<>%
    dplyr::right_join(plan, by = tmp_col) %>%
    dplyr::select(-tmp_col)

  out
}

missing_col_placeholder <- function(col_key) {
  out <- tibble::tibble_row(
    name = col_key, aliases = list(NULL),
    html = list("<i>Column doc not found</i>"),
    html_ref = ""
  )
}


pull_out_coldocs <- function(columns, lookup_cache) {
  if (rlang::is_empty(columns)) return(NULL)
  out <- lookup_cache$mget(columns)

  which_missing <- attr(out, "missing")

  for (i in which_missing) {
    out[[i]] <- missing_col_placeholder(columns[[i]])
  }

  out %<>%
    dplyr::bind_rows()

  out %<>%
    dplyr::select(name, description = html, defined_at = html_ref) %>%
    dplyr::mutate(
      description = purrr::map_chr(description, ~paste0(., collapse = "<br>")),
      description = glue::glue(
        "{description}",
        "{discuss_found} {defined_at}",
        discuss_found = dplyr::if_else(
          stringr::str_length(defined_at) > 0,
          "Found at",
          ""),
        .sep = "<br>"
      )) %>%
    dplyr::select(-defined_at)
  out
}

#' Link Column Names to their Documentation.
#'
#' Given a list-column containing column-names for targets (As part of a
#' `drake::drake_plan()` having been processed by `extract_column_names()`),
#' cross reference these columns with the `lookup_cache`, and produce html tables
#' for each set of columns.
#'
#' @return a character vector, of the same length as the outer dimension of
#'         the input list, each element being a html table describing each
#'         column.
#' @export
#' @family graph_decoration
#' @param target_column_list a list of character vectors specifying column names
#' @param lookup_cache todo:doc me
link_col2doc <- function(target_column_list, lookup_cache) {
  if (!is(lookup_cache, "storr")) stop("Must pass a storr object to link_col2doc")

  out <- target_column_list %>%
    purrr::map(
      pull_out_coldocs, lookup_cache = lookup_cache
    )
  out %<>%
    purrr::map_chr(
      ~knitr::kable(
        .,
        format = "html",
        escape = FALSE) %>%
        kableExtra::kable_styling(c("striped", "responsive", "condensed"))
    )

  out
}

# Embedded Docstring Parsing ====

enrich_single_docstring <- function(x) {
  x %<>%
    stringr::str_trim() %>%
    roxygen2:::markdown() %>%
    pkgdown::rd2html() %>%
    paste(sep = "\n", collapse = "\n")
  x
}

enrich_docstrings <- function(docstrings) {
  docstrings %<>% purrr::map_chr(enrich_single_docstring)
  docstrings
}


# Command Highlighting ============

highlight_single_command <- function(x) {
  x %<>%
    rlang::expr_deparse(width = Inf) %>%
    paste(sep = "\n", collapse = "\n") %>%
    downlit::highlight(pre_class = "downlit")
  x
}

highlight_commands <- function(commands) {
  commands %<>% purrr::map_chr(highlight_single_command)
  commands
}

# Plan Decoration ==============
#' @export
decorate_plan <- function(
  plan, cache, group = NULL, clusters = NULL,
  desc_colname = "desc", colname_out = desc_colname,
  lookup_cache = NULL,
  ...) {
  sym <- rlang::sym

  plan %<>%
    dplyr::mutate(
      !!colname_out := enrich_docstrings(!!sym(desc_colname)),
      !!colname_out := glue::glue(
        "<h1>{target}</h1>",
        "{output_column}",
        "<h2>Command</h2>",
        "<details><summary>Command</summary>",
        "{highlight_commands(command)}",
        "</details>",
        "<h2>Columns></h2>",
        "{cols_extracted}",
        cols_extracted = {
          .data %>%
          extract_column_names(
          cache,
          group = group, clusters = clusters,
          colname_out = "gimme") %>%
          .$gimme %>% link_col2doc(lookup_cache)
          },
        output_column = !!sym(colname_out),
        .sep = "\n"
      )
    )

  plan
}
