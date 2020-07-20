extract_target_column_names <- function(target, cache) {
  out <- target %>% drake::readd(character_only = TRUE, cache = cache)

  out %<>% names()

  out
}


#' Extract column names for targets in plan
#'
#' @inheritParams drake::drake_graph_info
#' @param colname_out the name given to the list-column containing each target's columns
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

#' Link Column Names to their Documentation
#' @export
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
