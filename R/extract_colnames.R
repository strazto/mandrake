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

  out <- plan

  out %<>%
    dplyr::mutate_at(
      group,
      dplyr::if_else(
        is.na,
        target,
        .
        )) %>%
    dplyr::group_by(group) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  out %<>%
    dplyr::select(target) %>%
    dplyr::mutate(!! colname_out := purrr::map(
      ., extract_target_column_names, cache = cache)
      )

  out
}
