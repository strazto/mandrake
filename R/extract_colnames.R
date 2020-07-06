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
