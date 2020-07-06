
#' Extract column names for targets in plan
#' Maybe allow
extract_column_names <- function(plan, cache, group = NULL, clusters = NULL, ...) {
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

  out
}
