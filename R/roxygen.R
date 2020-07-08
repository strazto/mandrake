

#' @export
roxy_tag_parse.roxy_tag_col <- function(x) {
  # \h is regex for horizontal whitespace (not linefeeds etc)
  # \h mightnt be defined in R's version of regex, will have to write a custom
  # character class
  spc_pat <- "\\h"

  front_pattern <- glue::glue(
    "{open_pat}{dir_pat}{spc_pat}*{name_pat}{spc_pat}*{alias_pat}",
    open_pat = glue::glue("^{spc_pat}*"), # Match 0 or more whitespace characters from start
    dir_pat  = "(?:(?<direction>\\[[^[\\]]*?\\]))?", # Optionally match a direction in square brackets [direction]
    name_pat = "(?<name>\\w+)", # Match wordy characters
    alias_pat  = "(?:(?<aliases>\\[[^[\\]]*?\\]))?" #Option match a yaml array style list in parentheses
  )

  matched_sig <- gregexpr(front_pattern, x, perl = TRUE)
  matches <- list()

  types <- c("capture.start", "capture.length")

  for (type in types) {
    matches[[type]] <- matched_sig %>%
      .[[1]] %>%
      attr(type) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(tidyr::everything(), values_to = type)
  }

  matches %<>%
  {
    dplyr::left_join(.$capture.start, .$capture.length, by = "name")
  }

  matches %<>%
    dplyr::mutate(
      capture_end = capture.start + capture.length,
      match = stringr::str_sub(x, capture.start, capture_end)
      ) %>%
    tidyr::pivot_wider(id_cols = name, names_from = name, values_from = match) %>%
    dplyr::mutate(dplyr::across(c(direction, aliases), ~list(yaml::yaml.load(.))))

  matches
}
