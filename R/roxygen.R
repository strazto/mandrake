

#' @export
roxy_tag_parse.roxy_tag_col <- function(x) {
  # \h is regex for horizontal whitespace (not linefeeds etc)
  # \h mightnt be defined in R's version of regex, will have to write a custom
  # character class

  explain_format = FALSE
  format_msg <- glue::glue(
    "format is:",
    "  #' @col [direction] column_name [yaml, list, of, aliases]",
    "  #' Optional Description of column_name.",
    "[direction] is optional yaml list that can take values [in, out].",
    "[yaml, list, of, aliases] is optional, and allows column to be referenced",
    "  by other strings",
    .sep = "\n"
  )

  spc_pat <- "\\h"

  front_pattern <- glue::glue(
    "{open_pat}{dir_pat}{spc_pat}*{name_pat}{spc_pat}*{alias_pat}",
    open_pat = glue::glue("^{spc_pat}*"), # Match 0 or more whitespace characters from start
    dir_pat  = "(?:(?<direction>\\[[^[\\]]*?\\]))?", # Optionally match a direction in square brackets [direction]
    name_pat = "(?<name>\\w+)", # Match wordy character
    alias_pat  = "(?:(?<aliases>\\[[^[\\]]*?\\]))?" #Option match a yaml array style list in parentheses
  )
  match_object <- gregexpr(front_pattern, x$raw, perl = TRUE)

  if (match_object[[1]] < 0) {
    roxygen2::roxy_tag_warning(x, "Unable to parse column header!")
    x$val <- NULL
    roxygen2::roxy_tag_warning(x, format_msg)
    return(x)
  }

  header_length <- match_object[[1]] %>% {. + attr(., "match.length")}

  matches <- x$raw %>% extract_named_captures(match_object)
  body <- x$raw %>% stringr::str_sub(start = header_length)

  if (any(is.na(matches$name))) {
    roxygen2::roxy_tag_warning(x, "missing name argument")
    explain_format <- TRUE
  }

  parse_yaml_part <- function(yaml_text, part) {
    tryCatch(
      yaml::yaml.load(yaml_text),
      error = function(cond) {
        roxygen2::roxy_tag_warning(x, "Error parsing ", part)
        explain_format <- TRUE
        return(NULL)
      }
      )
  }

  matches %<>%
    dplyr::mutate(
      dplyr::across(
        c(direction, aliases),
        ~list(parse_yaml_part(., curr_column()))
        )
      )

  if (explain_format) roxygen2::roxy_tag_warning(x, format_msg)

  body_rd   <- body %>% roxygen2:::markdown_if_active(x)
  body_html <- body_rd %>% pkgdown::rd2html(autolink = TRUE)

  matches %<>%
    dplyr::mutate(
      body = body,
      rd = body_rd,
      html = list(body_html)
    )

  x$val <- matches

  x
}

#' Extract named captures from gregexpr
extract_named_captures <- function(string, match_object) {

  out <- list()

  types <- c("capture.start", "capture.length")

  for (type in types) {
    out[[type]] <- match_object %>%
      .[[1]] %>%
      attr(type) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(tidyr::everything(), values_to = type)
  }

  out %<>%
    {
      dplyr::left_join(.$capture.start, .$capture.length, by = "name")
    }

  out %<>%
    dplyr::mutate(
      capture_end = capture.start + capture.length,
      match = stringr::str_sub(string, capture.start, capture_end)
    )

  out %<>%
    tidyr::pivot_wider(id_cols = name, names_from = name, values_from = match)

  out
}
