

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
    .sep = "\n",
    .trim = FALSE
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
      html = list(body_html),
      name = stringr::str_trim(name)
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

# Generate / Dispatch rd sections ==============

#' @export
#' @family roxygen
roxy_tag_rd.roxy_tag_col <- function(x, base_path, env) {
  dirs_in <- c("in", "i")
  dirs_out <- c("out", "i")

  is_in <- any(x$val$direction %in% dirs_in)
  is_out <- any(x$val$direction %in% dirs_out)

  if (is_in) {
    out <- roxygen2::rd_section("mandrake_input_column", x$val)
    return(out)
  }

  if (is_out) {
    out <- roxygen2::rd_section("mandrake_output_column", x$val)
    return(out)
  }

  out <- roxygen2::rd_section("mandrake_general_column", x$val)
  return(out)
}

# Merge rd sections =========

#' @export
#' @family roxygen
merge.rd_section_mandrake_input_column <- function(x, y, ...) {
  rd_section("mandrake_input_column", dplyr::bind_rows(x$val, y$val))
}

#' @export
#' @family roxygen
merge.rd_section_mandrake_output_column <- function(x, y, ...) {
  rd_section("mandrake_output_column", dplyr::bind_rows(x$val, y$val))
}

#' @export
#' @family roxygen
merge.rd_section_mandrake_general_column <- function(x, y, ...) {
  rd_section("mandrake_general_column", dplyr::bind_rows(x$val, y$val))
}

# Format rd sections =================

#' @export
#' @family roxygen
format.rd_section_mandrake_input_column <- function(x, ...) {
  general_mandrake_column_format(x, title = "Input Columns")
}

#' @export
#' @family roxygen
format.rd_section_mandrake_output_column <- function(x, ...) {
  general_mandrake_column_format(x, title = "Output Columns")
}

#' @export
#' @family roxygen
format.rd_section_mandrake_general_column <- function(x, ...) {
  general_mandrake_column_format(x, title = "Other Columns")
}

# rd formatting helpers ==============

general_list_format <- function(x, ...) {
  out <- glue::glue(
    "\\item{<nm>}{<rd>}",
    .open = "<",
    .close = ">",
    nm = x[["name"]],
    rd = x[["rd"]],
    .sep = "\n"
  )
  out
}

general_mandrake_column_format <- function(x, ...) {
  `%||%` <- rlang::`%||%`
  dots <- rlang::list2(...)

  title <- dots$title %||% "General Column"

  glue::glue(
    "\\section{<title>:}{",
    "<general_list_format(x)>",
    "}",
    .open = "<", .close = ">", .sep = "\n"
  )
}
