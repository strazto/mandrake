#' @export
roxy_tag_parse.roxy_tag_inheritCol <- function(x) {

  explain_format = FALSE
  format_msg <- glue::glue(
    "format is:",
    "  #' @inheritCol src [yaml, list, of, columns]",
    "---",
    "src is optional, and should be a packagename, if omitted will inherit from",
    "  current package.",
    "[yaml, list, of, columns] is required, and allows column to be referenced",
    "  by other strings.",
    "  Columns may be given unnamed or named. as in:",
    "    [col1, col2, col_alias: col3]",
    "  Elements should be named if they are given to the function as named",
    "    param, but defined in the source package with a different name.",
    "    Use the form [name_param: name_source]",
    "  Column list may span multiple lines.",
    .sep = "\n",
    .trim = FALSE
  )

  spc_pat <- "\\h"

  front_pattern <- glue::glue(
    "{open_pat}{src_pat}{spc_pat}*{col_pat}",
    open_pat = glue::glue("^{spc_pat}*"), # Match 0 or more whitespace characters from start
    src_pat = "(?<src>[\\w\\.]+)?", # Optionally match wordy character
    # Necessarily match a yaml array style list in []
    col_pat  = "(?:(?<columns>\\[[^[\\]]*?\\]))"
  )

  match_object <- gregexpr(front_pattern, x$raw, perl = TRUE)

  if (match_object[[1]] < 0) {
    roxygen2::roxy_tag_warning(x, "Unable to parse column header!")
    x$val <- NULL
    roxygen2::roxy_tag_warning(x, format_msg)
    return(x)
  }

  matches <- x$raw %>% extract_named_captures(match_object)


  x
}

#' @export
roxy_tag_rd.roxy_tag_inheritCol <- function(x, base_path, env) {

  roxygen2::rd_section("mandrake_input_column", x$val)
}
