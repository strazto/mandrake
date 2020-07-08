


roxy_tag_parse.roxy_tag_col <- function(x) {
  # \h is regex for horizontal whitespace (not linefeeds etc)
  # \h mightnt be defined in R's version of regex, will have to write a custom
  # character class
  spc_pat <- "\\h"

  front_pattern <- glue::glue(
    "{open_pat}{dir_pat}{spc_pat}*{name_pat}{spc_pat}*{alias_pat}",
    open_pat = glue::glue("^{spc_pat}*"), # Match 0 or more whitespace characters from start
    dir_pat  = "(?:(?<dir>\\[[^[\\]]*?\\]))?", # Optionally match a direction in square brackets [direction]
    name_pat = "(?<name>\\w+)", # Match wordy characters
    alias_pat  = "(?:(?<alias>\\[[^[\\]]*?\\]))?" #Option match a yaml array style list in parentheses
  )

  matched_sig <- gregexpr(front_pattern, x, perl = TRUE)





}
