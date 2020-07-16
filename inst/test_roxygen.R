input <- "
  #' @col [in] matthews_col [matts_col]
  #' I love matt's col. It is **Beautiful**.
  #' Check out [mandrake::extract_column_names()]
  #' @md
  matthews_function <- function(df) df
  "

block <- roxygen2::parse_text(input)


input2col <- "
#' Ahoyhoy
#'
#' I am a nerd
#' @col [in] matthews_col [matts_col]
#' I love matt's col. It is **Beautiful**.
#' Check out [mandrake::extract_column_names()]
#' @col [out] matthews_new_col [newcol, newest_col]
#' @col [o] MY_BEST_COL_YET
#' This is the BEST col EVER my friend I love it
#' @md
matthews_function2 <- function(df) df
"

input_vector <- c(input, input2col)

blocks <- roxygen2::parse_text(input_vector)
