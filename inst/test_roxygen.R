input <- "
  #' Matthews Function
  #'
  #' @col [in] matthews_col [matts_col]
  #' I love matt's col. It is **Beautiful**.
  #' Check out [mandrake::extract_column_names()]
  #' @md
  #' @export
  matthews_function <- function(df) df
  "

#block <- roxygen2::parse_text(input)

topic <- roxygen2::roc_proc_text(roxygen2::rd_roclet(), input)

input_col_roclet_procd <- roxygen2::roc_proc_text(mandrake::col_roclet(), input)
roxygen2::roclet_output(mandrake::col_roclet(), input_col_roclet_procd, ".")

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


input_broke <- "
#' BUM
#' hey
#' @col [in, !] broke [i am broke, !]
my_fun <- function(x) return(x)
"

topic_broke <- roxygen2::roc_proc_text(roxygen2::rd_roclet(), input_broke)

broke_blocks <- roxygen2::parse_text(input_broke)

test_broke_tag <- roxygen2::roxy_tag("col", " [in] broke [i am broke, !]")

parsed_broke <- roxygen2::roxy_tag_parse(test_broke_tag)

# Test inherit parsing

input_inherit <- "
#' Hello
#' @col [in] matthews_col [matts_col]
#' I love matt's col. It is **Beautiful**.
#' Check out [mandrake::extract_column_names()]  is_in <- any(x$val$direction %in% dirs_in)
#'
#' @inheritCol mandrake [mpg, cyl]
#' @md
matthews_function_inherit <- function(df) df
"

#topic <- roxygen2::roc_proc_text(roxygen2::rd_roclet(), input_inherit)

inherit_col_roclet_procd <- roxygen2::roc_proc_text(mandrake::col_roclet(), input_inherit)
roxygen2::roclet_output(mandrake::col_roclet(), inherit_col_roclet_procd, ".")

block_inherit <- roxygen2::parse_text(input_inherit)


my_test_tag <- roxygen2::roxy_tag("inheritCol", " mandrake [mpg]")

roxygen2::roxy_tag_parse(my_test_tag) -> parsed_inherit
