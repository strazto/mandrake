test_that("parsing our tag works", {
  input <- "
  #' @col [in] matthews_col [matts_col]
  #' I love matt's col. It is **Beautiful**.
  #' Check out [extract_column_names()]
  #' @md
  matthews_function <- function(df) df
  "

  block <- roxygen2::parse_text(input)




  fail("Test part not implemented")
})
