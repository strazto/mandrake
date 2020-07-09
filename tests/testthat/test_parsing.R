test_that("parsing our tag works", {
  input <- "
  #' @col [in] matthews_col [matts_col]
  #' I love matt's col. It is **Beautiful**.
  #' Check out [mandrake::extract_column_names()]
  #' @md
  matthews_function <- function(df) df
  "

  block <- roxygen2::parse_text(input)

  my_tag <- block[[1]]$tags[[1]]

  expect_equal(my_tag$tag, "col")
  expect_equal(my_tag$val$direction, list("in"))
  expect_identical(my_tag$val$aliases, list("matts_col"))
  expect_match(my_tag$val$rd, ".*\\\\strong\\{Beautiful\\}.*")
  expect_match(my_tag$val$rd, ".*\\\\link\\[mandrake:extract_column_names\\].*")

  expect_match(
    my_tag$val$html[[1]],
    "<strong>Beautiful</strong>",
    all = F)
  expect_match(
    my_tag$val$html[[1]],
    "<a href='.*/extract_column_names.html'>mandrake::extract_column_names.*</a>",
    all = F)

})

test_that("parsing a block with 2 deffs of our tag works", {
  input <- "
  #' @col [in] matthews_col [matts_col]
  #' I love matt's col. It is **Beautiful**.
  #' Check out [extract_column_names()]
  #' @col [out] matthews_new_col [newcol, newest_col]
  #' @md
  matthews_function <- function(df) df
  "

})
