col_roclet <- function() {
  roclet("col")
}

roclet_process.roclet_col <- function(
  roc, blocks, env, base_path
) {
  out <- blocks %>%
    purrr::map_dfr(
      ~ get_block_data(roc, ., env, base_path))

  out
}


get_block_data <- function(
  roc, block, env, base_path) {

  topic <- block$object$topic
  pkg <- roxygen2::roxy_meta_get("current_package")

  out <- roxygen2::block_get_tags(block, "col")

  out %<>%
    purrr::map_dfr("val")

  out %<>%
    dplyr::mutate(
      topic = topic,
      package = pkg,
      html_ref = downlit::autolink(glue::glue("{package}::{topic}")),
      rd_ref   = glue::glue(
        "\\code{\\link[<package>:<topic>]{<package>::<topic>}}",
        .open = "<", .close = ">")
      )
  out
}
