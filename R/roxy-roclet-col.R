#' @export
col_roclet <- function() {
  roxygen2::roclet("col")
}

#' @export
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
  `%||%` <- rlang::`%||%`

  topic <- block$object$topic
  pkg <- roxygen2::roxy_meta_get("current_package")

  out <- roxygen2::block_get_tags(block, "col")

  out %<>%
    purrr::map_dfr("val")

  out %<>%
    dplyr::mutate(
      topic = topic,
      package = pkg,
      rd_ref   = glue::glue(
        "\\code{\\link[<package><s><topic>]{<package><s><s><topic>}}",
        .open = "<", .close = ">",
        package = pkg %||% "",
        s = dplyr::if_else(rlang::is_empty(pkg), "", ":")
        ),
      html_ref = pkgdown::rd2html(rd_ref, autolink = TRUE)
      )
  out
}

#' @export
roclet_output.roclet_col <- function(roc, results, base_path, ...) {
  invisible(NULL)
}
