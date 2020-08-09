#' Roclet for processing the `@col` tag
#'
#' @family roxygen_roclet
#' @export
col_roclet <- function() {
  roxygen2::roclet("col")
}

#' @rdname col_roclet
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
  out <- roclet_col_process_col_tag(roc, block, env, base_path)


  out
}

roclet_col_process_col_tag <- function(roc, block, env, base_path) {
  `%||%` <- rlang::`%||%`

  # From:
  # https://github.com/r-lib/roxygen2/blob/c73def498f29783044fe50865f22c2482f80942d/R/rd.R#L122
  topic <- roxygen2::block_get_tag(block, "name")$val %||% block$object$topic

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

roclet_col_process_inheritCol_tag <- function(roc, block, env, base_path) {
  out <- roxygen2::block_get_tags(block, "inheritCol")
  out %<>%
    purrr::map_dfr(
      list("val", roxy_tag_inheritCol_process),
      base_path = base_path, env = env)


  out
}


default_column_map_output_path <- function() {
  file.path("inst", "mandrake")
}



#' @rdname col_roclet
#' @export
roclet_output.roclet_col <- function(roc, results, base_path, ...) {
  `%||%` <- rlang::`%||%`

  pkg_name <- roxygen2::roxy_meta_get("current_package") %||% "package"

  output_path <- roxygen2::roxy_meta_get(
    "mandrake_output",
    default_column_map_output_path())

  output_path %<>% file.path(glue::glue("{pkg_name}.yaml"))

  cat(c("Writing", output_path))

  output_path <- file.path(base_path, output_path)
  output_dir <- dirname(output_path)

  if (!dir.exists(output_dir)) {
    warning(
      "mandrake output path, ", output_dir, " does not exist, making it now!"
      )
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  out <- results %>%
    serialize_df()

  out <- c(roxygen2:::made_by("#"), out)

  out %>% write(file = output_path)

  invisible(NULL)
}
