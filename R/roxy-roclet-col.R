#' Roclet for processing the `@col` tag
#'
#' @family roxygen_roclet
#' @export
col_roclet <- function() {
  roxygen2::roclet("col")
}

#' @rdname col_roclet
#' @export
#' @importFrom roxygen2 roclet_process
roclet_process.roclet_col <- function(
  roc, blocks, env, base_path
) {
  out <- list()
  out$cols <- blocks %>%
    purrr::map_dfr(
      ~roclet_col_process_col_tag(roc, ., env, base_path)
    )

  out$inherited <- blocks %>%
    purrr::map_dfr(
      ~ roclet_col_process_inheritCol_tag(roc, ., env, base_path))

  out %<>% dplyr::bind_rows()

  out
}


roclet_col_process_col_tag <- function(roc, block, env, base_path) {
  `%||%` <- rlang::`%||%`
  st <- get_inheritance_cache(env)

  # From:
  # https://github.com/r-lib/roxygen2/blob/c73def498f29783044fe50865f22c2482f80942d/R/rd.R#L122
  topic <- roxygen2::block_get_tag(block, "name")$val %||% block$object$topic

  pkg <- roxygen2::roxy_meta_get("current_package") %||%
    rlang::env_get(env, ".packageName", default = "")

  out <- roxygen2::block_get_tags(block, "col")

  out %<>%
    purrr::map_dfr("val")

  out %>%
    dplyr::group_by(name) %>%
    dplyr::group_walk(add_entry_to_cache, lookup_cache = st)

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
      ~roxy_tag_inheritCol_process(
        ., base_path = base_path, env = env
      ))

  out
}


default_column_map_output_path <- function() {
  file.path("inst", "mandrake")
}



#' @rdname col_roclet
#' @export
#' @importFrom roxygen2 roclet_output
roclet_output.roclet_col <- function(roc, results, base_path, ...) {
  `%||%` <- rlang::`%||%`

  if (rlang::is_empty(results) | !nrow(results)) {
    cat("roclet_col found no @col or @inheritCol")
    return(invisible(NULL))
  }

  output_dir <- roxygen2::roxy_meta_get(
    "mandrake_output",
    default_column_map_output_path())

  cat(c("Writing", output_dir))

  output_dir <- file.path(base_path, output_dir)

  if (!dir.exists(output_dir)) {
    warning(
      "mandrake output path, ", output_dir, " does not exist, making it now!"
      )
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  results %>%
    dplyr::group_by(package) %>%
    dplyr::group_walk(
      write_package_results,
      output_dir = output_dir,
      .keep = TRUE
      )

  invisible(NULL)
}

write_package_results <- function(results, pkg_name, output_dir = NULL) {
  if (rlang::is_empty(output_dir)) stop("Output dir needs to be given!")


  output_path <- glue::glue("{pkg_name}.yaml")

  cat(c("\n|", output_path))

  output_path <- output_dir %>% file.path(output_path)

  out <- results %>%
    dplyr::distinct(name, body, .keep_all = TRUE) %>%
    serialize_df()

  out <- c(roxygen2:::made_by("#"), out)

  out %>% write(file = output_path)

  invisible(NULL)
}
