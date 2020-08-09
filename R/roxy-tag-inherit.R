#' @export
roxy_tag_parse.roxy_tag_inheritCol <- function(x) {

  explain_format = FALSE
  x$val <- NULL
  `%||%` <- rlang::`%||%`

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
    roxygen2::roxy_tag_warning(x, format_msg)
    return(x)
  }

  matches <- x$raw %>% extract_named_captures(match_object)

  # If no source is given, use the current package
  current_package <- roxygen2::roxy_meta_get("current_package") %||% "package"

  matches %<>%
    dplyr::mutate(
      src = dplyr::if_else(is.na(src), current_package, src),
      src = stringr::str_trim(src)
      )

  matches <- {
    withCallingHandlers({
      matches %>%
        dplyr::mutate(columns = list(parse_yaml_part(columns, "columns")))
    },
    parserError = function(e) {
      roxygen2::roxy_tag_warning(x, e$message)
      p <- rlang::env_parent()
      p$explain_format <- TRUE
      return(NULL)
    }
    )
  }

  if (explain_format) roxygen2::roxy_tag_warning(x, format_msg)
  else x$val <- matches

  x
}

get_inheritance_cache <- function(env) {
  if (!rlang::env_has(env, ".mandrake")) env$.mandrake <- storr::storr_environment()

  rlang::with_env(env, {
    if (!inherits(.mandrake, "storr")) .mandrake <- storr::storr_environment()
  })

  return(env$.mandrake)
}

cache_pkg_if_not <- function(package, lookup_cache) {
  ns_pkg <- paste0("package:", package)

  if (!ns_pkg %in% lookup_cache$list_namespaces()) {
    load_package_colspec(package, lookup_cache)
  }

  invisible(lookup_cache)
}

roxy_tag_inheritCol_process <- function(x, base_path, env) {
  st <- get_inheritance_cache(env)

  pkg <- x$val$src

  cache_pkg_if_not(pkg, st)
  ns_pkg <- paste0("package:", pkg)

  x$val %<>% tidyr::unnest(columns)

  values <- st$mget(x$val$columns, namespace = ns_pkg)

  values %<>% dplyr::bind_rows()
  values
}

#' @export
roxy_tag_rd.roxy_tag_inheritCol <- function(x, base_path, env) {
  values <- roxy_tag_inheritCol_process(x, base_path, env)

  values %<>%
    dplyr::mutate(
      rd = glue::glue(
        "{rd}",
        "Inherited from {rd_ref}",
        .sep = "\n"
      )
    )

  out <- roxygen2::rd_section("mandrake_input_column", values)

  out
}
