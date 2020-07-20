tag_item <- function(x, tag_val) {
  if (is.null(x)) x <- list()
  attr(x, "tag") <- tag_val
  x
}

tag_list_col <- function(x) {
  x %<>%
    purrr::modify(~tag_item(., "list")) %>%
    tag_item("list_col")
  x
}


serialize_df <- function(x) {

  out <- x %>%
    dplyr::mutate(dplyr::across(where(is.list), tag_list_col)) %>%
    purrr::transpose() %>%
    purrr::set_names( nm = purrr::map_chr(., "name"))

  out %<>%
    yaml::as.yaml()
  out
}

list_handler <- function(x) {
  tag_item(x, "list")
}

has_list_tag <- function(x) {
  x %<>% attr("tag")

  if (rlang::is_empty(x)) return(FALSE)

  return("list" %in% x)
}

restore_col_type <- function(x) {
  is_list_col <- x %>%
    purrr::map_lgl(has_list_tag) %>%
    any()

  if (is_list_col) {
    x %<>%
      tag_list_col() %>%
      purrr::simplify_all()
  } else {
    x %<>% unlist()
  }

  x
}

deserialize_df <- function(x) {
  out <- x %>% yaml::yaml.load(handlers = list(list = list_handler))

  out %<>% purrr::set_names(NULL)

  out %<>%
    purrr::transpose() %>%
    tibble::as_tibble()

  out %<>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), restore_col_type)
    )

  out
}

default_column_map_input_path <- function() {
  "mandrake"
}

#' @export
load_package_colspec <- function(pkg_name, lookup_cache = NULL) {
  # If no store is given, make one
  st <- lookup_cache
  if (rlang::is_empty(st)) st <- storr::storr_environment()

  `%||%` <- rlang::`%||%`
  pkg_path <- system.file(package = pkg_name)

  opts <- roxygen2::load_options(pkg_path)

  mandrake_path <- opts$mandrake_output %||%
    default_column_map_input_path()

  # The directory containing the mapppings
  mandrake_path <- file.path(pkg_path, mandrake_path)

  message("Adding cols from ", pkg_name, " to lookup cache")

  spec_paths <- mandrake_path %>%
    list.files(pattern = ".*\\.ya?ml$")

  spec_paths %>%
    purrr::walk(function(path) {
      message("Including ", path, " in lookup cache")
      spec <- file.path(mandrake_path, path) %>% load_colspec_file()

      spec %>%
        dplyr::group_by(name) %>%
        dplyr::group_walk(add_entry_to_cache, lookup_cache = st, .keep = TRUE)

      invisible(NULL)
    })


  return(st)
}


#' Load colspec from a single file, to be imported into storr cache
load_colspec_file <- function(path) {
  out <- path %>%
    readLines() %>%
    paste0(collapse = "\n") %>%
    deserialize_df()

  out
}

add_entry_to_cache <- function(entry, keys, lookup_cache = NULL) {
  if (rlang::is_empty(lookup_cache))
    stop("Empty lookup cache given to add_entry_to_cache")

  keys %<>% dplyr::pull()
  aliases <- entry %>%
    dplyr::pull(aliases) %>%
    purrr::flatten_chr()

  keys %<>% c(aliases)

  lookup_cache$fill(keys, entry)
  invisible(NULL)
}
