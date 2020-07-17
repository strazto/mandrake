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
