# Target Column Extraction and Decoration =====

extract_target_column_names <- function(target, cache) {
  if (cache$exists(target)) out <- cache$get(target)
  else out <- list()

  out %<>% names()

  out
}


#' Extract Column Names for targets in Drake Plan
#'
#' Given a [drake::drake_plan()], extract the column names of each target, and store it
#' in a new column.
#'
#' @inheritParams drake::drake_graph_info
#' @param colname_out the name given to the list-column containing each target's columns
#' @family graph_decoration
#' @export
extract_column_names <- function(plan, cache, group = NULL, clusters = NULL,
                                 colname_out = "target_column_list", ...) {

  # Handle missingness for grouping data
  if (rlang::is_empty(group)) group <- "target"
  if (rlang::is_empty(clusters)) clusters <- plan %>% dplyr::pull(group) %>% unique()

  tmp_col = "temporary_join_col__"

  plan %<>%
    dplyr::mutate(
      !!tmp_col := dplyr::if_else(
        is.na(!!rlang::sym(group)),
        target,
        !!rlang::sym(group)
      ))

  out <- plan

  # Within the group, subset it, if it should cluster
  out %<>%
    dplyr::group_by(!!rlang::sym(tmp_col)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  out %<>%
    dplyr::select(target, !!group, !!tmp_col) %>%
    dplyr::mutate(!! colname_out := purrr::map(
      target, extract_target_column_names, cache = cache)
      ) %>%
    dplyr::select(!!tmp_col, !!colname_out)

  out %<>%
    dplyr::right_join(plan, by = tmp_col) %>%
    dplyr::select(-tmp_col)

  out
}

missing_col_placeholder <- function(col_key) {
  out <- tibble::tibble_row(
    name = col_key, aliases = list(NULL),
    html = list("<i>Column doc not found</i>"),
    html_ref = ""
  )
}


pull_out_coldocs <- function(columns, lookup_cache) {
  if (rlang::is_empty(columns)) return(NULL)
  out <- lookup_cache$mget(columns)

  which_missing <- attr(out, "missing")

  for (i in which_missing) {
    out[[i]] <- missing_col_placeholder(columns[[i]])
  }

  out %<>%
    dplyr::bind_rows()

  out %<>%
    dplyr::select(name, description = html, defined_at = html_ref) %>%
    dplyr::mutate(
      description = purrr::map_chr(description, ~paste0(., collapse = "<br>")),
      description = glue::glue(
        "{description}",
        "{discuss_found} {defined_at}",
        discuss_found = dplyr::if_else(
          stringr::str_length(defined_at) > 0,
          "Found at",
          ""),
        .sep = "<br>"
      )) %>%
    dplyr::select(-defined_at)
  out
}

#' Link Column Names to their Documentation.
#'
#' Given a list-column containing column-names for targets (As part of a
#' `drake::drake_plan()` having been processed by `extract_column_names()`),
#' cross reference these columns with the `lookup_cache`, and produce html tables
#' for each set of columns.
#'
#' @return a list column, of the same length as the outer dimension of
#'         the input list, each element being a dataframe describing each
#'         column.
#' @export
#' @family graph_decoration
#' @param target_column_list a list of character vectors specifying column names
#' @inheritParams load_package_colspec
link_col2doc <- function(target_column_list, lookup_cache) {
  if (!is(lookup_cache, "storr")) stop("Must pass a storr object to link_col2doc")

  out <- target_column_list %>%
    purrr::map(
      pull_out_coldocs, lookup_cache = lookup_cache
    )
  # out %<>% coldoc_dfs_2_html_tables()

  out
}

coldoc_dfs_2_html_tables <- function(list_coldoc_dfs) {
  out <- list_coldoc_dfs %>%
    purrr::map_chr(
      ~knitr::kable(
        .,
        format = "html",
        escape = FALSE) %>%
        kableExtra::kable_styling(c("striped", "responsive", "condensed"))
    )
  out
}

# Embedded Docstring Parsing ====

enrich_single_docstring <- function(x) {
  x %<>%
    stringr::str_trim() %>%
    roxygen2:::markdown() %>%
    pkgdown::rd2html() %>%
    paste(sep = "<br>", collapse = "<br>")
  x
}

enrich_docstrings <- function(docstrings) {
  docstrings %<>% purrr::map_chr(enrich_single_docstring)
  docstrings
}


# Command Highlighting ============

highlight_single_command <- function(x) {
  x %<>%
    rlang::expr_deparse(width = Inf) %>%
    styler::style_text(scope = "line_breaks") %>%
    paste(sep = "\n", collapse = "\n") %>%
    downlit::highlight(pre_class = "downlit")
  x
}

highlight_commands <- function(commands) {
  commands %<>% purrr::map_chr(highlight_single_command)
  commands
}

# Plan Decoration ==============

#' Decorate Plan with a Rich html Description
#'
#' @inheritParams drake::drake_graph_info
#' @inheritParams load_package_colspec
#' @param desc_colname the name of the column that provides the markdown description
#' @param colname_out the name of the column in which to store the enriched description
#' @export
#' @family graph_decoration
decorate_plan <- function(
  plan, cache, group = NULL, clusters = NULL,
  desc_colname = "desc", colname_out = desc_colname,
  lookup_cache = NULL,
  ...) {
  sym <- rlang::sym

  plan %<>%
    dplyr::mutate(
      !!colname_out := enrich_docstrings(!!sym(desc_colname))
    )

  tmp_extracted_nm <- "cols_extracted_tmp__"

  plan %<>%
    extract_column_names(
      cache,
      group = group, clusters = clusters,
      colname_out = tmp_extracted_nm) %>%
    dplyr::mutate(
      dplyr::across(
        tmp_extracted_nm,
        ~link_col2doc(
          .,lookup_cache = lookup_cache)))


  rendered_col <- plan %>%
    render_col_html(
      description_colname = colname_out,
      extracted_colname = tmp_extracted_nm
    )

  plan %<>%
    dplyr::mutate(!!colname_out := rendered_col) %>%
    dplyr::select(-c(tmp_extracted_nm))

  plan
}


render_col_html <- function(
  plan,
  description_colname,
  extracted_colname
) {
  sym <- rlang::sym

  rendered_col <- plan %>%
    # Transform list of dfs to character vector of html
    dplyr::mutate(
      !!extracted_colname := coldoc_dfs_2_html_tables(!!sym(extracted_colname))
    ) %>%
    glue::glue_data(
      "<h3>{target}</h3>",
      "{output_column}",
      "<h4>Command</h4>",
      "<details><summary>Command</summary>",
      "{highlight_commands(command)}",
      "</details>",
      "<h4>Columns</h4>",
      "{cols_extracted}",
      output_column = .[[description_colname]],
      cols_extracted = .[[extracted_colname]],
      .sep = "\n"
    )

  rendered_col
}

#' Attach Html Dependencies
#'
#' This should be done for self-contained instances of visNetwork, when
#' not being rendered as part of a larger document.
#' It sets up some stylesheet + javascript dependencies.
#'
#' Use it like:
#' ```
#' graph <- visNetwork::visNetwork()
#' graph %<>% attach_dependencies()
#' graph %>% visNetwork::visSave()
#' ```
#'
#' @family widget_dependencies
#' @param graph a graph made by [drake::render_graph()].
#' @param standalone logical, whether the current graph is rendered on
#'        its own page, or is part of a larger rmarkdown document.
#' @export
attach_dependencies <- function(graph, standalone = T) {
  # Jquery is used for manipulating the dom, and dynamically
  # Modifying the sidebar
  jquery <- htmltools::htmlDependency(
    "jquery", version = "3.4.1",
    src = "lib/jquery/3.4.1/",
    #src = list(href = "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.4.1/"),
    script = "jquery.min.js"
    )

  # Bootstrap's beautiful css
  bootstrap <- htmltools::htmlDependency(
    "bootstrap",
    version = "3.4.1",
    package = "mandrake",
    src = "lib/twitter-bootstrap/3.4.1/css/",
    #src = list(href = "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.4.1/css/"),
    stylesheet = "bootstrap.min.css"
  )

  # Custom Syntax Highlighting Classes
  chroma <- htmltools::htmlDependency(
    "chroma",
    "0.0.0.9001",
    package = "mandrake",
    src = "lib/mandrake",
    stylesheet = "chroma.css"
  )

  DOMPurify <- htmltools::htmlDependency(
    "DOMPurify",
    "2.2.7",
    # Unfortunately, rmarkdown has not reviewed my PR
    # https://github.com/rstudio/rmarkdown/pull/1948
    # Which allows href dependencies in rmarkdown docs
    # As a result, for now, I will be embedding this dependency
    package = "mandrake",
    src = "lib/DOMPurify/2.2.7",
    script = "purify.min.js"
  )

  # This script fixes a mangled "type" attribute set on some stylesheets
  # by htmlwidgets saveWidget(selfcontained = TRUE)
  # Restores the type to simply "text/css"
  fix_utf <- htmltools::htmlDependency(
    "fix_utf",
    "0.0.0.9001",
    package = "mandrake",
    src = "lib/mandrake",
    script = "fix_utf.js"
  )

  graph_events <- htmltools::htmlDependency(
    "graph_event_handlers",
    "0.0.0.9001",
    package = "mandrake",
    src = "lib/mandrake",
    script = "graph_events.js"
  )

  mustache <- htmltools::htmlDependency(
    "mustache.js",
    "4.2.0",
    package = "mandrake",
    src = "lib/mustache.js/4.2.0",
    script = "mustache.min.js"
  )

  templates <- get_template_dependencies()

  graph$dependencies %<>%
    c(list(chroma, fix_utf, DOMPurify, mustache, graph_events, templates))

  # Only add jquery and bootstrap if standalone
  if (standalone) {
    graph$dependencies %<>%
      c(list(jquery, bootstrap))
  }

  graph
}


get_template_dependencies <- function() {

  template_dir <- system.file(
    file.path("lib", "mandrake", "templates"),
    package = "mandrake"
  )

  templates <- template_dir %>%
    tools::list_files_with_exts("html.mustache")

  templates %<>%
    purrr::map_chr(function(template_path) {
      elem_id <- template_path %>%
        basename() %>%
        stringr::str_remove("\\.html\\.mustache$")

      elem_id <- paste0(
        c("mandrake", "template", elem_id),
        collapse = "-"
      )

      out <- template_path %>%
        htmltools::includeScript(
          id = elem_id,
          type = "x-tmpl-mustache"
        ) %>%
        as.character()
      return(out)
    })

  templates <- htmltools::htmlDependency(
    "mandrake_templates",
    "0.1",
    package = "mandrake",
    src = "lib/mandrake/templates",
    script = list(src = "test.html.mustache", type = "x-tmpl-mustache"),
    head = templates
  )

  return(templates)

}
