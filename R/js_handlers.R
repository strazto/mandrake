describe_event_handlers <- function(handler) {
  out <- handler()

  out <- c(
    "\\preformatted{",
    out,
    "}"
  )
  out
}

#' (DEPRECATED) Handler for embedding data into the legend
#'
#' If attached as onSelect behaviour for a visNetwork graph,
#' will allow clicking of nodes to replace the legend for the graph
#' with metadata about that node.
#'
#' Presently requires JQuery to operate, so may not work when launched from
#' R session, but does work in pkgdown.
#'
#' It looks like this:
#'
#' `r suppressWarnings(describe_event_handlers(mandrake::embed_event_handler))`
#'
#' \lifecycle{deprecated}
#' @export
#' @family js_handlers
#' @family deprecated
embed_event_handler <- function() {
  lifecycle::deprecate_warn(
    "1.0.0",
    "mandrake::embed_event_handler()",
    "mandrake::attach_dependencies()",
    details = paste0(
      "calling embed_event_handler from R is DEPRECATED ",
      "due to lacking sanitization for XSS attacks\n",
      "the built-in JS deps + functions may be loaded using",
      "mandrake::attach_dependencies().",
      'and use drake::render_graph(on_select = "embed_event_handler") ',
      'to access this function.\n',
      "If you implement your own selection handler, ",
      "mandrake::attach_dependencies() imports the JS package DOMPurify\n",
      "You are strongly advised to use DOMPurify to sanitize any HTML you ",
      "render.\n",
      "to load other external JS deps, use htmltools::htmlDependency().\n"
    )
  )

  alert_event_handler(TRUE)
}

#' Handler for displaying node data as popup
#'
#' Mainly useful for testing - Basically chucks the data for that particular node
#' into a popup.
#'
#' @export
#' @family js_handlers
alert_event_handler <- function(warn_deprecated = FALSE){
  deprecate_msg <- "''"
  if (warn_deprecated) {
    deprecate_msg <- glue::glue(
      "'",
      "using mandrake::embed_event_handler() is deprecated.",
      "please use mandrake::attach_dependencies & ",
      'on_select = "embed_event_handler()" when rendering graph',
      "'",
      .sep = "\n"
    )
  }

  alert_msg <- glue::glue(
    "function(props) {
      node = this.body.data.nodes.get(props.nodes[0]);
      cr = '\\r\\n';
      alert(
        '' + <{deprecate_msg}>
        'selected ' +  node.label + ':' + cr +
        '=======' + cr +
        'COLNAMES:' + cr +
        node.on_select_col + cr +
        '=============== '
      );
    }",
    .open = "<{",
    .close = "}>",
    .sep = "\n"
  ) %>% as.character()

  alert_msg
}
