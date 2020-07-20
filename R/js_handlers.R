
#' Handler for embedding data into the legend
#'
#' If attached as onSelect behaviour for a visNetwork graph,
#' will allow clicking of nodes to replace the legend for the graph
#' with metadata about that node.
#'
#' Presently requires JQuery to operate, so may not work when launched from
#' R session, but does work in pkgdown.
#' @export
#' @family js_handlers
embed_event_handler <- function() {
  glue::glue(
    "function(props) {",
    "container = this.body.container;",
    "legend = $(container).prev()[0];",
    "node = this.body.data.nodes.get(props.nodes[0]);",
    "$(legend).css({
         'font-size': '10px',
         'width' : '30%',
         'overflow-y' : 'scroll'
       });",
    "$(container).css('width', '70%');",
    "legend.innerHTML = node.on_select_col;",
    "}",
    .sep = "\n",
    .open = "<{",
    .close = "}>"
  ) %>% as.character()
}

#' Handler for displaying node data as popup
#'
#' Mainly useful for testing - Basically chucks the data for that particular node
#' into a popup.
#'
#' @export
#' @family js_handlers
alert_event_handler <- function(){
  js <- "
  function(props) {
    node = this.body.data.nodes.get(props.nodes[0]);
    cr = '\\r\\n';
    alert('selected ' +  node.label + ':' + cr +
            '=======' + cr +
            'COLNAMES:' + cr +
           node.on_select_col + cr +
           '=============== '
           );
  }"
}
