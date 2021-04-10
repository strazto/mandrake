const embedHandler_legacy = function(props) {
  container = this.body.container;
  legend = $(container).parent().children("[id*='legendhtmlwidget']")[0];
  node = this.body.data.nodes.get(props.nodes[0]);
  $(legend).css({
    'font-size': '10px',
    'width' : '30%',
    'overflow-y' : 'scroll'
  });

  $(container).css('width', '70%');

  legend.innerHTML = DOMPurify.sanitize(node.on_select_col);
  $(legend).find('pre').css('font-size', 'inherit');
}

const alert_handler = function(props) {
  node = this.body.data.nodes.get(props.nodes[0]);
  cr = '\r\n';

  console.log({props: props, object: this, node : node});

  alert('selected ' +  node.label + ':' + cr +
    '=======' + cr +
    'COLNAMES:' + cr +
    node.on_select_col + cr +
    '=============== '
  );
}


const embedHandler = function(props) {
  var container = this.body.container;
  var legend = $(container).parent().children("[id*='legendhtmlwidget']")[0];
  var node = this.body.data.nodes.get(props.nodes[0]);
  $(legend).css({
    'font-size': '10px',
    'width' : '30%',
    'overflow-y' : 'scroll'
  });

  $(container).css('width', '70%');

  node['x'] = JSON.parse(node.on_select_col);
  node.x.column_descriptions = node.x.column_descriptions[0];

  var template = $("#mandrake-template-display").html();
  var rendered = Mustache.render(template, node);

  // console.log({props: props, object: this, node : node});

  legend.innerHTML = DOMPurify.sanitize(rendered);

  $(legend).find('pre').css('font-size', 'inherit');
}
