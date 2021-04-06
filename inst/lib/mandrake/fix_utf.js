// htmlwidgets has a bug w/ rendering selfcontained widgets
// Some stylesheets have their `type` attribute overriden to describe the charset
// This doesn't work, so this script undoes it
$(function() {
  $('style[type="text/css; charset=utf-8"]').attr("type", "text/css");
})

