setInterval(function(){
  if ($('html').hasClass('shiny-busy')) {
    setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
        $('div.busy').show()
      }
    }, 1500)
  } else {
    $('div.busy').hide()
  }
}, 100)
