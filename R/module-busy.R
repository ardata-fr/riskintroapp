#' @importFrom htmltools div h4 singleton tagList
#' @importFrom shiny icon
busyUI <- function(id) {
  tagList(
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "riskintro/css/busy.css"),
      tags$script(type = "text/javascript", src = "riskintro/js/busy.js")
    )),
    tags$div(
      class = "busy",
      tags$h4("working..."),
      icon("gear", "fa-5x fas fa-spin")
    )
  )
}
