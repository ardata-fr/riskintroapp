
#' Display Error Popup Alert
#'
#' Shows a modal popup alert for displaying error messages with formatted error details.
#' Used throughout the application for consistent error reporting.
#'
#' @param title Character string. The alert title (default: "Error").
#' @param text Character string. Descriptive text for the error.
#' @param error The error object or message to display.
#' @param type Character string. Alert type (default: "danger").
#'
#' @export
#' @importFrom shinyWidgets show_alert
#' @inheritParams shinyWidgets::show_alert
popup_alert_error <- function(
    title = "Error",
    text = "The following error occured",
    error,
    type = "danger"
    ) {
  show_alert(
    title = "Unable to save workspace",
    text = tags$div(
      style = "text-align: left;",
      text,
      tags$br(),
      tags$pre(style = "white-space:normal;",
               tags$code(as.character(error)))),
    html = TRUE,
    type = type
  )
}

#' @importFrom shinyWidgets alert
alert_error <- function(text = "Error:", error, status = "danger") {
  shinyWidgets::alert(
    tags$div(
    style = "text-align: left;",
    text,
    tags$br(),
    tags$pre(style = "white-space:normal;",
             tags$code(as.character(error)))),
    status = status
  )
}
