
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

# Function to create inline Shiny components
inlineComponents <- function(..., gap = "10px") {
  # Get all the components passed to the function
  components <- list(...)

  # Wrap each component in a div with inline-block style
  wrapped_components <- lapply(components, function(comp) {
    tags$div(
      style = paste0("display: inline-block; margin-right: ", gap, "; vertical-align: top;"),
      comp
    )
  })

  # Return a div containing all wrapped components
  tags$div(
    style = "display: block;",
    wrapped_components
  )
}

# More flexible function with additional styling options
inlineLayout <- function(...,
                         gap = "10px",
                         align = "middle",
                         wrap = FALSE) {
  components <- list(...)

  # Determine flex-wrap based on wrap parameter
  flex_wrap <- if(wrap) "wrap" else "nowrap"

  # Create the container with flexbox for better control
  tags$div(
    style = paste0(
      "display: flex; ",
      "gap: ", gap, "; ",
      "align-items: ", align, "; ",
      "flex-wrap: ", flex_wrap, ";"
    ),
    components
  )
}
