#' Run the application locally
#'
#' Run the shiny application locally.
#'
#' @param port used port
#' @param launch.browser launch a browser when opening the application
#' @importFrom shiny runApp
#' @export
run_riskintro <- function(port = 9999, launch.browser = TRUE) {
  dir <- system.file(package = "riskintroapp", "shiny")
  shiny::runApp(dir, port = port, launch.browser = launch.browser)
}
