#' @export
#' @title App onStart function
#' @description
#' This function is called when the app starts. It sets options for the app and
#' adds a resource path for the app's static files.
#' @importFrom gdtools register_liberationsans
#' @importFrom shiny addResourcePath
#' @export
onstart <- function() {
  options(shiny.maxRequestSize = 30 * 1024^3) # for uploading large files with shiny::fileInput
  register_liberationsans()
  register_fontawesome()
  cirad_gg_theme_set()
  addResourcePath("riskintro", system.file(package = "riskintroapp", "www"))
  options(shiny.useragg = TRUE)
  invisible()
}
