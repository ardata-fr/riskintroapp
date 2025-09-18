.onLoad <- function(...) {
  shiny::registerInputHandler("custom.dragula", function(data, ...) {
    if (is.null(data)) {
      NULL
    } else {
      data$source <- unlist(data$source)
      data$target <- lapply(data$target, unlist, recursive = FALSE)
      data
    }
  }, force = TRUE)
}
