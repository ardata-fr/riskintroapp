.configIsValidMsgs <- list(
  rescale_missing = "Rescaling parameters have not been set.",
  epi_units_missing = "Epidemiological units dataset must be imported.",
  emission_risk_missing = "Emission scores dataset must be imported."
)

#' @title Build a Configuration Status
#' @description
#' Build a configuration status from a
#' boolean and a message to display if the
#' boolean is FALSE.
#' @param value boolean value, FALSE for something wrong
#' in the configuration.
#' @param msg string value, its purpose is to be displayed
#' if `value` is FALSE
#' @examples
#' build_config_status(value = TRUE, msg = "something is wrong")
#' @noRd
build_config_status <- function(value, msg) {
  if (!is_non_empty_single_logical(value)) {
    value <- FALSE
  }
  if (!is_non_empty_string(msg)) {
    msg <- "unexpected error"
  }
  x <- value
  attr(x, "comment") <- msg
  x
}

#' @importFrom rlang is_string
#' @title Test for Non-Empty String
#' @description
#' Checks if the input is a single string that is not empty.
#' @param x The object to test.
#' @return A logical value: `TRUE` if `x` is a single string with non-zero
#' length; otherwise, `FALSE`.
#' @examples
#' is_non_empty_string("Hello") # TRUE
#' is_non_empty_string("") # FALSE
#' is_non_empty_string(c("A", "B")) # FALSE
#' is_non_empty_string(NA) # FALSE
#' @noRd
is_non_empty_string <- function(x) {
  is_string(x) && nzchar(x)
}

#' @title Test for Non-Empty Single Logical Value
#' @description
#' Checks if the input is a non-NA single logical value.
#' @param x The object to test.
#' @return A logical value: `TRUE` if `x` is a single, non-NA logical value;
#' otherwise, `FALSE`.
#' @examples
#' is_non_empty_single_logical(TRUE)
#' is_non_empty_single_logical(FALSE)
#' is_non_empty_single_logical(c(TRUE, FALSE))
#' is_non_empty_single_logical(NA)
#' @importFrom rlang is_logical
#' @noRd
is_non_empty_single_logical <- function(x) {
  is_logical(x) && length(x) == 1 && !is.na(x)
}

#' @title Report a Configuration Status
#' @description
#' Render a simple Shiny-friendly UI element that shows
#' a green checkmark with the message if valid, or a red cross
#' with the message if invalid.
#' @param status an object created by [build_config_status()]
#' @return a shiny::tagList containing an icon and message
#' @examples
#' status <- build_config_status(TRUE, "All good")
#' report_config_status(status)
#' @noRd
report_config_status <- function(status) {

  if (is.null(status)) return(NULL)
  stopifnot(is.logical(status), length(status) == 1L)

  msg <- attr(status, "comment") %||% ""
  if (isTRUE(status)) {
    shiny::div(
      shiny::icon("check-circle", style = "color: green;"),
      shiny::span(msg, style = "margin-left: 5px; margin-right: 5px;")
    )
  } else {
    shiny::div(
      shiny::icon("times-circle", style = "color: red;"),
      shiny::span(msg, style = "margin-left: 5px; margin-right: 5px;")
    )
  }
}
