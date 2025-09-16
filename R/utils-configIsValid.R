.configIsValidMsgs <- list(
  rescale_missing = "Rescaling parameters have not been set.",
  epi_units_missing = "Epidemiological units dataset must be imported.",
  emission_risk_missing = "Emission scores dataset must be imported."
)

#' @title Validate Configuration
#' @description
#' Generic function to validate configuration based on validator type.
#' @param x A string with the method to call
#' @param ... parameters to validate
#' @return A config_status object (logical with message attribute)
#' @examples
#' config_is_valid.test <- function(x, ...){
#' params <- list(...)
#' if(is.null(params$a)) {
#'   return(FALSE)
#' }
#' if(is.null(params$b)) {
#'   return(FALSE)
#' }
#' if(is.null(params$c)) {
#'   return(FALSE)
#' }
#' TRUE
#' }
#'
#' config_is_valid("test", a = 1, b = 2, c = 3)
#'
#' @export
config_is_valid <- function(x, ...) {
  params <- list(...)
  class(params) <- x
  UseMethod("config_is_valid", params)
}

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
build_config_status <- function(value, msg, error = NULL, warnings = NULL) {
  if (!is_non_empty_single_logical(value)) {
    value <- FALSE
  }
  if (!is_non_empty_string(msg)) {
    msg <- "unexpected error"
  }
  x <- value
  attr(x, "comment") <- msg
  if (!is.null(error)) {
    attr(x, "error") <- get_error_message(error)
  }
  if (!is.null(warnings) && length(warnings) > 0) {
    attr(x, "warnings") <- warnings
  }
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
#' @param status an object created by `build_config_status`
#' @return a shiny::tagList containing an icon and message
#' @examples
#' status <- build_config_status(TRUE, "All good")
#' report_config_status(status)
#' @noRd
#' @importFrom shinyWidgets panel
report_config_status <- function(status) {
  if (is.null(status)) return(NULL)
  stopifnot(is.logical(status), length(status) == 1L)

  msg <- attr(status, "comment") %||% ""
  error <- attr(status, "error")
  warnings <- attr(status, "warnings")

  if (isTRUE(status) && is.null(warnings)) {
    shinyWidgets::panel(
      status = "success",
      shiny::icon("circle-check", , class = "text-success"),
      shiny::span(msg, style = "margin-left: 5px; margin-right: 5px;")
    )
  } else if (isTRUE(status) && !is.null(warnings)) {
    warn_divs <- lapply(as.list(warnings), function(x) {
      shiny::div(
        shiny::icon("triangle-exclamation", class = "text-warning"),
        shiny::span(x, style = "margin-left: 5px; margin-right: 5px;")
      )
    })
    shinyWidgets::panel(
      status = "warning",
      div(
        shiny::icon("circle-check", , class = "text-success"),
        shiny::span(msg, style = "margin-left: 5px; margin-right: 5px;")
      ),
      warn_divs
    )
  } else {
    shinyWidgets::panel(
      status = "danger",
      shiny::icon("circle-xmark", class = "text-danger"),
      shiny::span(msg, style = "margin-left: 5px; margin-right: 5px;"),
      if (!is.null(error)) error_box(error)
    )
  }
}

error_box <- function(error) {
  shiny::div(
    error,
    style = "margin-left: 5px; margin-right:
  5px; background-color: #d3d3d3; font-family:
  monospace; padding: 2px 4px; border-radius: 3px;")
}


#' Capture all function results and side effects
#'
#' A combination of [purrr::safely()] in case of errors and [purrr::quietly]
#' in case of warnings.
#'
#' Errors are generally passed to isConfigValid reactives and warnings are passed
#' to report_warnings
#'
#' @param .fun function to run safely and quietly (caputuring results)
#' @param ... argments to pass to `.fun`
#' @keywords internal
#' @importFrom purrr quietly safely
#' @export
safe_and_quiet <- function(.fun, ...){
  safe_fun <- quietly(safely(.fun))
  out_safe <- safe_fun(...)
  out <-
    list(
      result = out_safe$result$result,
      error = out_safe$result$error,
      output = out_safe$output,
      warnings = out_safe$warnings,
      messages = out_safe$messages)
  if(!is.null(out$error)){
    out$error <- get_error_message(out$error)
  }
  # out <- map(out, length_zero_to_na)
  out
}


#' Report CLI warning messages in HTML
#'
#' Converts CLI warning messages from riskintroanalysis functions into
#' pretty UI outputs to use with renderUI and uiOutput.
#' @param msg raw msg string, generally captured by safe_and_quiet()
#' @return HTML object to use with renderUI.
#' @keywords internal
report_warning <- function(msg){
  if (is.null(msg)) return(NULL)

  html_msg <- cli_warning_to_html(msg)
  shinyWidgets::panel(
    status = "warning",
    html_msg
  )
}

#' Convert CLI Warning Message to HTML
#'
#' Converts CLI-formatted warning messages from riskintroanalysis into HTML
#' suitable for display in Shiny applications.
#'
#' @param message Character string containing CLI-formatted warning message
#'   with special symbols like ℹ and ! for info and warning indicators.
#'
#' @return HTML object suitable for rendering in Shiny UI
#'
#' @importFrom htmltools HTML htmlEscape
#' @keywords internal
#'
#' @examples
#' warning_msg <- "There are NA values in `emission_risk_factors` dataset.\nℹ Missing values identified for the following countries:\n\"ALB\", \"ARM\", \"CYM\", \"HRV\", \"FLK\" and \"JPN\" and 63 more\n! By default, NA values are considered as having the highest level of risk."
#' cli_warning_to_html(warning_msg)
cli_warning_to_html <- function(message) {
  if (is.null(message) || message == "") {
    return(htmltools::HTML(""))
  }

  # Split message into lines
  lines <- strsplit(message, "\n")[[1]]

  # Process each line
  html_lines <- vapply(lines, function(line) {
    line <- trimws(line)
    if (line == "") return("")

    # Handle info lines (ℹ symbol)
    if (grepl("^ℹ", line)) {
      content <- gsub("^ℹ\\s*", "", line)
      return(paste0('<div class="mb-1"><i class="fas fa-info-circle text-info me-1"></i>', htmltools::htmlEscape(content), '</div>'))
    }

    # Handle warning lines (! symbol)
    if (grepl("^!", line)) {
      content <- gsub("^!\\s*", "", line)
      return(paste0('<div class="mb-1"><i class="fas fa-exclamation-triangle text-warning me-1"></i>', htmltools::htmlEscape(content), '</div>'))
    }

    # Handle regular text with code highlighting
    line <- gsub("`([^`]+)`", '<code>\\1</code>', line)
    return(paste0('<div class="mb-1">', line, '</div>'))

  }, character(1))

  # Remove empty lines and combine
  html_content <- paste(html_lines[html_lines != ""], collapse = "")

  # Always add warning symbol at start
  if (html_content != "") {
    html_content <- paste0('<div class="mb-1"><i class="fas fa-exclamation-triangle text-warning me-1"></i>Warning</div>', html_content)
  }

  htmltools::HTML(paste0('<div class="cli-warning">', html_content, '</div>'))
}
