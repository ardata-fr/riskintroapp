.configIsValidMsgs <- list(
  rescale_missing = "Rescaling parameters have not been set.",
  epi_units_missing = "Epidemiological units dataset must be imported.",
  emission_risk_missing = "Emission scores dataset must be imported."
)

#' @title Build a Configuration Status
#' @description
#' Build a configuration status from a boolean and a message to display alongside
#' it.
#' @param value boolean value, FALSE for something wrong
#' in the configuration. TRUE for success.
#' @param msg string value, its purpose is to be displayed
#' if `value` is FALSE or TRUE.
#' @param error error object or string to display if value is FALSE and there is an error. Often the
#' `res$error` of [safe_and_quiet()].
#' @param warnings character vector of warnings. Often the
#' `res$warnings` of [safe_and_quiet()].
#' @param warnings_msg Alternative msg value for when value is TRUE and warnings are present.
#' @examples
#' build_config_status(value = TRUE, msg = "something is wrong")
#' @keywords internal
#' @seealso [report_config_status()]
build_config_status <- function(value, msg, error = NULL, warnings = NULL, warnings_msg = NULL) {
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
  if (!is.null(warnings_msg) > 0) {
    attr(x, "warnings_msg") <- warnings_msg
  }
  x
}
#' @importFrom rlang is_string
is_non_empty_string <- function(x) {
  is_string(x) && nzchar(x)
}
#' @importFrom rlang is_logical
is_non_empty_single_logical <- function(x) {
  is_logical(x) && length(x) == 1 && !is.na(x)
}

#' @title Report a Configuration Status
#' @description
#' Render a simple Shiny-friendly UI element that shows
#' a green checkmark with the message if valid, or a red cross
#' with the message if invalid.
#' @param status an object created by `build_config_status`
#' @return if `status` is not null a shiny::tagList containing an icon and message.
#' If `status` is NULL returns NULL for ease of use in [shiny::renderUI()].
#' @examples
#' status <- build_config_status(TRUE, "All good")
#' report_config_status(status)
#' @keywords internal
#' @importFrom shinyWidgets panel
report_config_status <- function(status) {
  if (is.null(status)) return(NULL)
  stopifnot(is.logical(status), length(status) == 1L)

  msg <- attr(status, "comment") %||% ""
  error <- attr(status, "error")
  warnings <- attr(status, "warnings")
  warnings_msg <- attr(status, "warnings_msg") %||% msg

  if (isTRUE(status) && is.null(warnings)) {
    shinyWidgets::panel(
      status = "success",
      shiny::icon("circle-check", , class = "text-success"),
      shiny::span(msg, style = "margin-left: 5px; margin-right: 5px;")
    )
  } else if (isTRUE(status) && !is.null(warnings)) {
    warn_divs <- lapply(as.list(warnings), function(x) {
      format_cli_warning_to_html(x)
    })
    shinyWidgets::panel(
      status = "success",
      div(
        shiny::icon("circle-check", , class = "text-success"),
        shiny::span(
          warnings_msg,
          style = "margin-left: 5px; margin-right: 5px;"
          )
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
  res <- safe_fun(...)
  out <- list(
    result = res$result$result,
    error = res$result$error,
    output = res$output,
    warnings = res$warnings,
    messages = res$messages
  )
  if(!is.null(out$error)){
    out$error <- get_error_message(out$error)
  }
  # out <- map(out, length_zero_to_na)
  out
}

#' Convert CLI Warning Message to HTML
#'
#' Converts CLI-formatted warning messages from riskintroanalysis into HTML
#' suitable for display in Shiny applications.
#' @param message Character string containing CLI-formatted warning message
#'   with special symbols like ℹ and ! for info and warning indicators.
#' @return HTML object suitable for rendering in Shiny UI
#' @importFrom htmltools HTML htmlEscape
#' @keywords internal
format_cli_warning_to_html <- function(message) {
  if (is.null(message) || message == "") {
    return(htmltools::HTML(""))
  }

  # Add ! to the first warning if there is none
  if (!grepl(pattern = "^!", message[[1]])) {
    message[[1]] <- gsub(pattern = "^", replacement = "! ", message[[1]])
  }

  # Split message into lines
  lines <- strsplit(message, "\n")[[1]]

  # Remove empty lines and combine first
  html_content <- paste(lines[lines != ""], collapse = " ")

  # Clean up all whitespace (including mid-sentence newlines)
  html_content <- gsub("\\s+", " ", html_content)
  html_content <- trimws(html_content)

  # Add strategic line breaks BEFORE converting to HTML icons
  # Break before any info or warning symbol
  html_content <- gsub("(\\S)\\s+(\u2139|!)", "\\1\n\\2", html_content)
  # Break before country list
  html_content <- gsub("(:)\\s+(\")", "\\1\n  \\2", html_content)

  # NOW handle backticks and convert to HTML icons
  html_content <- gsub("`([^`]+)`", '<code>\\1</code>', html_content)

  # Replace i with info icon (preserve line breaks)
  html_content <- gsub("(^|\n)\u2139", '\\1<i class="fas fa-info-circle text-info me-1"></i>', html_content)
  # Replace warning with warning icon (preserve line breaks)
  html_content <- gsub("(^|\n)!", '\\1<i class="fas fa-exclamation-triangle text-warning me-1"></i>', html_content)

  # Convert line breaks to HTML
  html_content <- gsub("\n", "<br>", html_content)

  # Wrap in div
  html_content <- paste0('<div class="mb-1">', html_content, '</div>')

  htmltools::HTML(html_content)
}

#' @title Extract the Message from a Try-Error or Error Object
#' @description
#' Retrieves the error message from a \code{try-error} or an error object. If the error originates
#' from \pkg{rlang}, the CLI formatting is disabled before extracting the message.
#' @param x A \code{try-error} or an error object (e.g., an object of class \code{"error"} or \code{"rlang_error"}).
#' @return
#' A character string containing the error message.
#' @keywords internal
#' @importFrom stringr str_squish str_trim str_extract str_locate fixed
#' str_sub
get_error_message <- function(x) {
  if (is.character(x)) return(paste("Error:", x))
  if (inherits(x, "try-error")) {
    x <- attr(x, "condition")
  }

  if (inherits(x, "rlang_error")) {
    x$use_cli_format <- FALSE
  }
  str <- conditionMessage(x)
  if (grepl("error_call = current_env()) :", str, fixed = TRUE)) {
    last_details_loc <- str_locate(
      string = str,
      pattern = fixed("error_call = current_env()) :")
    )
    last_details_loc <- as.vector(last_details_loc)
    last_details <- str_sub(str, start = last_details_loc[2] + 1)
    last_details <- str_squish(last_details)
    str <- str_trim(last_details)
  }

  gsub("simpleError\\: ", "", str)
}

#' @title Checks if an Object is an Error
#' @description
#' Determines whether an object is an error, specifically if it inherits from
#' the classes `error` or `try-error`. For use with resulting values of [safe_and_quite()].
#' @param x The object to test. Can be of any type.
#' @keywords internal
#' @importFrom rlang inherits_any
is_error <- function(x) {
  inherits_any(x, c("error", "try-error"))
}


