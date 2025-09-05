#' @importFrom rlang is_string
is_valid_name <- function(text) {
  if (!is_string(text)) return(FALSE)
  # Pattern matches any character that is NOT alphanumeric, space, or underscore
  !grepl("[^A-Za-z0-9\\s_]", text, perl = TRUE)
}
