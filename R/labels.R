

# div(
#   tags$label(
#     class = "control-label",
#     `for` = ns("test_parameters"),
#     get_label("ttest-independent-test_parameters", "english")
#   ),
#   helpPopup(get_help("ttest-independent-test_parameters", "english"))
# ),
# multipleColumnSelectionInput(
#   inputId = ns("test_parameters"),
#   label = NULL,
#   choices = character(0),
#   selected = character(0),
#   actionsBox = TRUE
# )

#' Title with inline help tooltip
#' @param title The title text
#' @param help_text The help text for the tooltip
#' @param level Header level (1-6), defaults to 4
#' @importFrom bslib tooltip
#' @importFrom htmltools div HTML
#' @noRd
titleWithHelp <- function(title, help_text, level = 4) {
  header_tag <- switch(as.character(level),
    "1" = h1,
    "2" = h2,
    "3" = h3,
    "4" = h4,
    "5" = h5,
    "6" = h6,
    h4  # default
  )

  div(
    style = "display: inline-flex; align-items: center; gap: 8px;",
    header_tag(title, style = "margin: 0;"),
    helpPopup(help_text)
  )
}

#' Label with inline help tooltip
#' @param label The label text
#' @param help_text The help text for the tooltip
#' @importFrom bslib tooltip
#' @importFrom htmltools span HTML
#' @noRd
labelWithHelp <- function(label, help_text) {
  span(
    style = "display: inline-flex; align-items: center; gap: 5px;",
    label,
    helpPopup(help_text)
  )
}

#' Title using label keys
#' @param key Key for the thing being labeled
#' @param level Header level (1-6), defaults to 4
#' @param lang Language code, defaults to "en"
#' @noRd
titleWithHelpKey <- function(key,level = 4, lang = "en") {
  title_text <- get_label(key, lang)
  help_text <- get_help(key, lang)
  titleWithHelp(title_text, help_text, level)
}

#' Label using label keys
#' @param label_key Key for thing being labeled
#' @param lang Language code, defaults to "en"
#' @noRd
labelWithHelpKey <- function(key, lang = "en") {
  label_text <- get_label(key, lang)
  help_text <- get_help(key, lang)
  labelWithHelp(label_text, help_text)
}

#' @importFrom bslib tooltip
helpPopup <- function(text){
  tooltip(
    icon("circle-question"),
    HTML(paste0("<div style=\"text-align:left;\">",
                text, "</div>")), placement = "right"
  )
}

get_label <- function(key, lang = "en"){
  lang_subset <- labels_int[labels_int$lang == lang, ]
  matched_indices <- match(key, lang_subset$key)
  lang_subset$label[matched_indices]
}

get_help <- function(key, lang = "en"){
  lang_subset <- labels_int[labels_int$lang == lang, ]
  matched_indices <- match(key, lang_subset$key)
  lang_subset$help[matched_indices]
}

