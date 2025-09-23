#' @importFrom bslib bs_theme bs_add_rules font_google
#' @title CIRAD Bootstrap Theme
#' @description
#' This function creates a Bootstrap theme for CIRAD.
#' @keywords internal
bs_theme_cirad <- function() {

  theme <- bs_theme(
    version = 5,
    base_font = "Open Sans",
    heading_font = "Open Sans"
  )

  bslib::bs_add_variables(
    theme = theme,
    "tooltip-max-width" = "500px"
  )

  # For dragulaInput theme
  bslib::bs_add_rules(
    theme = theme,
    c(
      ".label-primary { @extend .badge }",
      ".label-primary { @extend .bg-primary }"
    )
  )
}

#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tagList tags singleton HTML
#' @title App Header UI
#' @description
#' This function creates the header for the RiskIntroApp, including scripts,
#' styles, and custom message handlers.
#' @keywords internal
app_header <- function() {
  tagList(
    tags$script(src = "riskintro/js/title-change.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "riskintro/css/styles.css"),
    tags$script(src = "riskintro/js/leaflet-utils.js"),
    tags$script(HTML("
          Shiny.addCustomMessageHandler('update-title', function(message) {
          document.getElementById('navbar-title').innerHTML = message;
        });
      ")),
    tags$style(".dropdown-menu {z-index: 10000;}"),
    tags$style(".sidebar {overflow: visible !important;}"),
    tags$style(HTML("
          #navbar-title {
            margin: 0 !important;
            padding: 0 !important;
            line-height: 1 !important;
            display: inline-flex !important;
            align-items: center !important;
          }
        ")),

    # Causes clicked links generated from labels.yaml to no close
    # current session, and instead open links in new tab.
    tags$script(HTML("
              document.addEventListener('DOMContentLoaded', function() {
                const links = document.querySelectorAll('a[href^=\"http\"]');
                links.forEach(link => {
                link.setAttribute('target', '_blank');
              });
            });
        ")),
    tags$head(
      singleton(
        tags$head(
          tags$link(rel = "shortcut icon", href = "riskintro/favicon.ico"),
        )
      )
    ),
    useShinyjs(),
    busyUI("busy")
  )
}

