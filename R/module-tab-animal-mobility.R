#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput
animalMobilityUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Animal mobility",
      uiOutput(ns("config_is_valid")),
      actionButton(
        inputId = ns("import_mobility"),
        label = "Import animal mobility",
        width = '100%',
        icon = icon('file-import')
      ),
      actionButton(
        inputId = ns("calculate_risk"),
        label = "Calculate mobility risk",
        width = '100%',
        icon = icon("calculator")
      )
    ),
    navset_card_tab(
      id = ns("panel_ui"),
      nav_panel(
        title = "Map view",
        leafletOutput(ns("map"), width = "100%", height = "85vh")
      ),
      nav_panel(
        title = "Table view",
        reactableOutput(outputId = ns("table"))
      )
    )
  )
}

animalMobilityServer <- function(id, epi_units, emission_scores) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Placeholder server logic
    }
  )
}
