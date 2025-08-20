#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput
roadAccessRiskUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Road access",
      uiOutput(ns("config_is_valid")),
      actionButton(
        inputId = ns("download_raster"),
        label = "Download road access raster",
        width = '100%',
        icon = icon('download')
      ),
      actionButton(
        inputId = ns("calculate_risk"),
        label = "Calculate road access risk",
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

roadAccessRiskServer <- function(id, epi_units) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Placeholder server logic
    }
  )
}
