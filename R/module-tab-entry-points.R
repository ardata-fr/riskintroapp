#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput
entryPointsUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Entry points",
      uiOutput(ns("config_is_valid")),
      actionButton(
        inputId = ns("import_entry_points"),
        label = "Import entry points",
        width = '100%',
        icon = icon('file-import')
      ),
      actionButton(
        inputId = ns("calculate_risk"),
        label = "Calculate entry points risk",
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

entryPointsServer <- function(id, epi_units) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Placeholder server logic
    }
  )
}
