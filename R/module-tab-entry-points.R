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
        inputId = ns("configure_parameters"),
        label = "Configure parameters",
        width = '100%',
        icon = icon("cogs")
      ),
      actionButton(
        inputId = ns("calculate_risk"),
        label = "Calculate entry points risk",
        width = '100%',
        icon = icon("calculator")
      ),
      actionButton(
        inputId = ns("open_risk_scaling"),
        label = "Edit risk scaling",
        width = '100%',
        icon = icon("pen-to-square")
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

#' @importFrom sf st_drop_geometry
#' @importFrom leaflet renderLeaflet leafletProxy addPolygons
#' @importFrom reactable reactable renderReactable
#' @importFrom shiny moduleServer observeEvent reactive req showModal removeModal
#' @importFrom riskintroanalysis calc_entry_point_risk
entryPointsServer <- function(id, epi_units, emission_risk_table) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      entryPointsData <- reactiveVal(NULL)
      entryPointsRiskData <- reactiveVal(NULL)
      entryPointsParameters <- reactiveVal(list(
        max_risk = 100,
        coef_legal = 1,
        coef_illegal = 1,
        illegal_factor = 3
      ))

      # configIsValid ----
      configIsValid <- reactive({
        config_is_valid(
          x = "analyse_entry_points_risk",
          epi_units = epi_units(),
          emission_risk_table = emission_risk_table(),
          entry_points_data = entryPointsData(),
          entry_points_risk_data = entryPointsRiskData(),
          rescaled_entry_points_risk = rescaledRisk()
        )
      })

      output$config_is_valid <- renderUI({
        report_config_status(configIsValid())
      })

      # Initialize maps ----
      baseMap <- reactive({ basemap() })
      output$map <- renderLeaflet({
        req(baseMap())
        baseMap()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)

      # Import entry points ----
      observeEvent(input$import_entry_points, {
        # TODO: Implement entry points import functionality
        showNotification("Entry points import not yet implemented", type = "message")
      })

      # Configure scaling parameters modal ----
      observeEvent(input$configure_parameters, {
        showModal(entryPointsParametersUI(id = ns("parameters_modal")))
      })

      new_parameters <- entryPointsParametersServer("parameters_modal")
      observeEvent(new_parameters(), {
        req(new_parameters())
        entryPointsParameters(new_parameters())
        showNotification("Entry points parameters updated", type = "message")
      })

      # Calculate entry points risk
      observeEvent(input$calculate_risk, {
        req(entryPointsData(), epi_units(), emission_risk_table())

        safely_calc_risk <- safely(calc_entry_point_risk)
        result <- safely_calc_risk(
          entry_points = entryPointsData(),
          epi_units = epi_units(),
          emission_risk = emission_risk_table()
          # Note: Current calc_entry_point_risk doesn't accept scaling parameters
          # These would need to be implemented in a future version of the function
        )
        entryPointsRiskData(result)
      })

      # Risk scaling modal
      observeEvent(input$open_risk_scaling, {
        req(entryPointsRiskData())
        showModal(rescaleRiskUI(id = ns("rescale_modal")))
      })

      new_rescaling_args <- rescaleRiskServer(
        id = "rescale_modal",
        dataset = reactive(entryPointsRiskData()$result),
        from = c(0, 100),  # Entry points typically scale to 0-100
        risk_col = "entry_points_risk",
        to = c(0, 100)
      )

      rescaledRisk <- reactive({
        req(entryPointsRiskData())
        args <- new_rescaling_args()
        if (is.null(args)) {
          return(entryPointsRiskData()$result)
        }
        rescale_risk_scores(
          dataset = entryPointsRiskData()$result,
          cols = args$cols,
          from = args$from,
          to = args$to,
          method = args$method,
          inverse = args$inverse
        )
      })

      # Update map
      observe({
        req(configIsValid())
        plot_risk_interactive(
          dataset = rescaledRisk(),
          ll = leafletProxy(mapId = "map")
        )
      })

      # Table output
      output$table <- renderReactable({
        req(configIsValid())
        risk_data <- req(rescaledRisk())
        table_data <- sf::st_drop_geometry(risk_data)
        reactable::reactable(table_data)
      })

      # Return risk data
      return(entryPointsRiskData)
    }
  )
}

#' @export
config_is_valid.analyse_entry_points_risk <- function(x, ...){
  x <- list(...)
  epi_units <- x$epi_units
  emission_risk_table <- x$emission_risk_table
  entry_points_data <- x$entry_points_data
  entry_points_risk_data <- x$entry_points_risk_data
  rescaled_entry_points_risk <- x$rescaled_entry_points_risk

  if (!isTruthy(epi_units)) {
    status <- build_config_status(
      value = FALSE,
      msg = .configIsValidMsgs$epi_units_missing
    )
    return(status)
  }

  if (!isTruthy(emission_risk_table)) {
    status <- build_config_status(
      value = FALSE,
      msg = .configIsValidMsgs$emission_risk_missing
    )
    return(status)
  }

  if (!isTruthy(entry_points_data)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Entry points dataset must be imported."
    )
    return(status)
  }

  if (!isTruthy(entry_points_risk_data)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Entry points risk has not been calculated."
    )
    return(status)
  }

  if (is_error(entry_points_risk_data$error)) {
    status <- build_config_status(
      value = FALSE,
      msg = "There was an error when calculating entry points risk."
    )
    return(status)
  }

  if (!isTruthy(entry_points_risk_data$result)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Entry points risk dataset not found."
    )
    return(status)
  }

  attrs <- attributes(entry_points_risk_data$result)
  if (!isTruthy(attrs$risk_col)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Entry points risk is missing `risk_col` attribute."
    )
    return(status)
  }
  if (!isTruthy(attrs$scale)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Entry points risk is missing `scale` attribute."
    )
    return(status)
  }
  if (!isTruthy(rescaled_entry_points_risk)) {
    status <- build_config_status(
      value = FALSE,
      msg = .configIsValidMsgs$rescale_missing
    )
    return(status)
  }

  build_config_status(
    value = TRUE,
    msg = "Analysis complete."
  )
}
