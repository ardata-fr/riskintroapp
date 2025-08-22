#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput
borderRiskUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Border risk",
      uiOutput(ns("config_is_valid")),
      actionButton(
        inputId = ns("calculate_border_lengths"),
        label = "Calculate shared borders",
        width = '100%',
        icon = icon("map")
      ),
      actionButton(
        inputId = ns("calculate_risk"),
        label = "Calculate border risk",
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
#' @importFrom shiny moduleServer observeEvent reactive req showNotification
#' @importFrom riskintroanalysis calc_border_lengths calc_border_risk
borderRiskServer <- function(id, epi_units, emission_risk_table) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Data storage ----
      sharedBorders <- reactiveVal(NULL)
      borderRiskData <- reactiveVal(NULL)

      # Configuration validation ----
      configIsValid <- reactive({
        config_is_valid_border_risk(
          epi_units = epi_units(),
          emission_risk_table = emission_risk_table(),
          shared_borders = sharedBorders(),
          border_risk_data = borderRiskData(),
          rescaled_border_risk = rescaledRisk()
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

      # Calculate shared borders ----
      observeEvent(input$calculate_border_lengths, {
        safely_calc_borders <- safely(calc_border_lengths)
        result <- safely_calc_borders(epi_units = epi_units())
        sharedBorders(result)
      })

      # Calculate border risk ----
      observeEvent(input$calculate_risk, {
        shared_borders <- req(sharedBorders())
        safely_calc_risk <- safely(calc_border_risk)
        result <- safely_calc_risk(
          epi_units = epi_units(),
          shared_borders = shared_borders$result,
          emission_risk = emission_risk_table()
        )
        borderRiskData(result)
      })

      # Risk scaling ----
      observeEvent(input$open_risk_scaling, {
        req(borderRiskData())
        showModal(rescaleRiskUI(id = ns("rescale_modal")))
      })
      new_rescaling_args <- rescaleRiskServer(
        id = "rescale_modal",
        dataset = reactive(borderRiskData()$result),
        from = c(0, 12),
        risk_col = "border_risk",
        to = c(0, 100)
      )
      rescaledRisk <- reactive({
        args <- req(new_rescaling_args())
        rescale_risk_scores(
          dataset = borderRiskData()$result,
          cols = args$cols,
          from = args$from,
          to = args$to,
          method = args$method,
          inverse = args$inverse
        )
      })

      # Update map ----
      observe({
        req(configIsValid())
        plot_risk_interactive(
          dataset = rescaledRisk(),
          ll = leafletProxy(mapId = "map")
        )
      })

      # Table output ----
      output$table <- renderReactable({
        req(configIsValid())
        risk_data <- req(rescaledRisk())
        table_data <- sf::st_drop_geometry(risk_data)
        reactable::reactable(table_data)
      })

      # Return risk data ----
      return(borderRiskData)
    }
  )
}


config_is_valid_border_risk <- function(
    epi_units,
    emission_risk_table,
    shared_borders,
    border_risk_data,
    rescaled_border_risk
){

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

  if (!isTruthy(shared_borders)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Shared borders has not been calculated, click the button below."
    )
    return(status)
  }

  if (is_error(shared_borders$error)) {
    status <- build_config_status(
      value = FALSE,
      msg = paste("There was an error calculating shared borders,",
      "please save your workspace and send to RiskIntroApp maintainer.",
      "See the 'About' tab for contact information.")
    )
    return(status)
  }

  if (!isTruthy(shared_borders$result)) {
    status <- build_config_status(
      value = FALSE,
      msg = paste("Shared borders dataset is missing,",
                  "please save your workspace and send to RiskIntroApp maintainer.",
                  "See the 'About' tab for contact information.")
    )
    return(status)
  }

  if (!isTruthy(border_risk_data)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Border risk has not been calculated."
    )
    return(status)
  }

  if (is_error(border_risk_data$error)) {
    status <- build_config_status(
      value = FALSE,
      msg = "There was an error when calculating border risk."
    )
    return(status)
  }

  if (!isTruthy(border_risk_data$result)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Border risk dataset not found."
    )
    return(status)
  }

  attrs <- attributes(border_risk_data$result)
  if (!isTruthy(attrs$risk_col)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Border risk is missing `risk_col` attribute."
    )
    return(status)
  }
  if (!isTruthy(attrs$scale)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Border risk is missing `scale` attribute."
    )
    return(status)
  }
  if (!isTruthy(rescaled_border_risk)) {
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
