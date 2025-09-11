#' @importFrom bslib layout_sidebar sidebar navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput tags
borderRiskUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Border risk",
      uiOutput(ns("config_is_valid")),
      tags$br(),
      actionButton(
        inputId = ns("calculate_border_lengths"),
        label = "Calculate shared borders",
        icon = icon("map")
      ),
      tags$br(),
      actionButton(
        inputId = ns("open_risk_scaling"),
        label = "Edit risk scaling",
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
#' @importFrom shiny moduleServer observeEvent reactive req
#' @importFrom riskintroanalysis calc_border_lengths calc_border_risk
borderRiskServer <- function(id, epi_units, emission_scores) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Data storage ----
      sharedBorders <- reactiveVal(NULL)
      riskScores <- reactiveVal(NULL)

      # Risk scaling arguments ----
      rescaling_args <- reactiveVal(list(
        method = "linear",
        inverse = FALSE
      ))

      # configIsValid ----
      configIsValid <- reactive({
        if (!isTruthy(epi_units())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Epidemiological units dataset must be imported."
          )
          return(status)
        }

        if (!isTruthy(emission_scores())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Emission risk scores must be calculated."
          )
          return(status)
        }

        if (!isTruthy(sharedBorders())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Shared borders has not been calculated,
            click the button below to start."
          )
          return(status)
        }

        if (is_error(sharedBorders()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "There was an error calculating shared borders:",
            error = sharedBorders()$error
          )
          return(status)
        }

        if (!isTruthy(riskScores())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Border risk has not been calculated."
          )
          return(status)
        }

        if (is_error(riskScores()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while calculating border risk:",
            error = riskScores()$error
          )
          return(status)
        }

        if (!isTruthy(rescaledScores())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Risk scores cannot be rescaled."
          )
          return(status)
        }

        build_config_status(
          value = TRUE,
          msg = "Configuration is valid."
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

      # borders ----
      observeEvent(input$calculate_border_lengths, {
        safely_calc_borders <- safely(calc_border_lengths)
        result <- safely_calc_borders(epi_units = epi_units())
        sharedBorders(result)
      })

      # calc_* ----
      observe({
        req(sharedBorders(), !is_error(sharedBorders()$error))
        req(epi_units(), emission_scores())

        safely_calc_risk <- safely(calc_border_risk)
        result <- safely_calc_risk(
          epi_units = epi_units(),
          shared_borders = sharedBorders()$result,
          emission_risk = emission_scores()
        )
        riskScores(result)
      })

      # rescaling ----
      observeEvent(input$open_risk_scaling, {
        req(riskScores())
        req(!is_error(riskScores()$error))
        showModal(rescaleRiskUI(id = ns("rescale_modal")))
      })

      new_rescaling_args <- rescaleRiskServer(
        id = "rescale_modal",
        dataset = reactive(riskScores()$result)
      )
      observeEvent(new_rescaling_args(), {
        rescaling_args(new_rescaling_args())
      })

      rescaledScores <- reactive({
        if (!isTruthy(riskScores()$result)) {
          return(NULL)
        }
        args <- req(rescaling_args())
        rescale_risk_scores(
          dataset = riskScores()$result,
          method = args$method,
          inverse = args$inverse
        )
      })

      # map ----

      # table ----
      output$table <- renderReactable({
        req(configIsValid())
        reactable::reactable(
          rescaledScores(),
          searchable = TRUE,
          filterable = TRUE,
          showPageSizeOptions = TRUE,
          defaultPageSize = 100,
          striped = TRUE
        )
      })

      # returnData ----
      returnData <- reactiveVal(NULL)
      observe({
        req(configIsValid())
        plot_risk_interactive(
          dataset = rescaledScores(),
          ll = leafletProxy("map")
        )
        returnData(rescaledScores())
      })

      returnData
    }
  )
}

