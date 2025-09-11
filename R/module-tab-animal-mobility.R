#' @importFrom bslib layout_sidebar sidebar navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput tags
#' @importFrom shinyWidgets dropMenu
animalMobilityUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Animal mobility",
      uiOutput(ns("config_is_valid")),
      tags$br(),
      actionButton(
        inputId = ns("import_mobility"),
        label = "Import animal mobility",
        width = '100%',
        icon = icon('file-import')
      ),
      tags$br(),
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
#' @importFrom leaflet renderLeaflet leafletProxy
#' @importFrom reactable reactable renderReactable
#' @importFrom shiny moduleServer observeEvent reactive req showModal removeModal renderUI isTruthy
#' @importFrom riskintroanalysis calc_animal_mobility_risk rescale_risk_scores
#' @importFrom purrr safely
animalMobilityServer <- function(id, input_data, epi_units, emission_scores) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Data storage ----
      animalMobilityData <- reactiveVal(NULL)
      riskScores <- reactiveVal(NULL)

      # Risk scaling arguments ----
      rescaling_args <- reactiveVal(list(
        method = "linear",
        inverse = FALSE
      ))

      # Initialize map ----
      baseLeaflet <- reactive({basemap()})
      output$map <- renderLeaflet({
        req(baseLeaflet())
        baseLeaflet()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)

      # import ----
      observeEvent(input$import_mobility, {
        showModal(importAnimalMobilityUI(ns("import")))
      })

      new_mobility_data <- importAnimalMobilityServer("import")
      observeEvent(new_mobility_data(), {
        req(new_mobility_data())
        input_data(new_mobility_data())
      })

      # calc_* ----
      observe({
        req(input_data(), epi_units(), emission_scores())

        safely_calc <- safely(calc_animal_mobility_risk)
        result <- safely_calc(
          animal_mobility = input_data(),
          emission_risk = emission_scores(),
          epi_units = epi_units(),
          method = "mean"
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

      # configIsValid ----
      configIsValid <- reactive(label = paste0("configIsValid-", id), {
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

        if (!isTruthy(input_data())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Animal mobility data must be imported."
          )
          return(status)
        }

        if (!isTruthy(riskScores())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Animal mobility risk has not been calculated."
          )
          return(status)
        }

        if (is_error(riskScores()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while calculating animal mobility risk:",
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
      outputOptions(output, "config_is_valid", suspendWhenHidden = FALSE)

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

      # map ----
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
