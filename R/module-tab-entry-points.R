#' @importFrom bslib layout_sidebar sidebar navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput tags
entryPointsUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = .sidebar_width,
      title = "Entry points",
      uiOutput(ns("config_is_valid")),
      uiOutput(ns("warnings")),
      tags$br(),
      actionButton(
        inputId = ns("import_entry_points"),
        label = "Import entry points",
        icon = icon('file-import')
      ),
      tags$br(),
      actionButton(
        inputId = ns("configure_parameters"),
        label = "Configure parameters",
        icon = icon('cogs')
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
#' @importFrom shiny moduleServer observeEvent reactive req showModal removeModal
#' @importFrom riskintroanalysis calc_entry_point_risk
entryPointsServer <- function(id, input_data, epi_units, emission_scores) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Data storage ----
      riskScores <- reactiveVal(NULL)
      entryPointsParameters <- reactiveVal(list(
        max_risk = 100,
        coef_legal = 1,
        coef_illegal = 1,
        illegal_factor = 3
      ))

      # Risk scaling arguments ----
      rescaling_args <- reactiveVal(list(
        method = "linear",
        inverse = FALSE
      ))

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
            msg = "Entry points data must be imported."
          )
          return(status)
        }

        if (!isTruthy(riskScores())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Entry points risk has not been calculated."
          )
          return(status)
        }

        if (is_error(riskScores()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while calculating entry points risk:",
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

      # warnings -----
      output$warnings <- renderUI({
        report_warning(riskScores()$warnings)
      })
      outputOptions(output, "warnings", suspendWhenHidden = FALSE)

      # Initialize maps ----
      baseMap <- reactive({ basemap() })
      output$map <- renderLeaflet({
        req(baseMap())
        baseMap()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)

      # import ----
      observeEvent(input$import_entry_points, {
        showModal(importEntryPointsUI(id = ns("import_modal")))
      })

      imported_entry_points <- importEntryPointsServer("import_modal")
      observeEvent(imported_entry_points(), {
        req(imported_entry_points())
        input_data(imported_entry_points())
      })

      # parameters ----
      observeEvent(input$configure_parameters, {
        showModal(entryPointsParametersUI(id = ns("parameters_modal")))
      })

      new_parameters <- entryPointsParametersServer("parameters_modal")
      observeEvent(new_parameters(), {
        req(new_parameters())
        entryPointsParameters(new_parameters())
      })

      # calc_* ----
      observe({
        req(input_data(), epi_units(), emission_scores())

        result <- safe_and_quiet(
          .fun = calc_entry_point_risk,
          entry_points = input_data(),
          epi_units = epi_units(),
          emission_risk = emission_scores()
          # Note: Current calc_entry_point_risk doesn't accept scaling parameters
          # These would need to be implemented in a future version of the function
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

