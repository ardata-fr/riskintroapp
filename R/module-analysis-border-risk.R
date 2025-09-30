#' @importFrom bslib layout_sidebar sidebar navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput tags
borderRiskUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = .sidebar_width,
      title = titleWithHelpKey("border-risk-title"),
      uiOutput(ns("config_is_valid")),
      uiOutput(ns("warnings")),
      tags$br(),
      bslib::input_task_button(
        id = ns("calc"),
        label = "Calculate shared borders",
        icon = icon("map"),
        label_busy = "Calculating..."
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
#' @importFrom shiny moduleServer observeEvent reactive req updateActionButton
#' @importFrom riskintroanalysis calc_border_lengths calc_border_risk
#' @importFrom bslib bind_task_button
#' @importFrom mirai mirai
#' @importFrom shiny ExtendedTask
#' @importFrom shinyWidgets show_toast
borderRiskServer <- function(id, input_data, epi_units, emission_scores, saved_config) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # saved_config -----
      observeEvent(saved_config(), {
        rescaling_args(saved_config()$rescaling_args)
      })

      # init maps ----
      baseMap <- reactive({ basemap() })
      output$map <- renderLeaflet({
        req(baseMap())
        baseMap()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)
      observeEvent(epi_units(), {
        req(epi_units())
        setBoundsFromSF(leafletProxy("map"), epi_units())
      })

      # Data storage ----
      riskScores <- reactiveVal(NULL)

      # Risk scaling arguments ----
      rescaling_args <- reactiveVal(list(
        method = "linear",
        inverse = FALSE
      ))

      # Setup async "function" ----
      async_calc_borders <- ExtendedTask$new(
        function(x) {
          mirai::mirai(.expr = {
            safely_calc_borders <- purrr::safely(riskintroanalysis::calc_border_lengths)
            result <- safely_calc_borders(epi_units = x)
            result
          },
          .args = list(x = x)
          )
          }
      ) |> bind_task_button("calc")


      # Button click starts process
      observeEvent(input$calc, {
        req(epi_units())
        async_calc_borders$invoke(x = epi_units())
      })
      # Reactive monitors process
      new_input_data <- reactive({
        async_calc_borders$result()
      })
      # Once new value arrives, replace input_data()
      observeEvent(new_input_data(), {
        show_toast(
          "Border risk has finished calculating.",
          type = "info",
          timer = 5000
          )
        input_data(new_input_data())
      })

      # calc_* ----
      observe({
        req(input_data())
        req(!is_error(input_data()$error))
        req(epi_units(), emission_scores())
        logger::log_trace("Running calc_border_risk in borderRiskServer")
        result <- safe_and_quiet(
          .fun = calc_border_risk,
          epi_units = epi_units(),
          shared_borders = input_data()$result,
          emission_risk = emission_scores()
        )
        riskScores(result)
      })

      # rescaling ----
      observeEvent(input$open_risk_scaling, {
        req(riskScores())
        req(!is_error(riskScores()$error))
        showModal(rescaleRiskUI(id = ns("rescale_modal"), rescaling_args = rescaling_args()))
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

        warnings <- character()
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
            msg = "Shared borders has not been calculated, click the button above to start."
          )
          return(status)
        }
        if (is_error(input_data()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "There was an error calculating shared borders:",
            error = input_data()$error
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

        if (isTruthy(riskScores()$warnings)) {
          warnings <- c(warnings, riskScores()$warnings)
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
          msg = "Analysis complete.",
          warnings = warnings,
          warnings_msg = "Analysis complete with warnings:"
        )
      })
      output$config_is_valid <- renderUI({
        report_config_status(configIsValid())
      })

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
        returnData(list(
          dataset = rescaledScores(),
          config = list(
            rescaling_args = rescaling_args()
          )
        ))
      })

      returnData
    }
  )
}

