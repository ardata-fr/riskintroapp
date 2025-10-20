#' @importFrom bslib layout_sidebar sidebar navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput tags
#' @importFrom shinyWidgets dropMenu
animalMobilityUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = .sidebar_width,
      title = titleWithHelpButton(
        key = "animal-mobility-title",
        ns = ns,
        help_url = "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/animal-mobility-analysis.html"
      ),
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
animalMobilityServer <- function(id, input_data, epi_units, emission_scores, saved_config) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Help button ----
      observeEvent(input$open_help, {
        url <- "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/animal-mobility-analysis.html"
        shinyjs::runjs(paste0("window.open('", url, "', 'help', 'width=1200,height=800,scrollbars=yes,resizable=yes');"))
      })

      # saved_config -----
      observeEvent(saved_config(), {
        rescaling_args(saved_config()$rescaling_args)
      })

      # init map ----
      baseLeaflet <- reactive({basemap()})
      output$map <- renderLeaflet({
        req(baseLeaflet())
        baseLeaflet()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)
      observeEvent(epi_units(), {
        req(epi_units())
        setBoundsFromSF(leafletProxy("map"), epi_units())
      })

      # Data storage ----
      animalMobilityData <- reactiveVal(NULL)
      riskScores <- reactiveVal(NULL)

      # Risk scaling arguments ----
      rescaling_args <- reactiveVal(list(
        method = "linear",
        inverse = FALSE,
        reverse = FALSE,
        to = c(0, 100)
      ))

      # import ----
      observeEvent(input$import_mobility, {
        showModal(importAnimalMobilityUI(ns("import")))
      })

      new_mobility_data <- importAnimalMobilityServer("import")
      observeEvent(new_mobility_data(), {
        req(new_mobility_data())
        logger::log_info("Animal mobility data imported in animalMobilityServer")
        input_data(new_mobility_data())
      })

      # calc_* ----
      observe({
        req(input_data(), epi_units(), emission_scores())

        logger::log_trace("Running calc_animal_mobility_risk in animalMobilityServer")
        result <- safe_and_quiet(
          .fun = calc_animal_mobility_risk,
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
          inverse = args$inverse,
          reverse = args$reverse,
          to = args$to
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
            msg = "Emission scores must be provided."
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
