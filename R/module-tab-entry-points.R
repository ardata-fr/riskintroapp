#' @importFrom bslib layout_sidebar sidebar navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput tags
entryPointsUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = .sidebar_width,
      title = titleWithHelpKey("entry-points-title"),
      uiOutput(ns("config_is_valid")),
      # uiOutput(ns("warnings")),
      tags$br(),

      div(
        class = "alert alert-info",
        style = "margin-bottom: 15px;",
        icon("info-circle"),
        " Click on the map to add new entry points or click on existing markers to edit them."
      ),

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
      # actionButton(
      #   inputId = ns("open_risk_scaling"),
      #   label = "Edit risk scaling",
      #   icon = icon("pen-to-square")
      # )
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
#' @importFrom leaflet renderLeaflet leafletProxy addPolygons addCircleMarkers clearMarkers
#' @importFrom reactable reactable renderReactable
#' @importFrom shiny moduleServer observeEvent reactive req showModal removeModal reactiveVal
#' @importFrom riskintroanalysis calc_entry_point_risk
entryPointsServer <- function(id, input_data, epi_units, emission_scores) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

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
      entry_point_params <- reactiveVal(list(
        max_risk = 100,
        coef_legal = 1,
        coef_illegal = 1,
        illegal_factor = 3
      ))

      # Map event reactives for interactive editing ----
      map_click_reactive <- reactiveVal(NULL)
      map_marker_click_reactive <- reactiveVal(NULL)

      observeEvent(input$map_click, {
        map_click_reactive(input$map_click)
      })

      observeEvent(input$map_marker_click, {
        map_marker_click_reactive(input$map_marker_click)
      })

      # Interactive editing ----
      interactive_data <- interactiveEntryPointsEditorServer(
        id = "interactive_editor",
        input_data = input_data,
        emission_scores = emission_scores,
        map_proxy = reactive(leafletProxy("map")),
        map_click = reactive(map_click_reactive()),
        map_marker_click = reactive(map_marker_click_reactive())
      )

      # Update input_data when interactive edits occur
      observeEvent(interactive_data(), {
        req(interactive_data())
        input_data(interactive_data())
      })

      # Risk scaling arguments ----
      rescaling_args <- reactiveVal(list(
        method = "linear",
        inverse = FALSE
      ))

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
        entry_point_params(new_parameters())
      })

      # calc_* ----
      observe({
        req(input_data(), epi_units(), emission_scores(),entry_point_params())

        result <- safe_and_quiet(
          .fun = calc_entry_point_risk,
          entry_points = input_data(),
          epi_units = epi_units(),
          emission_risk = emission_scores(),
          scaling_args = entry_point_params()
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
      # Add simple markers for entry points
      observe({
        req(input_data())

        ll <- leafletProxy("map")
        entry_points <- input_data()

        # Clear existing markers
        ll |> clearMarkers()

        if (nrow(entry_points) > 0) {
          # Entry points always use sf geometry
          ll |>
            addCircleMarkers(
              data = entry_points,
              layerId = ~point_id,
              radius = 8,
              fillOpacity = 0.8,
              stroke = TRUE,
              weight = 2,
              color = "white",
              fillColor = ~ifelse(mode == "C", "#f72585", "#7209b7"),
              popup = ~paste0("<b>", point_name, "</b><br>Type: ", type, "<br>Mode: ", mode)
            )
        }
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

