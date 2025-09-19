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

  country_choices <- setNames(
    object = riskintrodata::world_sf$iso3,
    nm = paste(riskintrodata::world_sf$iso3, "-", riskintrodata::world_sf$country_name)
  )

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
        max_risk = 12,
        coef_legal = 1,
        coef_illegal = 1,
        illegal_factor = 3
      ))

      # Map event reactives for interactive editing ----
      clicky <- reactiveVal(NULL)
      observeEvent(input$map_click, {
        req(input_data(), emission_scores())
        map_click <- input$map_click
        marker_click <- input$map_marker_click
        dataset <- input_data()
        # convert floats to character for comparison
        # check if new point or editing existing point.
        if (identical(
          sprintf("%.2f", map_click[c("lat", "lng")]),
          sprintf("%.2f", marker_click[c("lat", "lng")])
        )) {
          map_click$operation <- "update"
          map_click$id <- marker_click$id
        } else {
          map_click$operation <- "create"
          new_id <- order(
            as.integer(gsub("ep-", "", unique(dataset$point_id), fixed = TRUE)), decreasing = TRUE
            )[[1]] + 1
          map_click$id <- sprintf("ep-%05i", new_id)
        }
        dat <- dataset[dataset$point_id %in% marker_click$id, ] |> sf::st_drop_geometry()
        config <- as.list(dat[1, ]) # these values are unique
        config$sources <- dat$sources  # but there are multiple sources per point
        config$source_choices <- country_choices
        clicky(map_click)
        showModal(editEntryPointsUI(id = ns("edit_entry_points"), config = config))
      })

      # Interactive editing ----
      edit_data <- editEntryPointsServer(
        id = "edit_entry_points",
        map_click = clicky
      )
      observeEvent(edit_data(), {
        operation <- edit_data()$operation
        new_data <- edit_data()$row
        out <- input_data()
        if (operation %in% c("delete", "update")) {
          out <- out[!out$point_id %in% unique(new_data$point_id), ]
        }
        if (operation %in% c("update", "create")) {
          out <- rbind(out, new_data)
        }
        input_data(out)
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
      observe({
        req(configIsValid())
        plot_risk_interactive(rescaledScores(), ll = leafletProxy("map"))
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

