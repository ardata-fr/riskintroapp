emissionScoresUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = .sidebar_width,
      title = titleWithHelpButton(
        key = "emission-scores-title",
        ns = ns,
        help_url = "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/emission-scores.html"
      ),
      uiOutput(ns("config_is_valid")),
      importEmissionRiskFactorsUI(ns("import_erf")),
      tags$hr(),
      div(
        class = "alert alert-info",
        style = "margin-bottom: 15px;",
        icon("info-circle"),
        " Click countries on the map to edit their underlying emission risk factors."
      ),
      # weights ----
      actionButton(
        inputId = ns("edit_weights"),
        label = "Edit Factor Weights",
        icon = icon("scale-balanced"),
        style = "margin-bottom: 10px;"
      ),
      # joker manager -----
      jokerManagerUI(
        id = ns("joker")
      ),
    ),
    navset_card_tab(
      id = ns("panel_ui"),
      nav_panel(
        title = "View map",
        leafletOutput(
          outputId = ns("map"),
          width = "100%",
          height = "80vh"
        )
      ),
      nav_panel(
        title = "View factors",
        reactableOutput(outputId = ns("factors_table"))
      ),
      nav_panel(
        title = "View scores",
        reactableOutput(outputId = ns("scores_table"))
      )
    )
  )
}

emissionScoresServer <- function(id, emission_risk_factors, updated_workspace, settings) {

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Help button ----
      observeEvent(input$open_help, {
        url <- "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/emission-scores.html"
        shinyjs::runjs(paste0("window.open('", url, "', 'help', 'width=1200,height=800,scrollbars=yes,resizable=yes');"))
      })

      # import ----
      new_erf <- importEmissionRiskFactorsServer("import_erf")
      observeEvent(new_erf(),{
        logger::log_info("Emission risk factors imported in emissionScoresServer")
        emission_risk_factors(new_erf())
      })

      country_id <- reactiveVal(NULL)
      observeEvent(input$map_shape_click, {
        country_id(input$map_shape_click$id)
      })

      observeEvent(country_id(), {
          showModal(
            riskFactorEditorUI(
              id = ns("factor_editor"),
              country_id = country_id(),
              current_weights = current_weights()
            )
          )
      })

      new_risk_factor_profile <- riskFactorEditorServer(
        id = "factor_editor",
        emission_risk_factors = emission_risk_factors,
        country_id = country_id,
        current_weights = current_weights
      )

      observeEvent(new_risk_factor_profile(), {
        country_id(NULL)
        operation <- new_risk_factor_profile()$operation
        existing_data <- emission_risk_factors()
        if (is.null(operation)) {
          # Do nothing
        } else if (operation == "upsert") {
          new_data <- new_risk_factor_profile()$data
          update_data <- dplyr::rows_upsert(
            x = existing_data,
            y = new_data,
            by = "iso3"
          )
          emission_risk_factors(update_data)
        } else if (operation == "delete") {
          update_data <- dplyr::filter(
            existing_data ,
            .data$iso3 != !!new_risk_factor_profile()$id
          )
          emission_risk_factors(update_data)
        }
      })

      # Factor weights management ----
      observeEvent(input$edit_weights, {
        showModal(emissionFactorWeightsUI(ns("weights_editor")))
      })

      current_weights <- emissionFactorWeightsServer(
        id = "weights_editor",
        current_weights = current_weights
        )

      emission_risk_result <- reactive({
        req(emission_risk_factors(), current_weights())
        logger::log_trace("Running calc_emission_risk in emissionScoresServer")
        res <- safe_and_quiet(
          .fun = riskintroanalysis::calc_emission_risk,
          emission_risk_factors = emission_risk_factors(),
          weights = current_weights(),
          keep_scores = TRUE
        )
        res
      })

      emission_scores <- reactiveVal(NULL)

      observe({
        req(!is_error(emission_risk_result()$error))
        emission_scores( emission_risk_result()$result)
      })

      # jokers -----
      new_joker_operation <- jokerManagerServer(
        id = "joker",
        emission_risk_factors = emission_risk_factors
      )
      observeEvent(new_joker_operation(), {
        operation <- new_joker_operation()

        if (is.null(operation$operation)) {
          update_data <- NULL
        } else if (operation$operation == "upsert") {
          new_data <- operation$data
          update_data <- dplyr::rows_upsert(
            x = emission_risk_factors(),
            y = new_data,
            by = "iso3"
          )
          emission_risk_factors(update_data)

        } else if (operation$operation == "open_editor") {
          country_id(operation$id)
        }
      })

      # configIsValid -----
      configIsValid <- reactive({

        warnings <- character()
        if (is_error(emission_risk_result()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while calculating emission risk scores:",
            error = emission_risk_result()$error
          )
        }
        if (is_error(emission_risk_result()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while calculating emission risk scores:",
            error = emission_risk_result()$error
          )
          return(status)
        }

        warnings <- c(warnings, emission_risk_result()$warnings)

        if (!isTruthy(emission_scores())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Emission scores have not been calculated."
          )
          return(status)
        }
        build_config_status(
          value = TRUE,
          msg = "Calculation complete.",
          warnings = warnings,
          warnings_msg = "Calculation complete with warnings:"
        )
      })
      output$config_is_valid <- renderUI({
        report_config_status(configIsValid())
      })
      outputOptions(output, "config_is_valid", suspendWhenHidden = FALSE)

      # map ----
      baseLeaflet <- reactive({basemap()})
      output$map <- renderLeaflet({
        req(baseLeaflet())
        baseLeaflet()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)

      observe({
        req(baseLeaflet())
        er <- req(emission_scores())
        plot_emission_risk_interactive(
          emission_risk = er,
          ll = leafletProxy("map")
        )
      })

      # factors_table --------
      output$factors_table <- renderReactable({
        req(emission_risk_factors())
        reactable(
          emission_risk_factors(),
          searchable = TRUE,
          filterable = TRUE,
          showPageSizeOptions = TRUE,
          defaultPageSize = 100,
          striped = TRUE
        )
      })

      # scores_table --------
      output$scores_table <- renderReactable({
        req(emission_scores())
        reactable(
          emission_scores(),
          searchable = TRUE,
          filterable = TRUE,
          showPageSizeOptions = TRUE,
          defaultPageSize = 100,
          striped = TRUE
        )
      })

      return(emission_scores)
    }
  )
}
