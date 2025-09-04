emissionScoresUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Emission risks",
      importEmissionRiskFactorsUI(ns("import_erf")),

      tags$hr(),

      # weights ----
      actionButton(
        inputId = ns("edit_weights"),
        label = "Edit Factor Weights",
        icon = icon("scale-balanced"),
        style = "margin-bottom: 10px;"
      )
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

emissionScoresServer <- function(id, updated_workspace, settings) {

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      emission_risk_factors <- reactiveVal(NULL)
      emission_scores <- reactiveVal(NULL)

      # update_workspace -----
      observeEvent(updated_workspace(), ignoreInit = TRUE, ignoreNULL = TRUE, {
        ws <- updated_workspace()
        emission_risk_factors(ws$datasets$emission_risk_factors)
      })


      new_erf <- importEmissionRiskFactorsServer("import_erf")
      observeEvent(new_erf(),{
        emission_risk_factors(new_erf())
      })

      country_id <- reactiveVal(NULL)
      observeEvent(input$map_shape_click, {
        showModal(
          riskFactorEditorUI(
            id = ns("factor_editor"),
            country_id = input$map_shape_click$id
          )
        )
        country_id(input$map_shape_click$id)
      })
      new_risk_factor_profile <- riskFactorEditorServer(
        id = "factor_editor",
        emission_risk_factors = emission_risk_factors,
        country_id = country_id
      )

      observeEvent(new_risk_factor_profile(), {
        country_id(NULL)
        operation <- new_risk_factor_profile()$operation
        existing_data <- emission_risk_factors()
        if (is.null(operation)) {
          update_data <- NULL
        } else if (operation == "upsert") {
          new_data <- new_risk_factor_profile()$data
          update_data <- dplyr::rows_upsert(
            x = existing_data,
            y = new_data,
            by = "iso3"
          )
        } else if (operation == "delete") {
          update_data <- dplyr::filter(
            existing_data ,
            .data$iso3 != !!new_risk_factor_profile()$id
          )
        }
        emission_risk_factors(update_data)
      })

      # Factor weights management ----
      current_weights <- reactiveVal(riskintrodata::get_erf_weights())

      factor_weights <- reactive({
        current_weights()
      })

      # Handle weights editor
      observeEvent(input$edit_weights, {
        showModal(emissionFactorWeightsUI(ns("weights_editor")))
      })

      weights_result <- emissionFactorWeightsServer("weights_editor", current_weights)
      observeEvent(weights_result(), {
        new_weights <- weights_result()
        if (!is.null(new_weights)) {
          current_weights(new_weights)
        }
      })

      observe({
        req(emission_risk_factors(), factor_weights())
        emission_scores(
          calc_emission_risk(
            emission_risk_factors = emission_risk_factors(),
            weights = factor_weights(),
            keep_scores = TRUE
          )
        )
      })

      ## map ----
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
          defaultPageSize = 25,
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
          defaultPageSize = 25,
          striped = TRUE
        )
      })

      return(emission_scores)
    }
  )
}
