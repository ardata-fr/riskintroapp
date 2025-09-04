emissionScoresUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Emission risks",
      importEmissionRiskFactorsUI(ns("import_erf"))
    ),
    leafletOutput(
      outputId = ns("map"),
      width = "100%",
      height = "85vh"
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

      observeEvent(input$map_shape_click, {
        showModal(
          riskFactorEditorUI(
            id = ns("factor_editor"),
            country_id = input$map_shape_click$id
          )
        )
      })
      new_risk_factor_profile <- riskFactorEditorServer(
        id = "factor_editor",
        emission_risk_factors = emission_risk_factors
      )

      observeEvent(new_risk_factor_profile(), {
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
          country_id <- new_risk_factor_profile()$id
          update_data <- dplyr::filter(.data$iso3 != "id")
        }
        emission_risk_factors(update_data)
      })

      factor_weights <- reactive({
        riskintrodata::emission_risk_weights
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

      baseLeaflet <- reactive({basemap()})
      output$map <- renderLeaflet({
        req(baseLeaflet())
        baseLeaflet()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)

      ## Emission risk map ----
      observe({
        req(baseLeaflet())
        er <- req(emission_scores())
        plot_emission_risk_interactive(
          emission_risk = er,
          ll = leafletProxy("map")
        )
      })

      return(emission_scores)
    }
  )
}
