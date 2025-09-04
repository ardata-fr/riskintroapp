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

      observeEvent(input$map_click, {
        click <- input$map_click
        lat <- click$lat
        lng <- click$lng
        browser()
      })

      observe({
        erf <- req(emission_risk_factors())
        emission_scores(
          calc_emission_risk(
            emission_risk_factors = erf,
            weights = riskintrodata::emission_risk_weights,
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
