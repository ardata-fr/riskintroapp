#' @import shiny
#' @importFrom leaflet renderLeaflet leafletProxy
server <- function(input, output, session) {

  # Datasets -----
  datasets <- reactiveValues(
    epi_units = NULL,
    emission_risk_factors = NULL,
    emission_risk = NULL,
    entry_points = NULL,
    animal_mobility = NULL,
    risk_table = NULL,
    shared_borders = NULL,
    road_access = NULL,
    border_risk = NULL
  )

  new_epi_units <- importEpiUnitsServer("import_epi_units")
  observeEvent(new_epi_units(),{
    datasets$epi_units <-new_epi_units()
  })
  observeEvent(datasets$epi_units, {
    epi_units <- req(datasets$epi_units)
    datasets$risk_table <- riskintroanalysis::risk_table(
      epi_units = epi_units,
      scale = c(0, 100)
    )
  })
  new_erf <- importEmissionRiskFactorsServer("import_erf")
  observeEvent(new_erf(),{
    datasets$emission_risk_factors <- new_erf()
  })

  observe({
    erf <- req(datasets$emission_risk_factors)
    datasets$emission_risk <- calc_emission_risk(
      emission_risk_factors = erf,
      weights = riskintrodata::emission_risk_weights,
      keep_scores = TRUE
    )
  })

  # Settings ----
  # Analysis settings
  settings <- reactiveVal(list())

  # nav_panel navigation ----
  observeEvent(input$nav_border_risk, {
    nav_select(id = "navbar", selected = "nav_border_risk")
  })
  observeEvent(input$nav_animal_movement_risk, {
    nav_select(id = "navbar", selected = "nav_animal_movement_risk")
  })
  observeEvent(input$nav_road_access_risk, {
    nav_select(id = "navbar", selected = "nav_road_access_risk")
  })
  observeEvent(input$nav_misc_risk, {
    nav_select(id = "navbar", selected = "nav_misc_risk")
  })
  observeEvent(input$nav_entry_point_risk, {
    nav_select(id = "navbar", selected = "nav_entry_point_risk")
  })

  # Maps ----
  baseLeafletRT <- reactive({basemap()})
  output$map_ri_summary <- renderLeaflet({
    req(baseLeafletRT())
    baseLeafletRT()
  })
  outputOptions(output, "map_ri_summary", suspendWhenHidden = FALSE)

  baseLeafletER <- reactive({basemap()})
  output$map_emission_risk <- renderLeaflet({
    req(baseLeafletER())
    baseLeafletER()
  })
  outputOptions(output, "map_emission_risk", suspendWhenHidden = FALSE)

  # Update maps ----
  ## Risk summary map -----

  risk_table_summary <- summariseRiskScoresServer(
    id = "summarise_risk_table",
    riskTable = reactive(datasets$risk_table)
  )
  observe({
    req(baseLeafletRT())
    rt <- req(risk_table_summary())
    plot_risk_interactive(
      dataset = rt,
      ll = leafletProxy("map_ri_summary")
    )
  })
  ## Emission risk map ----
  observe({
    req(baseLeafletER())
    er <- req(datasets$emission_risk)
    plot_emission_risk_interactive(
      emission_risk = er,
      ll = leafletProxy("map_emission_risk")
    )
  })

  # Workspace ----
  new_workspace <- workspaceServer(
    id = "workspace",
    settings = list(),
    datasets = datasets # Used for saving workspace
  )
  ## Load ----
  observeEvent(new_workspace(),{
    new_datasets <- new_workspace()$datasets
    to_update <- intersect(names(new_datasets), names(datasets))
    for (name in to_update){
      datasets[[name]] <- new_datasets[[name]]
    }
    to_delete <- setdiff(names(datasets), names(new_datasets))
    # delete if not already NULL
    for (name in to_delete){
      if (isTruthy(datasets[[name]])){
        datasets[[name]] <- NULL
      }
    }
    settings(new_workspace()$settings)
  })
}
