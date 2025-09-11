#' @import shiny
#' @importFrom leaflet renderLeaflet leafletProxy flyToBounds
#' @importFrom sf st_bbox
#' @importFrom bslib nav_select
#' @import riskintroanalysis
#' @import riskintrodata
server <- function(input, output, session) {

  # init map ----
  baseLeaflet <- reactive({basemap()})
  output$map <- renderLeaflet({
    req(baseLeaflet())
    baseLeaflet()
  })
  outputOptions(output, "map", suspendWhenHidden = FALSE)

  # Datasets -----

  epi_units <- reactiveVal(NULL)
  emission_risk_factors <- reactiveVal(NULL)

  # Workspace ----
  updated_workspace <- workspaceServer(
    id = "workspace",
    settings = list(),
    datasets = reactive(list(
      epi_units = epi_units(),
      emission_risk_factors = emission_risk_factors(),
      input_raster = input_raster()
    )),
    misc_risks = misc_risk_meta
  )

  ## Load ----
  observeEvent(updated_workspace(), ignoreInit = TRUE, {
    new_datasets <- updated_workspace()$datasets
    epi_units(new_datasets$epi_units)
    input_raster(new_datasets$input_raster)
  })

  # Import epi units ----

  observeEvent(input$import_epi_units,{
    showModal(importEpiUnitsUI(id = "import_epi_units"))
  })
  new_epi_units <- importEpiUnitsServer(
    id = "import_epi_units",
    is_overwriting = reactive(isTruthy(epi_units())))

  observeEvent(new_epi_units(),{
    epi_units(new_epi_units())
  })

  observeEvent(epi_units(), {
    epi_units <- req(epi_units())
    bb <- sf::st_bbox(epi_units)
    leaflet::flyToBounds(
      map = leafletProxy("map"),
      lng1 = bb$xmin[[1]], lat1 = bb$ymin[[1]],
      lng2 = bb$xmax[[1]], lat2 = bb$ymax[[1]]
      )
  })

  # emission_scores ----
  emission_risk_factors <- reactiveVal(NULL)
  emission_scores <- emissionScoresServer(
    id = "emission_scores",
    emission_risk_factors = emission_risk_factors,
    updated_workspace = updated_workspace,
    settings = reactive(list())
  )

  # risk tables ----
  ## misc_risks ----
  misc_risk_config <- miscRiskServer(
    id = "misc",
    epi_units = epi_units,
    updated_workspace = updated_workspace
  )
  misc_risk_table <- reactiveVal(NULL)
  misc_risk_meta <- reactiveVal(NULL)
  observeEvent(misc_risk_config(), {
    conf <- misc_risk_config()
    misc_risk_table(conf$misc_risk_table)
    misc_risk_meta(conf$misk_risk_meta)
  })

  border_risk <- borderRiskServer(
    id = "border",
    epi_units = epi_units,
    emission_scores = emission_scores
  )

  animal_mobility <- animalMobilityServer(
    id = "animal_mobility",
    epi_units = epi_units,
    emission_scores = emission_scores
  )

  input_raster <- reactiveVal(NULL)
  road_access <- roadAccessRiskServer(
    id = "road_access",
    epi_units = epi_units,
    input_raster = input_raster
  )

  entry_points <- entryPointsServer(
    id = "entry_points",
    epi_units = epi_units,
    emission_scores = emission_scores
  )

  core_risks <- reactive({
    if(
      isTruthy(border_risk()) ||
      isTruthy(animal_mobility()) ||
      isTruthy(road_access()) ||
      isTruthy(entry_points())
    ) {
      list(
        border_risk = border_risk(),
        animal_mobility = animal_mobility(),
        road_access = road_access(),
        entry_points = entry_points()
      )
    } else {
      NULL
    }
  })

  # nav_panel navigation ----
  # observeEvent(input$nav_border_risk, {
  #   nav_select(id = "navbar", selected = "nav_border_risk")
  # })
  # observeEvent(input$nav_animal_movement_risk, {
  #   nav_select(id = "navbar", selected = "nav_animal_movement_risk")
  # })
  observeEvent(input$nav_road_access_risk, {
    nav_select(id = "navbar", selected = "nav_road_access_risk")
  })
  # observeEvent(input$nav_entry_point_risk, {
  #   nav_select(id = "navbar", selected = "nav_entry_point_risk")
  # })
  observeEvent(input$nav_misc_risk, {
    nav_select(id = "navbar", selected = "nav_misc_risk")
  })

  # Risk summary map -----
  risk_table_summary <- summariseScoresServer(
    id = "summarise_risk_table",
    epi_units = epi_units,
    misc_risk_table = misc_risk_table,
    core_risks = core_risks
  )
  observe({
    req(baseLeaflet())
    req(risk_table_summary())
    plot_risk_interactive(
      dataset = risk_table_summary(),
      ll = leafletProxy("map")
    )
  })

  # Export -----
  observeEvent(input$open_export, {
    showModal(exportUI("export_module"))
    })
  exportServer(
    id = "export_module",
    files = reactive(list(
      "Epidemiological units" = epi_units(),
      "Epidemiological units with all introduction scores" = risk_table_summary(),
      "Intoduction scores table" = sf::st_drop_geometry(risk_table_summary())
    ))
  )
}
