#' @import shiny
#' @importFrom leaflet renderLeaflet leafletProxy flyToBounds
#' @importFrom sf st_bbox
#' @importFrom bslib nav_select
#' @import riskintroanalysis
#' @import riskintrodata
#' @importFrom shinyjs runjs
server <- function(input, output, session) {

  # Help button ----
  observeEvent(input$open_intro_risk_help, {
    url <- "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/introduction-risk.html"
    shinyjs::runjs(paste0("window.open('", url, "', 'help', 'width=1200,height=800,scrollbars=yes,resizable=yes');"))
  })

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
    core_config = core_config,
    datasets = reactive(list(
      epi_units = epi_units(),
      emission_risk_factors = emission_risk_factors(),
      input_raster = input_raster(),
      animal_mobility = animal_mobility_input(),
      entry_points = entry_points_input(),
      shared_borders = border_input()$result,
      overwriter_data = overwriter_data()
    )),
    misc_risks = misc_risk_meta
  )

  ## Load ----
  observeEvent(updated_workspace(), ignoreInit = TRUE, {
    new_datasets <- updated_workspace()$datasets
    dataset_names <- names(new_datasets)[!sapply(new_datasets, is.null)]
    logger::log_info("Workspace imported with datasets: {quote_and_collapse(dataset_names)}")
    epi_units(new_datasets$epi_units)
    emission_risk_factors(new_datasets$emission_risk_factors)
    input_raster(new_datasets$input_raster)
    animal_mobility_input(new_datasets$animal_mobility)
    entry_points_input(new_datasets$entry_points)
    border_input(list(result = new_datasets$shared_borders, error = NULL))
    overwriter_data(new_datasets$overwriter_data)
  })

  # import geodata ----

  observeEvent(input$open_geodata, {
    hideDropMenu("dropMenu_dropmenu")
    showModal(geodataUI(id = "geodata"))
  })
  geodata_import <- geodataServer(
    id = "geodata",
    is_overwriting = reactive(isTruthy(epi_units()))
    )
  observeEvent(geodata_import(), {
    logger::log_info("Epidemiological units imported from geodata")
    epi_units(geodata_import())
  })

  # import file ----
  observeEvent(input$import_epi_units,{
    hideDropMenu("dropMenu_dropmenu")
    showModal(importEpiUnitsUI(id = "import_epi_units"))
  })
  new_epi_units <- importEpiUnitsServer(
    id = "import_epi_units",
    is_overwriting = reactive(isTruthy(epi_units()))
    )

  observeEvent(new_epi_units(),{
    logger::log_info("Epidemiological units imported from file")
    epi_units(new_epi_units())
  })

  observeEvent(epi_units(), {
    req(epi_units())
    setBoundsFromSF(leafletProxy("map"), epi_units())
  })

  # emission_scores ----
  emission_risk_factors <- reactiveVal(NULL)
  emission_scores <- emissionScoresServer(
    id = "emission_scores",
    emission_risk_factors = emission_risk_factors,
    settings = reactive(list())
  )

  # risk tables ----
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

  border_input <- reactiveVal()
  border_risk <- borderRiskServer(
    id = "border",
    input_data = border_input,
    epi_units = epi_units,
    emission_scores = emission_scores,
    saved_config = reactive({
      updated_workspace()$settings$border_risk
    })
  )

  animal_mobility_input <- reactiveVal()
  animal_mobility <- animalMobilityServer(
    id = "animal_mobility",
    input_data = animal_mobility_input,
    epi_units = epi_units,
    emission_scores = emission_scores,
    saved_config = reactive({
      updated_workspace()$settings$animal_mobility
    })
  )

  input_raster <- reactiveVal(NULL)
  road_access <- roadAccessRiskServer(
    id = "road_access",
    epi_units = epi_units,
    input_raster = input_raster,
    saved_config = reactive({
      updated_workspace()$settings$road_access
    })
  )

  entry_points_input <- reactiveVal()
  entry_points <- entryPointsServer(
    id = "entry_points",
    input_data = entry_points_input,
    epi_units = epi_units,
    emission_scores = emission_scores,
    saved_config = reactive({
      updated_workspace()$settings$road_access
    })
  )

  core_config <- reactive({
    if(
      isTruthy(border_risk()) ||
      isTruthy(animal_mobility()) ||
      isTruthy(road_access()) ||
      isTruthy(entry_points())
    ) {
      list(
        border_risk = border_risk()$config,
        animal_mobility = animal_mobility()$config,
        road_access = road_access()$config,
        entry_points = entry_points()$config
      )
    } else {
      NULL
    }
  })

  core_risks <- reactive({
    if(
      isTruthy(border_risk()) ||
      isTruthy(animal_mobility()) ||
      isTruthy(road_access()) ||
      isTruthy(entry_points())
    ) {
      list(
        border_risk = border_risk()$dataset,
        animal_mobility = animal_mobility()$dataset,
        road_access = road_access()$dataset,
        entry_points = entry_points()$dataset
      )
    } else {
      NULL
    }
  })

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
  observeEvent(input$nav_entry_point_risk, {
    nav_select(id = "navbar", selected = "nav_entry_point_risk")
  })
  observeEvent(input$nav_misc_risk, {
    nav_select(id = "navbar", selected = "nav_misc_risk")
  })

  # overwriter -----
  overwriter_data <- reactiveVal(NULL)
  observeEvent(input$map_shape_click, {
    ir <- req(intro_risk())
    ir <- sf::st_drop_geometry(ir)
    ir <- ir[ir$eu_id %in% input$map_shape_click$id, ]
    ir <- ir[1, ]
    showModal(overwriterUI(
      id = "overwriter",
      conf = list(
        eu_id = ir$eu_id,
        eu_name = ir$eu_name,
        overwrite_risk = ir$overwrite_risk,
        overwrite_risk_comm = ir$overwrite_risk_comm,
        table = ir
      )
    ))
  })
  overwriterServer(
    id = "overwriter",
    clicky = reactive(input$map_shape_click),
    overwriter_data = overwriter_data,
    intro_risk = intro_risk
  )

  # intro_risk -----
  intro_risk <- summariseScoresServer(
    id = "summarise_risk_table",
    epi_units = epi_units,
    misc_risk_table = misc_risk_table,
    core_risks = core_risks,
    overwriter_data = overwriter_data
  )
  observe({
    req(baseLeaflet())
    req(intro_risk())
    plot_risk_interactive(
      dataset = intro_risk(),
      ll = leafletProxy("map")
    )
  })

  output$table <- renderReactable({
    req(intro_risk())
    reactable::reactable(
      intro_risk(),
      searchable = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      defaultPageSize = 100,
      striped = TRUE
    )
  })

  # export -----
  observeEvent(input$open_export, {
    showModal(exportUI("export_module"))
    })
  exportServer(
    id = "export_module",
    epi_units = epi_units,
    files = reactive({
      misc_list <- list("Combined table" = misc_risk_table())
      misc_meta <- misc_risk_meta()
      if (length(misc_meta) > 0) {
        for (name in names(misc_meta)) {
          misc_list[[name]] <- misc_meta[[name]]$dataset
        }
      }
      out_list <- list()
      if (isTruthy(epi_units())) {
        out_list[["Epidemiological units"]] <- list(
          "Input epidemiological units" = epi_units()
        )
      }
      if (isTruthy(emission_risk_factors()) && isTruthy(emission_scores())) {
        out_list[["Emission risk"]] <- list(
          "Emission risk factors" = emission_risk_factors(),
          "Emission risk scores" = emission_scores(),
          "Plot" = plot_emission_risk(emission_scores())
        )
      }
      if (isTruthy(intro_risk())) {
        out_list[["Final analysis"]] <- list(
          "Risk table as geosptial" = intro_risk(),
          "Risk table as tabular" = sf::st_drop_geometry(intro_risk()),
          "Plot" = plot_risk(intro_risk())
        )
      }
      if (isTruthy(entry_points_input()) && isTruthy(entry_points()$dataset)) {
        out_list[["Entry Points Data"]] <- list(
          "Input data" = entry_points_input(),
          "Introduction risk by epidemiological unit" = entry_points()$dataset,
          "Emission risk by enrty point" = extract_point_risk(entry_points()$dataset),
          "Plot" = plot_risk(entry_points()$dataset)
        )
      }
      if (isTruthy(animal_mobility_input()) && isTruthy(animal_mobility()$dataset)) {
        out_list[["Animal Mobility Data"]] <- list(
          "Input data" = animal_mobility_input(),
          "Introduction risk by epidemiological unit" = animal_mobility()$dataset,
          "Emission risk by animal mobility flow" = extract_flow_risk(animal_mobility()$dataset),
          "Plot" = plot_risk(animal_mobility()$dataset)
        )
      }
      if (isTruthy(border_input()$result) && isTruthy(border_risk()$dataset)) {
        out_list[["Border Risk Data"]] <- list(
          "Shared borders (input data)" = border_input()$result,
          "Introduction risk by epidemiological unit" = border_risk()$dataset,
          "Emission risk by shared border" = extract_border(border_risk()$dataset),
          "Plot" = plot_risk(border_risk()$dataset)
        )
      }
      if (isTruthy(input_raster()) && isTruthy(road_access()$dataset)) {
        out_list[["Road access risk"]] = list(
          "Input raster" = input_raster(),
          "Introduction risk by epidemiological unit" = road_access()$dataset,
          "Plot" = plot_risk(road_access()$dataset)
        )
      }
      out_list[["Additional risks"]] <- misc_list
      out_list
    })
  )

  helpServer(selected_tab = reactive(input$navbar))

}
