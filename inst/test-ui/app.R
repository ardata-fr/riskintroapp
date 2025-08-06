library(riskintroapp)
library(riskintrodata)
library(riskintroanalysis)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(datamods)
library(esquisse)
library(leaflet)

ui <- fluidPage(
  page_navbar(
    id = "navbar",
    navbar_options = navbar_options(
      bg = "#06292b"
    ),
    header = app_header(),
    theme = bs_theme_cirad(),
    title = "RiskIntroApp",
    nav_item(
      div(icon("warning"))
    ),
    nav_panel(
      title = "Epidemiological units",
      value = "study_settings",
      icon = icon("map"),
      page_fillable(
        layout_sidebar(
          sidebar = sidebar(
            title = "Epidemiological units",
            importEpiUnitsUI("import_epi_units")
          ),
            leafletOutput(outputId = "map_ri_summary", width = "100%", height = "85vh")
        )
      )
    ),
    nav_panel(
      title = "Emission risk",
      value = "emission_risk",
      icon = icon("arrows-up-down-left-right"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Emission risks",
          importEmissionRiskFactorsUI("import_erf")
        ),
        leafletOutput(outputId = "map_emission_risk", width = "100%", height = "85vh")
      )
    ),
    nav_menu(
      title = "Introduction risks",
      value = "risk_panel_selector",
      icon = icon("warning"),

      nav_item(actionLink(
        inputId = "nav_border_risk",
        label = "Border risk",
        icon("arrow-down-up-across-line")
      )),
      nav_item(actionLink(
        inputId = "nav_animal_movement_risk",
        label = "Animal movement risk",
        icon("truck-plane")
      )),
      nav_item(actionLink(
        inputId = "nav_entry_point_risk",
        label = "Entry point risk",
        icon("location-dot")
      )),
      nav_item(actionLink(
        inputId = "nav_road_access_risk",
        label = "Road access risk",
        icon("arrow-right-to-city")
      )),
      nav_item(actionLink(
        inputId = "nav_misc_risk",
        label = "Miscellaneous risks",
        icon("arrows-to-circle")
      ))
    ),
    nav_panel(
      title = "Summary",
      value = "summary_tab",
      icon = icon("arrows-to-circle"),
      div("Hello Summary")
    ),
    nav_panel_hidden(
      value = "nav_animal_movement_risk",
      div("nav_animal_movement_risk")
    ),
    nav_panel_hidden(
      value = "nav_road_access_risk",
      div("nav_road_access_risk")
    ),
    nav_panel_hidden(
      value = "nav_misc_risk",
      div("nav_misc_risk")
    ),
    nav_panel_hidden(
      value = "nav_border_risk",
      div("nav_border_risk")
    ),
    nav_panel_hidden(
      value = "nav_entry_point_risk",
      div("nav_entry_point_risk")
    ),
    nav_spacer(),
    workspaceUI("workspace"),
    nav_panel(
      title = "About",
      value = "about",
      icon = icon("circle-info")
    )
  )
)

server <- function(input, output) {

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
    browser()
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
    browser()
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
  observe({
    req(baseLeafletRT())
    rt <- req(datasets$risk_table)
    browser()
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

# Run the application
shinyApp(ui = ui, server = server, onStart = onstart)

