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
    nav_menu(
      title = "Workspace",
      align = "right",
      icon = icon("briefcase"),
      nav_item(actionLink(
        inputId = "save",
        label = "Save",
        icon("download"),
      )),
      nav_item(actionLink(
        inputId = "load",
        label = "Load",
        icon("upload")
      ))
    ),
    nav_panel(
      title = "About",
      value = "about",
      icon = icon("circle-info")
    )
  )
)

server <- function(input, output) {

  # Datasets -----
  # Input
  tab_epi_units <- importEpiUnitsServer("import_epi_units")
  tab_emission_risk_factors <- importEmissionRiskFactorsServer("import_erf")

  tab_emission_risk <- reactiveVal(NULL)
  observeEvent(tab_emission_risk_factors(), {
    erf <- req(tab_emission_risk_factors())
    er <- calc_emission_risk(
      emission_risk_factors = erf,
      weights = riskintrodata::emission_risk_weights,
      keep_scores = TRUE
    )
    tab_emission_risk(er)
  })

  tab_shared_borders <- reactiveVal(NULL)

  # Analysis output
  tab_ri_entry_points <- reactiveVal(NULL)
  tab_ri_road_access <- reactiveVal(NULL)
  tab_ri_animal_mobility <- reactiveVal(NULL)
  tab_ri_border <- reactiveVal(NULL)
  tab_ri_misc <- reactiveVal(list())

  # Summary of analyses -----
  tab_ri_summary <- reactiveVal(NULL)
  observeEvent(tab_epi_units(),{
    epi_units <- req(tab_epi_units())
    risk_tab <- riskintroanalysis::risk_table(
      epi_units = epi_units,
      scale = c(0, 100)
      )
    tab_ri_summary(risk_tab)
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
  output$map_ri_summary <- renderLeaflet(basemap())
  output$map_emission_risk <- renderLeaflet(basemap())

  # Update maps ----
  ## Risk summary map -----
  observeEvent(tab_ri_summary(), {
    req(tab_ri_summary())
    plot_risk_interactive(
      dataset = tab_ri_summary(),
      ll = leafletProxy("map_ri_summary")
    )
  })
  ## Emission risk map ----
  observeEvent(tab_emission_risk(), {
    er <- req(tab_emission_risk())
    browser()
    plot_emission_risk_interactive(
      emission_risk = tab_emission_risk(),
      leafletProxy(mapId = "map_emission_risk")
    )
  })


}

# Run the application
shinyApp(ui = ui, server = server, onStart = onstart)

