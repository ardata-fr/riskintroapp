
library(riskintroapp)
library(shiny)
library(shinyjs)
library(bslib)

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
      div("Epi units")
    ),
    nav_panel(
      title = "Emission risk",
      value = "emission_risk",
      icon = icon("arrows-up-down-left-right"),
      div("emission risk")
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
  tab_epi_units <- reactiveVal(NULL)
  tab_emission_risk_factors <- reactiveVal(NULL)
  tab_emission_risk <- reactiveVal(NULL)
  tab_shared_borders <- reactiveVal(NULL)

  # Analysis output
  tab_ri_entry_points <- reactiveVal(NULL)
  tab_ri_road_access <- reactiveVal(NULL)
  tab_ri_animal_mobility <- reactiveVal(NULL)
  tab_ri_border <- reactiveVal(NULL)
  tab_ri_misc <- reactiveVal(list())

  # Summary of analyses
  tab_ri_summary <- reactiveVal(NULL)

  # Settings ----
  # Analysis settings
  settings <- reactiveVal(list())

  # session$userData$tab_epi_units <- reactiveVal(NULL)
  # observeEvent(tab_epi_units(), {
  #   req(tab_epi_units())
  #   session$userData$tab_epi_units(tab_epi_units())
  # })

  # nav_panel navigation ----
  observeEvent(input$nav_border_risk, {nav_select("navbar", selected = "nav_border_risk")})
  observeEvent(input$nav_animal_movement_risk, {nav_select("navbar", selected = "nav_animal_movement_risk")})
  observeEvent(input$nav_road_access_risk, {nav_select("navbar", selected = "nav_road_access_risk")})
  observeEvent(input$nav_misc_risk, {nav_select("navbar", selected = "nav_misc_risk")})
  observeEvent(input$nav_entry_point_risk, {nav_select("navbar", selected = "nav_entry_point_risk")})
}

# Run the application
shinyApp(ui = ui, server = server, onStart = onstart)
