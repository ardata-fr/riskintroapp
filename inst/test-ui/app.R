
library(shiny)
library(bslib)

ui <- fluidPage(
  page_navbar(
    id = "navbar",
    navbar_options = navbar_options(
      bg = "#06292b",
      inverse = TRUE
    ),
    # theme = bs_theme_cirad(),
    title = "RiskIntroApp",
    nav_item(
      div(icon("warning"))
    ),
    nav_panel(
      title = "Epidemiological units",
      value = "study_settings",
      icon = icon("map"),
      # studySettingsUI("study_settings_tab")
    ),
    nav_panel(
      title = "Emission risk",
      value = "emission_risk",
      icon = icon("arrows-up-down-left-right"),
      # emissionRiskUI("emission_risk_tab")
    ),
    nav_menu(
      title = "Introduction risks",
      value = "risk_panel_selector",
      icon = icon("warning"),

      nav_item(actionLink(
        inputId = "border_risk",
        label = "Border risk",
        icon("arrow-down-up-across-line")
      )),
      nav_item(actionLink(
        inputId = "animal_movement_risk",
        label = "Animal movement risk",
        icon("truck-plane")
        )),
      nav_item(actionLink(
        inputId = "city_access_risk",
        label = "Entry point risk",
        icon("location-dot")
        )),
      nav_item(actionLink(
        inputId = "road_access_risk",
        label = "Road access risk",
        icon("arrow-right-to-city")
      )),
      nav_item(actionLink(
        inputId = "misc_risk",
        label = "Miscellaneous risks",
        icon("arrows-to-circle")
      ))
    ),
    # nav_panel_hidden(
    #   # title = "Miscellaneous risks",
    #   value = "border_risk",
    #   icon = icon("arrow-down-up-across-line"),
    #   # borderRiskUI("border_risk_tab")
    # ),
    # nav_panel_hidden(
    #   # title = "Animal movement risk",
    #   value = "animal_movement_risk",
    #   icon = icon("truck-plane"),
    #   # animalMovementUI("animal_movement_tab")
    # ),
    # nav_panel_hidden(
    #   # title = "Road access risk",
    #   value = "road_access_risk",
    #   icon = icon("arrow-right-to-city"),
    #   # roadAccessUI("road_access_risk_tab")
    # ),
    # nav_panel_hidden(
    #   # title = "Miscellaneous risks",
    #   value = "misc_risk",
    #   icon = icon("arrows-to-circle"),
    #   # riskSummaryUI("risk_summary_tab")
    # ),
    nav_panel(
      title = "Summary",
      value = "summary_tab",
      icon = icon("arrows-to-circle"),
      # riskSummaryUI("risk_summary_tab")
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
}

# Run the application
shinyApp(ui = ui, server = server)
