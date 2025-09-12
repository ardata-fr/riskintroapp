.sidebar_width <- 350
#' Riskintro Shiny UI
#'
#' Serves as the UI for riskintro shiny app.
#'
#' @import shiny
#' @importFrom bslib
#' bs_theme nav_panel nav_panel_hidden nav_item nav_menu
#' nav_spacer page_navbar navbar_options layout_sidebar
#' page_fillable sidebar
#' @importFrom leaflet
#'  leafletOutput
#' @export
ui <- function() {
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
    # Epidemiological units ----
    nav_panel(
      title = "Introduction risk",
      value = "study_settings",
      icon = icon("map"),
      layout_sidebar(
        sidebar = sidebar(
          width = .sidebar_width,
          title = "Introduction risk",
          actionButton(
            inputId = "import_epi_units",
            label = "Import epidemiological units",
            icon = icon("upload")
          ),
          tags$hr(),
          summariseScoresUI("summarise_risk_table"),

          exportUI()
        ),
        leafletOutput(
          outputId = "map",
          width = "100%",
          height = "85vh"
        )
      )
    ),
    # Emission risk ----
    nav_panel(
      title = "Emission scores",
      value = "emission_risk",
      icon = icon("arrows-up-down-left-right"),
      emissionScoresUI("emission_scores")
    ),
    # Intro risk analysis tabs ----
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
    nav_panel_hidden(
      value = "nav_animal_movement_risk",
      animalMobilityUI("animal_mobility")
    ),
    nav_panel_hidden(
      value = "nav_road_access_risk",
      roadAccessRiskUI("road_access")
    ),
    nav_panel_hidden(
      value = "nav_misc_risk",
      miscRiskUI("misc")
    ),
    nav_panel_hidden(
      value = "nav_border_risk",
      borderRiskUI("border")
    ),
    nav_panel_hidden(
      value = "nav_entry_point_risk",
      entryPointsUI("entry_points")
    ),
    nav_spacer(),

    # Help ----
    helpUI(),

    # Workspace -----
    workspaceUI("workspace"),

    # About ----
    nav_panel(
      title = "About",
      value = "about",
      icon = icon("circle-info"),
      aboutUI()
    )
  )

}
