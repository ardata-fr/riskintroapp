

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
    nav_panel(
      title = "Epidemiological units",
      value = "study_settings",
      icon = icon("map"),
      page_fillable(
        layout_sidebar(
          sidebar = sidebar(
            title = "Epidemiological units",
            importEpiUnitsUI("import_epi_units"),
            summariseRiskScoresUI("summarise_risk_table")
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

}
