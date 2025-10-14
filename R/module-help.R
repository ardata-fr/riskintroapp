helpUI <- function(id = "help") {
  ns <- NS(id)
  nav_item(
    actionLink(inputId = ns("open"), "Help", icon = icon("circle-question"))
  )
}

#' @importFrom shinyjs runjs
helpServer <- function(id = "help", selected_tab) {
  moduleServer(
    id,
    function(input, output, session) {

      # help system ----
      observeEvent(input$open, {
        base_url <- "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis"
        # help_urls <- list(
        #   "study_settings" = paste0(base_url, "/articles/introduction-risk.html"),
        #   "emission_risk" = paste0(base_url, "/articles/emission-scores.html"),
        #   "nav_border_risk" = paste0(base_url, "/articles/border-risk-analysis.html"),
        #   "nav_animal_movement_risk" = paste0(base_url, "/articles/animal-mobility-analysis.html"),
        #   "nav_entry_point_risk" = paste0(base_url, "/articles/entry-points-analysis.html"),
        #   "nav_road_access_risk" = paste0(base_url, "/articles/road-access-analysis.html"),
        #   "nav_misc_risk" = paste0(base_url, "/articles/additional-risks.html"),
        #   "about" = base_url
        # )

        # url <- help_urls[[selected_tab()]]
        # if (is.null(url)) {
        #   url <- base_url
        # }

        # Open in new tab
        # shinyjs::runjs(paste0("window.open('", url, "', '_blank');"))

        # Open documentation in popup window
        shinyjs::runjs(paste0("window.open('", base_url, "', 'help', 'width=1200,height=800,scrollbars=yes,resizable=yes');"))
      })
    }
  )
}
