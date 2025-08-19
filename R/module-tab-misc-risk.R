#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shinyWidgets dropMenu
miscRiskUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Miscellaneous risks",
      uiOutput(ns("config_is_valid")),
      dropMenu(
        arrow = FALSE,
        tag = actionButton(
          inputId = ns("dropMenu"),
          label = "Import risk",
          width = '100%'
        ),
        placement = "right",
        tagList(
          div(
            actionButton(
              inputId = ns("open_raster"),
              label = "From raster file",
              style = "width: 200px; margin-bottom: 7px;"
            )
          ),
          div(
            actionButton(
              inputId = ns("open_precalc"),
              label = "Precalculated risk",
              style = "width: 200px; margin-bottom: 7px;"
            )
          )
        )
      ),
      selectInput(
        inputId = ns("select_risk"),
        label = "Select current risk",
        choices = character(0L)
      ),
      actionButton(
        inputId = ns("open_risk_scaling"),
        label = "Edit risk scaling",
        icon = icon("pen-to-square")
      ),
      actionButton(
        inputId = ns("delete_risk"),
        label = "Delete risk",
        icon = icon("trash"),
        class = "btn-warning"
      ),
    ),
    navset_card_tab(
      id = ns("panel_ui"),
      nav_panel(
        title = "Map view",
        leafletOutput(ns("map"), width = "100%", height = "85vh")
      ),
      nav_panel(
        title = "Table view",
        reactableOutput(outputId = ns("table"))
      )
    )
  )
}

#' @importFrom sf st_drop_geometry
#' @importFrom leaflet
#'  renderLeaflet leafletProxy addPolygons
#' @importFrom reactable
#'  reactable renderReactable
miscRiskServer <- function(id, epi_units) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # miscRiskMetaData ----
      miscRiskMetaData <- reactiveVal(list())

      # Initialise maps ----
      baseMap <- reactive({basemap()})
      output$map <- renderLeaflet({
        req(baseMap())
        baseMap()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)

      # select_risk ----
      observe({
        updateSelectInput(
          inputId = "select_risk",
          choices = names(miscRiskMetaData())
        )
      })
      # selectedRisk ----
      selectedRisk <- reactive({
        req(length(miscRiskMetaData()) > 0)
        miscRiskMetaData()[[input$select_risk]]
      })

      # delete risk ----
      observeEvent(input$delete_risk, {
        req(length(miscRiskMetaData()) > 0)
        req(selectedRisk())
        metadata <- miscRiskMetaData()
        metadata[[selectedRisk()$name]] <- NULL
        miscRiskMetaData(metadata)
      })

      # miscRiskTable ----
      miscRiskTable <- reactive({
        req(epi_units())
        req(configIsValid())
        browser()
        if (length(miscRiskMetaData()) > 0) {
          build_misc_risk_table(epi_units = epi_units(), risk_list =  miscRiskMetaData())
        } else {
          NULL
        }
      })

      # Plot misk risk table ----
      observeEvent(miscRiskTable(), {
        mrt <- req(miscRiskTable())
        browser()
        ll <- leafletProxy(mapId = "map")
        labels <- generate_leaflet_labels(mrt)
        leaflet::addPolygons(
          ll, data = mrt, label = labels
        )
      })

      # # table ----
      output$table <- renderReactable({
        mrt <- req(miscRiskTable())
        browser()
        mrt <- sf::st_drop_geometry(mrt)
        mrt$eu_id <- NULL
        reactable::reactable(mrt)
      })

      # config_is_valid ----
      configIsValid <- reactive({
        if (length(miscRiskMetaData()) > 0) {
          sr <- req(selectedRisk())
          status <- validate_selected_risk(sr)
        } else {
          status <- build_config_status(
            value = FALSE,
            msg = "No miscellaneous risks have been imported yet."
          )
          report_config_status(status)
        }
        status
      })
      output$config_is_valid <- renderUI({
        report_config_status(configIsValid())
      })

      # Import precalc ----
      observeEvent(input$open_precalc, {
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(importMiscRiskPrecalculatedUI(ns("precalc_import")))
      })
      new_precalc <- importMiscRiskPrecalculatedServer(
        id = "precalc_import",
        riskMetaData = miscRiskMetaData,
        epi_units = epi_units
      )
      observeEvent(new_precalc(), ignoreNULL = TRUE, {
        metadata <- miscRiskMetaData()
        metadata[[new_precalc()$name]] <- new_precalc()
        miscRiskMetaData(metadata)
      })


      # Import raster ----
      observeEvent(input$open_raster, {
        showModal(modalDialog(
          fluidRow(column(
            width = 10, offset = 1,

            div("Not yet implemented")

          )),
          footer = list(
            actionButton(
              inputId = ns("apply_raster"),
              class = "btn-primary",
              label = "Import",
              disabled = TRUE),
            actionButton(
              inputId = ns("cancel_raster"),
              label = "Cancel",
              class = "btn-default"
            )
          ),
          size = "xl", easyClose = TRUE
        ))
      })




      # edit risk scaling -----
      observeEvent(input$open_risk_scaling, {
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(modalDialog(
          fluidRow(column(
            width = 10, offset = 1,

            div("Not yet implemented")

          )),
          footer = list(
            actionButton(
              inputId = ns("apply_rescale"),
              class = "btn-primary",
              label = "Import",
              disabled = TRUE),
            actionButton(
              inputId = ns("cancel_rescale"),
              label = "Cancel",
              class = "btn-default"
            )
          ), size = "xl", easyClose = TRUE
        ))
      })


      # observeEvent(input$dropMenu, ignoreNULL = TRUE,{
      #   if(!isTruthy(miscRiskTable())) {
      #     hideDropMenu(id = "dropMenu_dropmenu")
      #     show_alert(
      #       title = "Epidemiological units not initialised",
      #       text = "Epidemiological units need to be added to your workspace before these risks can be imported. You can do this in the sidebar of the Epidemiological units tab."
      #     )
      #   }
      # })



    }
  )
}


validate_selected_risk <- function(x) {

  if(!isTruthy(x$dataset)) {
    status <- build_config_status(
      value = FALSE,
      msg = "A dataset not found."
    )
    return(status)
  }
  if(!isTruthy(x$risk_cols)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Risk column not found."
    )
    return(status)
  }
  if(!isTruthy(x$join_by)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Join/identifier column not found."
    )
    return(status)
  }
  if(length(x$rescaling_param) == 0) {
    status <- build_config_status(
      value = FALSE,
      msg = "Rescaling settings not configured."
    )
    return(status)
  }

  build_config_status(
    value = TRUE,
    msg = "Selected risk is properly configured."
  )
}

build_misc_risk_table <- function(epi_units, risk_list){
  rt <- riskintroanalysis::risk_table(epi_units, scale = c(0, 100))
  for (i in risk_list) {
    rt <- add_risk(
      risk_table = rt,
      risk_data = i$dataset,
      cols = i$risk_cols,
      scale = c(0, 100),
      join_by = i$join_by
    )
  }
  rt
}
