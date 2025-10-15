#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shinyWidgets dropMenu
miscRiskUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = .sidebar_width,
      title = titleWithHelpButton(
        key = "misc-risk-title",
        ns = ns,
        help_url = "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/additional-risks.html"
      ),
      uiOutput(ns("config_is_valid")),
      selectInput(
        inputId = ns("select_risk"),
        label = "Select current risk",
        choices = character(0L)
      ),
      dropMenu(
        arrow = FALSE,
        tag = actionButton(
          inputId = ns("dropMenu"),
          label = "Import risk",
          width = '100%',
          icon = icon('file-import')
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
      actionButton(
        inputId = ns("open_risk_scaling"),
        label = "Edit risk scaling",
        icon = icon("pen-to-square")
      ),
      actionButton(
        inputId = ns("delete_risk"),
        label = "Delete risk",
        icon = icon("trash")
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
#' @importFrom dplyr across where
miscRiskServer <- function(id, epi_units, updated_workspace) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Help button ----
      observeEvent(input$open_help, {
        url <- "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/additional-risks.html"
        shinyjs::runjs(paste0("window.open('", url, "', 'help', 'width=1200,height=800,scrollbars=yes,resizable=yes');"))
      })

      # init map ----
      baseMap <- reactive({basemap()})
      output$map <- renderLeaflet({
        req(baseMap())
        baseMap()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)
      observeEvent(epi_units(), {
        req(epi_units())
        setBoundsFromSF(leafletProxy("map"), epi_units())
      })

      # miscRiskMetaData ----
      miscRiskMetaData <- reactiveVal(list())

      observeEvent(updated_workspace(), {
        ws <- updated_workspace()
        miscRiskMetaData(ws$misc_settings)
        updateSelectInput(
          inputId = "select_risk",
          choices = names(ws$misc_settings),
          selected = names(ws$misc_settings)[[1]]
        )
      })

      # selectedDataset ----
      selectedDataset <- reactive({
        req(input$select_risk)
        miscRiskMetaData()[[input$select_risk]]$dataset
      })

      # delete risk button ----
      observeEvent(input$delete_risk, {
        req(input$select_risk)
        metadata <- miscRiskMetaData()
        metadata[[input$select_risk]] <- NULL
        miscRiskMetaData(metadata)
        updateSelectInput(
          inputId = "select_risk",
          choices = names(metadata),
          selected = NULL
        )
        if(length(metadata) == 0) {
          ll <- leafletProxy(mapId = "map")
          leaflet::clearShapes(ll)
        }
      })

      # update map ----
      observe({
        mrt <- req(miscRiskTable())
        ll <- leafletProxy(mapId = "map")
        leaflet::clearShapes(ll)
        req(input$select_risk)
        pal <- riskintroanalysis::scorePalette(scale = c(0, 100))
        leaflet::clearShapes(ll)
        leaflet::addPolygons(
          ll,
          data = mrt,
          label = generate_leaflet_labels(mrt),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.8,
          fillColor = pal(mrt[[input$select_risk]])
        ) |>
          addScoreLegend(title = input$select_risk)
      })

      # # table ----
      output$table <- renderReactable({
        mrt <- req(miscRiskTable())
        mrt <- sf::st_drop_geometry(mrt)
        mrt <- dplyr::mutate(mrt, across(where(is.numeric), \(x) round(x, 3)))
        reactable::reactable(
          mrt,
          searchable = TRUE,
          filterable = TRUE,
          showPageSizeOptions = TRUE,
          defaultPageSize = 100,
          striped = TRUE)
      })

      # configIsValid ----
      configIsValid <- reactive(label = paste0("configIsValid-import-", id), {
        if (length(miscRiskMetaData()) > 0) {
          status <- config_is_valid_misc_risks(miscRiskMetaData())
        } else {
          status <- build_config_status(
            value = FALSE,
            msg = "No additional risks have been imported yet."
          )
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
        logger::log_info("Miscellaneous risk precalculated data imported in miscRiskServer: {new_precalc()$name}")
        metadata <- miscRiskMetaData()
        metadata[[new_precalc()$name]] <- new_precalc()
        miscRiskMetaData(metadata)
        updateSelectInput(
          inputId = "select_risk",
          choices = names(metadata),
          selected = new_precalc()$name
        )
      })


      # Import raster ----
      observeEvent(input$open_raster, {
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(importMiscRiskRasterUI(ns("raster_import")))
      })
      new_raster <- importMiscRiskRasterServer(
        id = "raster_import",
        riskMetaData = miscRiskMetaData,
        epi_units = epi_units
      )
      observeEvent(new_raster(), ignoreNULL = TRUE, {
        logger::log_info("Miscellaneous risk raster imported in miscRiskServer: {new_raster()$name}")
        metadata <- miscRiskMetaData()
        metadata[[new_raster()$name]] <- new_raster()
        miscRiskMetaData(metadata)
        updateSelectInput(
          inputId = "select_risk",
          choices = names(metadata),
          selected = new_raster()$name
        )
      })

      # edit risk scaling -----
      observeEvent(input$open_risk_scaling, {
        req(input$select_risk)
        rescaling_args <- miscRiskMetaData()[[input$select_risk]]$rescale_args
        showModal(rescaleRiskUI(id = ns("rescale_modal"),rescaling_args = rescaling_args))
      })
      new_rescaling_args <- rescaleRiskServer(
        id = "rescale_modal",
        dataset = selectedDataset
      )
      # Update rescaling_args for this dataset
      observeEvent(new_rescaling_args(), {
        new_args <- new_rescaling_args()
        metadata <- miscRiskMetaData()
        metadata[[input$select_risk]]$rescale_args <- new_args
        miscRiskMetaData(metadata)
      })

      observeEvent(input$dropMenu, ignoreNULL = TRUE,{
        if(!isTruthy(epi_units())) {
          hideDropMenu(id = "dropMenu_dropmenu")
          show_alert(
            title = "Epidemiological units not initialised",
            text = "Epidemiological units need to be added to your workspace before these risks can be imported. You can do this in the sidebar of the Epidemiological units tab."
          )
        }
      })

      # miscRiskTable ----
      miscRiskTable <- reactive({
        req(epi_units())
        req(configIsValid())
        if (length(miscRiskMetaData()) > 0) {
          build_misc_risk_table(
            epi_units = epi_units(),
            risk_list =  miscRiskMetaData()
          )
        } else {
          NULL
        }
      })

      miscRiskTableReturn <- reactive({
        mrt <- req(miscRiskTable())
        # Remove columns that will not be needed downstream
        mrt <- sf::st_drop_geometry(mrt)
        mrt$eu_name <- NULL
        mrt
      })

      returnData <- reactiveVal(NULL)
      observe({
        out <- list(
          # return meta data even if config is not valid
          # so that imported data can be saved to workspace
          misc_risk_table = if(configIsValid()) miscRiskTableReturn() else NULL,
          misk_risk_meta = miscRiskMetaData()
        )
        returnData(out)
      })
      return(returnData)
    })
}


config_is_valid_misc_risks <- function(x) {

  # Check validity of all misc risks
  all_statuses <- lapply(x, function(x) {

    if(!isTruthy(x$dataset)) {
      status <- build_config_status(
        value = FALSE,
        msg = "A dataset not found."
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

    if(!isTruthy(x$rescale_args)) {
      status <- build_config_status(
        value = FALSE,
        msg = .configIsValidMsgs$rescale_missing
      )
      return(status)
    }

    if(length(x$rescale_args) == 0) {
      status <- build_config_status(
        value = FALSE,
        msg = "Rescaling parameters have not been set."
      )
      return(status)
    }

    for (i in seq_along(x$rescale_args)) {
      this_arg <- x$rescale_args[i]
      if(is.null(this_arg[[1]]) || length(this_arg[[1]]) == 0) {
        status <- build_config_status(
          value = FALSE,
          msg = sprintf("Rescaling parmeter \"%s\" is missing.", names(this_arg))
        )
        return(status)
      }
    }

    build_config_status(
      value = TRUE,
      msg = "Selected risk is properly configured."
    )
  })



  if(any(!unlist(all_statuses))) {
    false_statuses <- Filter(f = isFALSE, x = all_statuses)
    messages <- sapply(false_statuses, function(x) attr(x, "comment"))
    status <- build_config_status(
      value = FALSE,
      msg = paste(
        tags$strong("Issues found"), tags$br(),
        paste0('"', names(messages), '": ' , messages, tags$br(), collapse = "")
      ) |>
        HTML()
    )
  } else {
    status <- build_config_status(
      value = TRUE,
      msg = "All risks are properly configured."
    )
  }

  status
}

#' @importFrom riskintroanalysis rescale_risk_scores
build_misc_risk_table <- function(epi_units, risk_list){

  rt <- riskintroanalysis::risk_table(epi_units, scale = c(0, 100))
  for (i in risk_list) {

    rescaled_risk_data <- rescale_risk_scores(
      dataset = i$dataset,
      cols = i$rescale_args$cols,
      from = i$rescale_args$from,
      to = c(0, 100),
      method = i$rescale_args$method,
      inverse = i$rescale_args$inverse,
      reverse = if (!is.null(i$rescale_args$reverse)) i$rescale_args$reverse else FALSE
    )

    rt <- add_risk(
      risk_table = rt,
      risk_data = rescaled_risk_data,
      join_by = i$join_by
    )
  }
  rt
}
