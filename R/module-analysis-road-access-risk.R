#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton icon uiOutput
roadAccessRiskUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = .sidebar_width,
      title = titleWithHelpButton(
        key = "road-access-title",
        ns = ns,
        help_url = "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/road-access-analysis.html"
      ),
      uiOutput(ns("config_is_valid")),
      tags$br(),
      dropMenu(
        placement = "right",
        tag = actionButton(
          inputId = ns("dropMenu"),
          label = "Import raster",
          icon = icon('file-import'),
          type = "default"
        ),
        div(actionButton(
          inputId = ns("dnld_btn"),
          label = "Direct download",
          icon = icon('download'),
          width = "198px"
        )),
        div(actionButton(
          inputId = ns("import_btn"),
          label = "Import file",
          width = "198px",
          icon = icon('file-import')
        ))
      ),

      tags$br(),

      actionButton(
        inputId = ns("open_risk_scaling"),
        label = "Edit risk scaling",
        icon = icon("pen-to-square")
      )
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

roadAccessRiskServer <- function(id, input_raster, epi_units, saved_config) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Help button ----
      observeEvent(input$open_help, {
        url <- "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/road-access-analysis.html"
        shinyjs::runjs(paste0("window.open('", url, "', 'help', 'width=1200,height=800,scrollbars=yes,resizable=yes');"))
      })

      # saved_config -----
      observeEvent(saved_config(), {
        rescaling_args(saved_config()$rescaling_args)
      })

      # init map ----
      baseLeaflet <- reactive({basemap()})
      output$map <- renderLeaflet({
        req(baseLeaflet())
        baseLeaflet()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)
      observeEvent(epi_units(), {
        req(epi_units())
        setBoundsFromSF(leafletProxy("map"), epi_units())
      })

      rescaling_args <- reactiveVal(list(
        method = "linear",
        inverse = FALSE
        ))

      # import ----
      observeEvent(input$import_btn, {
        req(epi_units()) # required for cropping output
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(importRoadAccessUI(ns("import")))
      })
      new_raster <- importRoadAccessServer("import")
      observeEvent(new_raster(), {
        logger::log_info("Road access raster imported in roadAccessRiskServer")
        downloadError(NULL)
        cropped <- terra::crop(new_raster(), epi_units(), mask = TRUE)
        input_raster(cropped)
      })

      # download ----
      downloadError <- reactiveVal(NULL)

      observeEvent(input$dnld_btn, {
        req(epi_units())
        hideDropMenu(id = "dropMenu_dropmenu")

        logger::log_trace("Initializing riskintrodata cache")
        cache_init <- safely(riskintrodata::init_riskintrodata_cache)()
        if(is_error(cache_init$error)) {
          downloadError(cache_init$error)
          return()
        }

        logger::log_trace("Downloading road access raster")
        download_result <- safely(riskintrodata::download_road_access_raster)()
        if(is_error(download_result$error)) {
          downloadError(download_result$error)
          return()
        }

        logger::log_trace("Loading road access raster")
        safely_rast <- safely(terra::rast)
        raster_result <- safely_rast(download_result$result)
        if(is_error(raster_result$error)) {
          downloadError(raster_result$error)
          return()
        }

        logger::log_trace("Cropping raster to epi_units")
        cropped <- terra::crop(raster_result$result, epi_units(), mask = TRUE)
        input_raster(cropped)

        show_toast(
          "Road accessibility raster has finished downloading.",
          type = "info",
          timer = 5000
        )
        downloadError(NULL)
      })


      # riskScores ----
      riskScores <- reactive({
        if (!isTruthy(epi_units()) ||
            !isTruthy(input_raster())
        ) {
          return(NULL)
        }
        safely_calc <- safely(calc_road_access_risk)
        res <- safely_calc(
          epi_units = epi_units(),
          road_access_raster = input_raster(),
          aggregate_fun = "mean"
        )
        res
      })

      # Risk scaling ----
      observeEvent(input$open_risk_scaling, {
        req(riskScores())
        showModal(rescaleRiskUI(id = ns("rescale_modal"), rescaling_args = rescaling_args()))
      })
      new_rescaling_args <- rescaleRiskServer(
        id = "rescale_modal",
        dataset = reactive(riskScores()$result),
      )
      observeEvent(new_rescaling_args(), {
        rescaling_args(new_rescaling_args())
      })
      rescaledScores <- reactive({
        if (!isTruthy(riskScores()$result)) {
          return(NULL)
        }
        args <- req(rescaling_args())
        rescale_risk_scores(
          dataset = riskScores()$result,
          method = args$method,
          inverse = args$inverse
        )
      })

      # configIsValid ----
      configIsValid <- reactive(label = paste0("configIsValid-", id), {

        warnings <- character()
        if (!isTruthy(epi_units())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Epidemiological units dataset must be imported."
          )
          return(status)
        }
        if (is_error(downloadError())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while downloading",
            error = downloadError()
          )
          return(status)
        }
        if (!isTruthy(input_raster())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Raster import is missing."
          )
          return(status)
        }
        if (!isTruthy(riskScores())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Risk scores cannot be calculated."
          )
          return(status)
        }
        if (is_error(riskScores()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while calculating risks scores:",
            error = riskScores()$error
          )
          return(status)
        }

        if (isTruthy(riskScores()$warnings)) {
          warnings <- c(warnings, riskScores()$warnings)
        }

        if (!isTruthy(rescaledScores())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Risk scores cannot be rescaled."
          )
          return(status)
        }
        build_config_status(
          value = TRUE,
          msg = "Analysis complete.",
          warnings = warnings,
          warnings_msg = "Analysis complete with warnings:"
        )
      })
      output$config_is_valid <- renderUI({
        report_config_status(configIsValid())
      })
      outputOptions(output, "config_is_valid", suspendWhenHidden = FALSE)

      # table ----
      output$table <- renderReactable({
        req(configIsValid())
          reactable::reactable(
            rescaledScores(),
            searchable = TRUE,
            filterable = TRUE,
            showPageSizeOptions = TRUE,
            defaultPageSize = 100,
            striped = TRUE
        )
      })

      # returnData ----
      returnData <- reactiveVal(NULL)
      observe({
        req(configIsValid())
        plot_risk_interactive(
          dataset = rescaledScores(),
          ll = leafletProxy("map")
        )
        returnData(list(
          dataset = rescaledScores(),
          config = list(
            rescaling_args = rescaling_args()
          )))
      })

      returnData
    })
}
