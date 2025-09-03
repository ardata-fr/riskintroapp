#' Import Miscellaneous Risk from Raster UI
#'
#' Creates a modal dialog interface for importing risk scores from raster files
#' and processing them for epidemiological risk analysis.
#'
#' @param id Character string. Module namespace identifier.
#'
#' @return A modal dialog UI element for raster file import and configuration.
#'
#' @importFrom shinyWidgets numericRangeInput
#' @importFrom shiny NS modalDialog fluidRow column tags fileInput textInput selectInput actionButton modalButton icon uiOutput
#' @importFrom leaflet leafletOutput
#' @importFrom bslib card card_body card_header layout_column_wrap
#'
#' @export
importMiscRiskRasterUI <- function(id) {
  ns <- NS(id)

  modalDialog(
    title = "Import risk from raster file",
    fluidRow(column(
      width = 10, offset = 1,
      tags$p("Import raster file (.tif, .tiff, .grd, .asc) containing risk values."),
      tags$p("Risk values will be extracted for each epidemiological unit using zonal statistics."),
      fileInput(
        inputId = ns("file"),
        label = "Select raster file",
        multiple = FALSE,
        accept = c(".tif", ".tiff", ".grd", ".asc"),
        width = "100%"
      ),
      inlineComponents(
        textInput(
          inputId = ns("name"),
          label = "Risk name"
        ),
        selectInput(
          inputId = ns("layer"),
          label = "Select raster layer",
          choices = character()
        )
      ),
      uiOutput(ns("read_error_ui")),
      inlineComponents(
        awesomeRadio(
          inputId = ns("aggregate_fun"),
          label = "Aggregation method",
          choices = c("mean", "max", "min", "sum"),
          selected = "mean"
        ),
        shinyWidgets::numericRangeInput(
          inputId = ns("scale"),
          label = "Risk score scale",
          value = c(0, 0)
        ),
        actionButton(
          inputId = ns("minmax_scale"),
          label = NULL,
          icon = icon("arrows-left-right-to-line"),
          width = "75px"
        )
      ),
      layout_column_wrap(
        width = 1/2,
        height = 400,
        fill = FALSE,

        bslib::card(
          full_screen = FALSE,
          bslib::card_header("Raw Raster Data"),
          bslib::card_body(
            class = "p-0",
            leafletOutput(ns("raw_map"), height = "350px")
          )
        ),
        bslib::card(
          full_screen = FALSE,
          bslib::card_header("Aggregated Risk Values"),
          bslib::card_body(
            class = "p-0",
            leafletOutput(ns("aggregated_map"), height = "350px")
          )
        )
      )
    )),
    footer = list(
      uiOutput(ns("config_is_valid")),
      actionButton(
        inputId = ns("apply"),
        class = "btn-primary",
        label = "Import",
        disabled = TRUE
      ),
      modalButton(label = "Cancel")
    ),
    size = "xl", easyClose = TRUE
  )
}

#' Import Miscellaneous Risk from Raster Server
#'
#' Server logic for importing risk scores from raster files with zonal statistics
#' extraction and validation for epidemiological risk analysis.
#'
#' @param id Character string. Module namespace identifier.
#' @param riskMetaData Reactive value containing existing risk metadata list.
#' @param epi_units Reactive expression returning epidemiological units sf object.
#'
#' @return Reactive expression returning risk metadata list when import is completed.
#'
#' @importFrom shinyWidgets updateNumericRangeInput
#' @importFrom terra rast values
#' @importFrom riskintroanalysis calc_road_access_risk
#' @importFrom shiny moduleServer req reactiveVal renderUI observeEvent updateSelectInput observe removeModal
#' @importFrom leaflet leaflet addTiles addRasterImage renderLeaflet colorNumeric
#'
#' @export
importMiscRiskRasterServer <- function(id, riskMetaData, epi_units) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Read raster data -----
      read_error <- reactiveVal(NULL)
      output$read_error_ui <- renderUI({
        req(read_error())
        alert_error(
          text = "Error: unable to read raster file",
          error = read_error()
        )
      })

      importRaster <- reactive({
        fp <- req(input$file$datapath)
        safely_rast <- safely(terra::rast)
        raster_result <- safely_rast(fp)
        if(is_error(raster_result$error)) {
          read_error(raster_result$error)
          return(NULL)
        } else {
          read_error(NULL)
          raster_result$result
        }
      })

      observe({
        req(importRaster())
        choices <- names(importRaster())
        if(length(choices) > 0) {
          updateTextInput(
            inputId = "name",
            value = names(importRaster())[[1]],
          )
          updateSelectInput(
            inputId = "layer",
            choices = names(importRaster())
          )
        }
      })

      # Update scale based on raster values ----
      observeEvent(input$minmax_scale, {
        req(input$layer)
        raster_data <- req(importRaster())
        min_max <- as.data.frame(terra::minmax(raster_data))[[input$layer]]
        req(min_max)
        updateNumericRangeInput(
          inputId = "scale",
          value = c(min_max[[1]], min_max[[2]])
        )
      })

      # Extract risk values for epidemiological units ----
      extractedRisk <- reactive({
        req(importRaster(), epi_units(), input$aggregate_fun)
        safely_augment_epi_units_with_raster <- safely(augment_epi_units_with_raster)
        result <- safely_augment_epi_units_with_raster(
          epi_units = epi_units(),
          raster = importRaster(),
          aggregate_fun = input$aggregate_fun,
          risk_name = input$name
        )
        if(is_error(result$error)) {
          read_error(result$error)
          return(NULL)
        } else {
          read_error(NULL)
          result$result
        }
      })

      # configIsValid ----
      configIsValid <- reactive({
        req(importRaster())
        config_is_valid(
          "import_misc_raster",
          raster = importRaster(),
          extracted_risk = extractedRisk(),
          epi_units = epi_units(),
          metadata = riskMetaData(),
          parameters = list(
            name = input$name,
            aggregate_fun = input$aggregate_fun,
            scale = input$scale
          )
        )
      })
      output$config_is_valid <- renderUI({
        report_config_status(configIsValid())
      })
      observe({
        if(configIsValid() && is.null(read_error())) {
          shinyjs::enable("apply")
        } else {
          shinyjs::disable("apply")
        }
      })

      # Raw raster map ----
      output$raw_map <- renderLeaflet({
        req(configIsValid())
        raster_layer <- importRaster()[[input$layer]]
        pal <- colorNumeric(
          "viridis",
          input$scale,
          na.color = "transparent"
        )
        basemap() |>
          addRasterImage(
            raster_layer,
            colors = pal,
            opacity = 0.8,
            group = "raster"
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal = pal,
            values = input$scale,
            title = "Raw Values"
          )
      })

      # Aggregated risk map ----
      output$aggregated_map <- renderLeaflet({
        req(configIsValid())
        extracted_risk <- extractedRisk()
        risk_values <- extracted_risk[[input$name]]
        pal <- leaflet::colorNumeric(
          palette = "viridis",
          domain = input$scale,
          reverse = TRUE,
          na.color = "lightgrey"
        )
        basemap() |>
          addPolygons(
            data = extracted_risk,
            fillColor = ~pal(risk_values),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            label = generate_leaflet_labels(
              extracted_risk,
              title_field = "eu_name",
              exclude_fields = "user_id"
              ),
            group = "risk"
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal = pal,
            values = risk_values,
            title = "Aggregated Risk"
          )
      })

      # Return values ----
      returnList <- reactiveVal(NULL)
      observeEvent(input$apply, {
        removeModal()
        dataset <- extractedRisk()
        attr(dataset, "scale") <- input$scale
        attr(dataset, "risk_col") <- input$name

        new_raster_risk <- list(
          name = input$name,
          type = "raster",
          join_by = "eu_id",
          dataset = dataset,
          rescale_args = list()
        )
        returnList(new_raster_risk)
      })

      return(returnList)
    })
}

#' @export
config_is_valid.import_misc_raster <- function(x, ...) {

  x <- list(...)

  raster <- x$raster
  extracted_risk <- x$extracted_risk
  epi_units <- x$epi_units
  metadata <- x$metadata
  parameters <- x$parameters

  # raster ----
  if(!isTruthy(raster)) {
    status <- build_config_status(
      value = FALSE,
      msg = "A raster file must be imported."
    )
    return(status)
  }

  # name ----
  if(!isTruthy(parameters$name)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Provide a risk name."
    )
    return(status)
  }
  if (parameters$name %in% names(metadata)) {
    status <- build_config_status(
      value = FALSE,
      msg = "There is already a risk with this name."
    )
    return(status)
  }

  # scale ----
  if (is.null(parameters$scale)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale not found."
    )
    return(status)
  }
  if (any(is.na(parameters$scale))) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale input must be numeric."
    )
    return(status)
  }
  if (length(parameters$scale) != 2) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale must have length 2."
    )
    return(status)
  }
  if (!is.numeric(parameters$scale)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale must be numeric."
    )
    return(status)
  }
  if (parameters$scale[[1]] == parameters$scale[[2]]) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale values should not be equal."
    )
    return(status)
  }
  if (parameters$scale[[1]] > parameters$scale[[2]]) {
    status <- build_config_status(
      value = FALSE,
      msg = "First scale value should be smaller than the second."
    )
    return(status)
  }

  # aggregation function ----
  valid_funs <- c("mean", "max", "min", "median", "sum")
  if(!parameters$aggregate_fun %in% valid_funs) {
    status <- build_config_status(
      value = FALSE,
      msg = paste("Aggregation function must be one of:", paste(valid_funs, collapse = ", "))
    )
    return(status)
  }

  build_config_status(
    value = TRUE,
    msg = "Configuration is valid."
  )
}
