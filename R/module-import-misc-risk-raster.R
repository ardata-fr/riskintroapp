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
      inlineComponents(
        shinyWidgets::awesomeCheckbox(
          inputId = ns("na_replace"),
          value = FALSE,
          label = "Replace values with NA"
        ),
        conditionalPanel(
          condition = "input.na_replace",
          ns = ns,
          numericInput(
            inputId = ns("na_replace_value"),
            label = "Value to replace",
            value = NULL,
            updateOn = "blur"
          )
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
#' @importFrom terra rast subst project
#' @importFrom riskintroanalysis calc_road_access_risk
#' @importFrom shiny
#'  moduleServer req reactiveVal renderUI observeEvent updateSelectInput observe removeModal
#' @importFrom leaflet
#'  leaflet addTiles addRasterImage renderLeaflet colorNumeric labelFormat
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
        }
        read_error(NULL)
        raster <- raster_result$result

        if (input$na_replace) {
          req(input$na_replace_value)
          safely_subst <- purrr::safely(terra::subst)
          subst_result <- safely_subst(raster, input$na_replace_value, NA)
          if(is_error(subst_result$error)) {
            read_error(subst_result$error)
            return(NULL)
          }
          raster <- subst_result$result
        }

        if (input$scale[[1]] < input$scale[[2]]){ # validate scale
          raster <- terra::clamp(
            x = raster,
            lower = input$scale[[1]],
            upper = input$scale[[2]],
            values = TRUE
          )
        }

        raster
      })

      observe({
        req(importRaster())
        choices <- names(importRaster())
        if(length(choices) > 0) {
          updateTextInput(
            inputId = "name",
            value = choices[[1]],
          )
          updateSelectInput(
            inputId = "layer",
            choices = choices
          )
        }
      })

      # Update scale input ----
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
        raster <- req(importRaster())
        req(epi_units(), input$aggregate_fun)
        safely_augment_epi_units_with_raster <- safely(augment_epi_units_with_raster)
        result <- safely_augment_epi_units_with_raster(
          epi_units = epi_units(),
          raster = raster,
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

      # Raw map ----
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
            group = "raster",
          ) |>
          addLegend_decreasing(
            pal = pal,
            values = input$scale,
            title = "Raw Values"
          )
      })

      # Aggregated map ----
      output$aggregated_map <- renderLeaflet({
        req(configIsValid())
        extracted_risk <- extractedRisk()
        risk_values <- extracted_risk[[input$name]]
        pal <- leaflet::colorNumeric(
          palette = "inferno",
          domain = input$scale,
          na.color = "lightgrey"
        )
        basemap() |>
          addPolygons(
            data = extracted_risk,
            fillColor = pal(risk_values),
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
          addLegend_decreasing(
            pal = pal,
            values = input$scale,
            title = "Aggregated Score"
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
          initial_scale = input$scale,
          risk_col = input$risk_col,
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
  if(terra::nlyr(raster) == 0) {
    status <- build_config_status(
      value = FALSE,
      msg = "Raster file contains no data layers."
    )
    return(status)
  }

  if(!terra::hasValues(raster)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Raster has no values."
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


  if (parameters$name %in% c('epi_units', "emission_risk_factors", "overall_risk")) {
    status <- build_config_status(
      value = FALSE,
      msg = "This risk name is reserved."
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

  # extracted risk data ----
  if(!isTruthy(extracted_risk)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Risk extraction failed. Check raster data and epidemiological units."
    )
    return(status)
  }

  if (nrow(extracted_risk) == 0) {
    status <- build_config_status(
      value = FALSE,
      msg = "Aggregation resulted in no values. The raster does not overlap epidemiological units."
    )
    return(status)
  }

  risk_values <- extracted_risk[[parameters$name]]
  if (all(is.na(risk_values))) {
    status <- build_config_status(
      value = FALSE,
      msg = "Aggregation resulted only missing values. The raster does not overlap epidemiological units."
    )
    return(status)
  }

  if(any(is.infinite(risk_values), na.rm = TRUE)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Extracted risk values contain infinite values."
    )
    return(status)
  }

  build_config_status(
    value = TRUE,
    msg = "Configuration is valid."
  )
}
