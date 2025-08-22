#' Entry Points Import Module UI
#'
#' Creates a modal dialog interface for uploading entry points data with
#' file upload, column mapping, and preview functionality.
#'
#' @param id Character string. The namespace id for the module.
#' @return A modal dialog UI element for importing entry points.
#'
#' @export
#' @importFrom shiny NS modalDialog fluidRow column tags fileInput uiOutput actionButton
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom esquisse dragulaInput
importEntryPointsUI <- function(id) {
  ns <- shiny::NS(id)

  modalDialog(
    width = 10, offset = 1,
    title = "Import entry points",
    size = "l",
    easyClose = TRUE,
    fade = TRUE,
    fluidRow(column(
      width = 10, offset = 1,
      tags$p("Import entry points data from file. Supported formats:"),
      tags$ul(
        tags$li("CSV files with longitude/latitude columns"),
        tags$li("Shapefiles (.shp bundle - select all files)"),
        tags$li("GeoPackage (.gpkg files)"),
        tags$li("GeoJSON files")
      ),
      fileInput(
        inputId = ns("file"),
        label = NULL,
        multiple = TRUE,
        accept = c(
          ".csv",
          ".tsv",
          ".txt",
          ".shp",
          ".dbf",
          ".sbn",
          ".sbx",
          ".shx",
          ".prj",
          ".qmd",
          ".gpkg",
          ".geojson"
        )
      ),
      uiOutput(ns("import_error")),
      navset_card_tab(
        nav_panel(
          "Map preview",
          leafletOutput(ns("map"), height = 400)
        ),
        nav_panel(
          "Table preview",
          reactableOutput(ns("table"))
        )
      ),
      uiOutput(ns("dragula")),
      uiOutput(ns("validation_error"))
    )),
    footer = list(
      uiOutput(ns("config_is_valid")),
      actionButton(
        ns("import_data"),
        "Import data",
        class = "btn btn-primary"
      ),
      actionButton(
        ns("cancel_import"),
        "Cancel",
        class = "btn btn-secondary"
      )
    )
  )
}

#' Entry Points Import Module Server
#'
#' Handles file import for entry points with:
#' - File upload (CSV, shapefile, GeoPackage, GeoJSON support)
#' - Interactive column mapping via drag-and-drop interface
#' - Map and table preview of imported data
#' - Data validation and error reporting
#'
#' @param id Character string. The namespace id for the module.
#' @return A reactive function returning the validated entry points dataset.
#'
#' @export
#' @importFrom riskintrodata read_geo_file validate_dataset extract_dataset is_dataset_valid
#' @importFrom purrr safely map
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom reactable reactableOutput renderReactable reactable
#' @importFrom shinyWidgets alert panel
#' @importFrom esquisse dragulaInput
#' @importFrom shinyjs enable disable
#' @importFrom readr read_csv
#' @importFrom shiny
#'  moduleServer reactiveVal observeEvent renderUI req
#'  isTruthy observe showModal modalDialog fluidRow column fileInput uiOutput
#'  actionButton removeModal showNotification HTML tags
#' @importFrom leaflet
#'  leafletOutput renderLeaflet addCircleMarkers addTiles setView fitBounds
#' @importFrom sf st_as_sf st_geometry_type st_drop_geometry st_bbox
importEntryPointsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    importTable <- reactiveVal(NULL)

    # File upload handling ----
    observeEvent(input$file, {
      file <- input$file
      req(file)
      # Handle different file types
      if (any(tools::file_ext(file$name) %in% c("csv", "txt", "tsv"))) {
        # CSV file - read with readr
        safely_read_delim <- safely(readr::read_delim)
        result <- safely_read_delim(file$datapath, show_col_types = FALSE)
        result$type <- "tabular"

      } else if (any(tools::file_ext(file$name) %in% c("shp"))) {
        # Geospatial files - handle shapefiles differently
        shp <- file$datapath[endsWith(file$datapath, suffix = ".shp")]
        if (length(shp) > 0) {
          # Restore original basenames for shapefiles
          file$new_name <- file.path(dirname(file$datapath), file$name)
          file.rename(from = file$datapath, to = file$new_name)
        }
        safely_read_geo_file <- safely(read_geo_file)
        result <- safely_read_geo_file(file$new_name %||% file$datapath)
        result$type <- "geospatial"

      } else if (any(tools::file_ext(file$name) %in% c("gpkg", "geojson"))) {
        safely_read_geo_file <- safely(read_geo_file)
        result <- safely_read_geo_file(file$datapath)
        result$type <- "geospatial"

      } else {
        result <- list(result = NULL, error = "This file type is not supported.")
      }
      importTable(result)
    })

    # Error display ----
    output$import_error <- renderUI({
      req(importTable())
      req(importTable()$error)
      alert_error(
        text = "Error while reading file:",
        error = importTable()$error
      )
    })

    # Column mapping interface ----
    output$dragula <- renderUI({
      req(importTable())
      dataset <- req(importTable()$result)
      sources <- colnames(dataset)

      # Get entry points specification
      spec <- riskintrodata:::.spec_entry_points
      required <- map(spec, \(x) x[["required"]])
      optional <- names(required)[!unlist(required)]
      optional_label <- paste(optional, "(optional)")
      required <- names(required)[unlist(required)]
      required <- required[required != "geometry"]  # Geometry handled separately
      targetsLabels <- c(required, optional_label)
      targetsIds <- c(required, optional)

      tagList(
        div(h4("Select column mapping for entry points analysis")),
        dragulaInput(
          inputId = ns("col_mapping"),
          label = NULL,
          sourceLabel = "Imported data columns",
          targetsLabels = targetsLabels,
          targetsIds = targetsIds,
          choices = sources,
          selected = auto_select_cols(
            user_cols = sources,
            options = targetsIds
          ),
          replace = TRUE,
          copySource = TRUE,
          width = "100%"
        )
      )
    })

    # Validation logic ----
    validationStatus <- reactive({
      req(importTable())
      dataset <- req(importTable()$result)
      mapping <- req(input$col_mapping)
      args <- mapping$target
      args <- append(
        args,
        list(
          x = dataset,
          table_name = "entry_points"
        )
      )
      safely_validate_dataset <- safely(validate_dataset)
      validation_status <- do.call(safely_validate_dataset, args)
      if (is_error(validation_status$error)){
        shiny::showNotification("'validate_dataset' has thrown an error!")
        return(NULL)
        } else {
        validation_status$result
      }
    })

    output$validation_error <- renderUI({
      validation_status_ui(validationStatus())
    })

    validatedDataset <- reactive({
      req(is_dataset_valid(validationStatus()))
      extract_dataset(validationStatus())
    })


    # configIsValid ----
    configIsValid <- reactive({
      config_is_valid(
        "import_entry_points",
        import_table = importTable(),
        dataset_validation_status = validationStatus(),
        validated_dataset = validatedDataset()
      )
    })

    output$config_is_valid <- reactive({
      report_config_status(configIsValid())
    })

    # Map view ----
    output$map <- renderLeaflet({
      if (isTruthy(validatedDataset())) {
        preview_map(validatedDataset())
      } else if (isTruthy(importTable()) && "sf" %in% class(importTable())) {
        preview_map(importTable())
      } else {
        NULL
      }
    })
    # Table view ----
    output$table <- renderReactable({
      req(importTable(), !isTruthy(importTable()$error))
      data <- importTable()$result
      data <- sf::st_drop_geometry(data)
      reactable(data)
    })

    observe({
      if (configIsValid()) {
        shinyjs::enable("import_data")
      } else {
        shinyjs::disable("import_data")
      }
    })

    # Import confirmation ----
    importedDataset <- reactiveVal(NULL)
    observeEvent(input$import_data, {

    })

    # Cancel import ----
    observeEvent(input$cancel_import, {
      # Reset internal state
      importTable(NULL)
    })

    # Return validated dataset ----
    return(importedDataset)
  })
}

#' @export
config_is_valid.import_entry_points <- function(x, ...){

  x <- list(...)
  import_table <- x$import_table
  dataset_validation_status <- x$dataset_validation_status
  validated_dataset <- x$validated_dataset

  if (!isTruthy(import_table)) {
    status <- build_config_status(
      value = FALSE,
      msg = "No dataset has been imported."
    )
    return(status)
  }

  if (!isTruthy(dataset_validation_status)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Dataset validation has not run yet."
    )
    return(status)
  }

  if (!is_dataset_valid(dataset_validation_status)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Dataset is has not been validated."
    )
    return(status)
  }

  if (!is_dataset_valid(dataset_validation_status)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Dataset is has not been validated."
    )
    return(status)
  }

  if (!isTruthy(validated_dataset)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Validated dataset not found."
    )
    return(status)
  }

  build_config_status(
    value = TRUE,
    msg = "Configuration is valid."
  )
}



# Helper functions ----

#' Create preview map for entry points
#' @param data sf object with entry points
#' @return leaflet map
#' @noRd
preview_entry_points_map <- function(data) {
  req(inherits(data, "sf"))

  # Basic validation that we have point geometry
  geom_types <- unique(as.character(st_geometry_type(data)))
  if (!all(geom_types == "POINT")) {
    # Return basic map if not all points
    return(leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(lng = 0, lat = 0, zoom = 2))
  }

  # Create leaflet map with entry points
  bbox <- sf::st_bbox(data)

  leaflet::leaflet(data) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(
      radius = 6,
      fillColor = "#ff7f0e",
      color = "#ffffff",
      weight = 1,
      fillOpacity = 0.8,
      popup = ~ifelse(
        "point_name" %in% names(data),
        paste0("<strong>", point_name, "</strong>"),
        paste0("Entry Point ", seq_len(nrow(data)))
      )
    ) |>
    leaflet::fitBounds(
      lng1 = bbox[1], lat1 = bbox[2],
      lng2 = bbox[3], lat2 = bbox[4]
    )
}
