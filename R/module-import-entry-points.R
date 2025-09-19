#' Entry Points Import Module UI
#'
#' Creates a modal dialog interface for uploading entry points data with
#' file upload, column mapping, and preview functionality.
#'
#' @param id Character string. The namespace id for the module.
#' @return A modal dialog UI element for importing entry points.
#'
#' @keywords internal
#' @importFrom shiny NS modalDialog fluidRow column tags fileInput uiOutput actionButton
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom esquisse dragulaInput
importEntryPointsUI <- function(id) {
  ns <- shiny::NS(id)

  modalDialog(
    width = 10, offset = 1,
    title = titleWithHelpKey("import-entry-points-title"),
    size = "xl",
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
        ),
        width = "100%"
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
#' @keywords internal
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
#'  actionButton removeModal HTML tags
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
      ext <- tolower(tools::file_ext(file$name))
      if (any(ext %in% c("csv", "txt", "tsv"))) {
        # CSV file - read with readr
        safely_read_delim <- safely(readr::read_delim)
        result <- safely_read_delim(file$datapath, show_col_types = FALSE)
        result$type <- "tabular"

      } else if (any(ext %in% c("shp"))) {
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

      } else if (any(ext %in% c("gpkg", "geojson"))) {
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

    # dragula ----
    output$dragula <- renderUI({
      req(importTable())

      dataset <- req(importTable()$result)
      sources <- colnames(dataset)
      type <- importTable()$type

      if (identical(type, "tabular")) {
        required <- c("point_name","lng","lat")
        optional <- c("mode","sources","type")
      } else {
        sources <- sources[sources!="geometry"]
        required <- c("point_name")
        optional <- c("mode","sources","type")
      }

      required_label <- get_label(required)
      required_label <- paste(required_label, "*")
      optional_label <- get_label(optional)
      optional_label[1:2] <- paste(optional_label[1:2], "**")
      targetIds <- c(required, optional)
      targets <- setNames(targetIds, nm = c(required_label, optional_label))
      target_status <- ifelse(targetIds %in% required, "primary", "warning")
      targetActions <- lapply(as.list(setNames(nm = targetIds)), function(x){
        helpPopup(get_help(x))
        })
      tagList(
        customDragulaInput(
          inputId = ns("col_mapping"),
          label = NULL,
          sourceLabel = "Available columns",
          sourceActions = helpPopup(get_help("mapping_tooltip")),
          targetActions = targetActions,
          targets = targets,
          choices = sources,
          selected = auto_select_cols(
            user_cols = sources,
            options = targetIds
          ),
          replace = TRUE,
          choice_status = "primary",
          target_status = target_status,
          badge = TRUE,
          ncolGrid = if(identical(type, "tabular")) 3 else 2,
          flip = FALSE
        ),
        HTML(get_help("entry_points_mapping")),
        tags$br()
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
    configIsValid <- reactive(label = paste0("configIsValid-import-", id), {
      if (!isTruthy(importTable())) {
        status <- build_config_status(
          value = FALSE,
          msg = "No dataset has been imported."
        )
        return(status)
      }

      if (!isTruthy(validationStatus())) {
        status <- build_config_status(
          value = FALSE,
          msg = "Dataset validation has not run yet."
        )
        return(status)
      }

      if (!is_dataset_valid(validationStatus())) {
        status <- build_config_status(
          value = FALSE,
          msg = "Dataset has not been validated."
        )
        return(status)
      }

      if (!isTruthy(validatedDataset())) {
        status <- build_config_status(
          value = FALSE,
          msg = "Validated dataset not found."
        )
        return(status)
      }

      build_config_status(
        value = TRUE,
        msg = "Import configuration is valid."
      )
    })

    output$config_is_valid <- renderUI({
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
    returnData <- reactiveVal(NULL)
    observeEvent(input$import_data, {
      returnData(validatedDataset())
      importTable(NULL) # Reset to empty
      removeModal()
    })

    # Cancel import ----
    observeEvent(input$cancel_import, {
      importTable(NULL) # Reset to empty
      removeModal()
    })

    # Return validated dataset ----
    return(returnData)
  })
}
