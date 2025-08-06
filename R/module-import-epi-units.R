#' Epidemiological Units Import Module UI
#'
#' Creates a simple import button for uploading geospatial epidemiological units data.
#'
#' @param id Character string. The namespace id for the module.
#' @return An action button for importing epidemiological units.
#'
#' @export
#' @importFrom shiny NS actionButton
importEpiUnitsUI <- function(id) {
  ns <- shiny::NS(id)
  actionButton(
    inputId = ns("do_import_epi_units"),
    label = "Import",
    width = "100%"
  )
}

#' Epidemiological Units Import Module Server
#'
#' Handles geospatial file import for epidemiological units with:
#' - File upload (shapefile, GeoPackage, GeoJSON support)
#' - Interactive column mapping via drag-and-drop interface
#' - Map and table preview of imported data
#' - Data validation and error reporting
#'
#' @param id Character string. The namespace id for the module.
#' @return A reactive function returning the validated epidemiological units dataset.
#'
#' @export
#' @importFrom riskintrodata read_geo_file
#' @importFrom purrr safely
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom reactable reactableOutput renderReactable reactable
#' @importFrom shinyWidgets alert panel
#' @importFrom esquisse dragulaInput
#' @importFrom shinyjs enable disable
#' @importFrom shiny
#'  moduleServer reactiveVal observeEvent renderUI req
#'  isTruthy observe showModal modalDialog fluidRow column fileInput uiOutput
#'  actionButton removeModal
#' @importFrom leaflet
#'  leafletOutput renderLeaflet
#' @importFrom riskintrodata
#'  validate_dataset is_dataset_valid extract_dataset
importEpiUnitsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    importTable <- reactiveVal(NULL)
    validationStatus <- reactiveVal(NULL)
    returnDataset <- reactiveVal(NULL)

    observeEvent(input$file, {
      file <- input$file
      # Handling shapefiles differently because they are made up of multiple files
      shp <- file$datapath[endsWith(file$datapath, suffix = ".shp")]
      if (length(shp) > 0) {
        # restore original basenames
        file$new_name <- file.path(dirname(file$datapath), file$name)
        file.rename(from = file$datapath, to = file$new_name)
      }
      safely_read_geo_file <- safely(read_geo_file)
      polygon <- safely_read_geo_file(file$new_name %||% file$datapath)
      importTable(polygon)
    })

    output$import_error <- renderUI({
      req(importTable())
      req(isTruthy(importTable()$error))
      shinyWidgets::alert(
        HTML(paste("Unable to import file:", tags$br(), importTable()$error)),
        status = "danger"
      )
    })
    output$map_preview <- renderLeaflet({
      req(importTable(), !isTruthy(importTable()$error))
      preview_map(importTable()$result)
    })
    output$table_preview <- renderReactable({
      req(importTable(), !isTruthy(importTable()$error))
      reactable(importTable()$result)
    })
    output$dragula <- renderUI({
      req(importTable())
      req(importTable()$result)
      dataset <- req(importTable()$result)
      sources <- colnames(dataset)
      spec <- riskintrodata:::.spec_epi_units
      required <- map(spec, \(x) x[["required"]])
      optional <- names(required)[!unlist(required)]
      optional_label <- paste(optional, "(optional)")
      required <- names(required)[unlist(required)]
      required <- required[required != "geometry"]
      targetsLabels <- c(required, optional_label)
      targetsIds <- c(required, optional)

      tagList(
        div(h4("Select column mapping for analysis operations")),
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

    observe({
      req(importTable())
      dataset <- req()
      mapping <- req(input$col_mapping)
      args <- mapping$target
      args <- append(
        args,list(x = importTable()$result,
                  table_name = "epi_units"))
      validation_status <- do.call(validate_dataset, args)
      validationStatus(validation_status)
    })

    output$validation_error <- renderUI({
      validation_status_ui(validationStatus())
    })

    # Modal UI ----
    observeEvent(input$do_import_epi_units, {
      showModal(modalDialog(
        width = 10, offset = 1,
        title = "Import epidemiological units",
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        fluidRow(column(
          width = 10, offset = 1,
          tags$p("Import a geospatial file with Browse..."),
          tags$p("For shapefiles (shp), make sure to select all the neccessary files."),
          fileInput(
            inputId = ns("file"),
            label = NULL,
            multiple = TRUE,
            accept = c(
              ".shp",
              ".dbf",
              ".sbn",
              ".sbx",
              ".shx",
              ".prj",
              ".gpkg",
              ".geojson",
              ".qmd",
              ".cpg"
            ),
            width = "100%"
          ),
          uiOutput(ns("import_error")),
          navset_card_tab(
            id = ns("panel_ui"),
            nav_panel(
              title = "Map view",
              leafletOutput(outputId = ns("map_preview"))
            ),
            nav_panel(
              title = "Table view",
              reactableOutput(outputId = ns("table_preview"))
            )
          ),
          uiOutput(ns("dragula")),
          uiOutput(ns("validation_error"))
        )),
      footer = list(
        actionButton(
          inputId = ns("apply"),
          class = "btn-primary",
          label = "Import",
          disabled = TRUE),
        actionButton(
          inputId = ns("cancel"),
          label = "Cancel",
          class = "btn-default"
        )
      )
      ))

      apply_btn_observer <<- observe({
        if (is_dataset_valid(validationStatus())) {
          shinyjs::enable("apply")
        } else {
          disable(id = "apply")
        }
      })
    })
    observeEvent(input$apply,{
      removeModal()
      if (!is.null(apply_btn_observer)) {
        apply_btn_observer$destroy()
        apply_btn_observer <- NULL
      }
      returnDataset(extract_dataset(validationStatus()))
    })
    observeEvent(input$cancel,{
      removeModal()
      if (!is.null(apply_btn_observer)) {
        apply_btn_observer$destroy()
        apply_btn_observer <- NULL
      }
    })

    returnDataset
  })
}

