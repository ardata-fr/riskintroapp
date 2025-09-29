#' Epidemiological Units Import Module UI
#'
#' Creates a simple import button for uploading geospatial epidemiological units data.
#'
#' @param id Character string. The namespace id for the module.
#' @return An action button for importing epidemiological units.
#'
#' @keywords internal
#' @importFrom shiny NS actionButton
importEpiUnitsUI <- function(id) {
  ns <- NS(id)

  modalDialog(
    title = titleWithHelpKey("import-epi-units-title"),
    size = "xl",
    easyClose = FALSE,
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
      uiOutput(ns("dataset_validation"))
    )),
    footer = list(
      uiOutput(ns("config_is_valid")),
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
#' @param is_overwriting reactive logical. Does the dataset being imported already exist?
#' @return A reactive function returning the validated epidemiological units dataset.
#'
#' @keywords internal
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
importEpiUnitsServer <- function(id, is_overwriting) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # File import ----
    importTable <- reactiveVal(NULL)
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

    # import_error ----
    output$import_error <- renderUI({
      if (is_overwriting() && is.null(importTable())) {
        return(alert(
            status = "warning", "Importing a new dataset will overwrite the existing one."
          ))
      }
      req(importTable())
      if (is_error(importTable()$error)) {
        alert_error(
          text = "Error while reading file:",
          error = importTable()$error
        )
      } else {
        alert(
          status = "success",
          sprintf("File geometry found in column: \"%s\"",  attr(importTable()$result, "sf_column"))
        )
      }
    })

    # Map ----
    output$map_preview <- renderLeaflet({
      req(importTable(), !isTruthy(importTable()$error))
      preview_map(importTable()$result)
    })
    # Table ----
    output$table_preview <- renderReactable({
      req(importTable(), !isTruthy(importTable()$error))
      reactable(importTable()$result)
    })

    # dragula  ----
    output$dragula <- renderUI({
      req(importTable())
      req(importTable()$result)
      dataset <- req(importTable()$result)
      sources <- colnames(dataset)
      # remove geometry as an option, it is handled automatically
      sources <- sources[sources != attr(dataset, "sf_column")]

      required <- c("eu_name")
      optional <- c("eu_id")
      required_labels <- get_label(required)
      optional_labels <- get_label(optional)
      required_labels <- paste(required_labels, "*")

      targetsLabels <- c(required_labels, optional_labels)
      targetIds <- c(required, optional)
      targets <- setNames(targetIds, nm = targetsLabels)
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
          ncolGrid = 2,
          flip = FALSE
        ),
        HTML(get_help("epi_units_mapping"))
      )
    })

    # configIsValid ----
    configIsValid <- reactive(label = paste0("configIsValid-import-", id), {
      req(importTable())
      dataset <- req()
      mapping <- req(input$col_mapping)
      args <- mapping$target
      args <- append(
        args,list(x = importTable()$result,
                  table_name = "epi_units"))

      safely_validate_dataset <- safely(validate_dataset)
      res <- do.call(safely_validate_dataset, args)
      # catch all in case of errors
      if (is_error(res$error)) {
        status <- build_config_status(
          value = FALSE,
          msg = res$error
        )
        res$status <- status
      } else if (is_dataset_valid(res$result)) {
        status <- build_config_status(
          value = TRUE,
          msg = "Import configuration is valid."
        )
        res$status <- status
      } else {
        status <- build_config_status(
          value = FALSE,
          msg = "Dataset is not valid, see above."
        )
        res$status <- status
      }
      res
    })

    output$dataset_validation <- renderUI({
      validation_status_ui(configIsValid()$result)
    })

    output$config_is_valid <- renderUI({
      report_config_status(configIsValid()$status, in_panel = FALSE)
    })

    observe({
      if (configIsValid()$status) {
        shinyjs::enable("apply")
      } else {
        disable(id = "apply")
      }
    })

    # returnDataset ----
    returnDataset <- reactiveVal(NULL)

    observeEvent(input$apply,{
      removeModal()
      returnDataset(extract_dataset(configIsValid()$result))
      importTable(NULL)
    })

    observeEvent(input$cancel,{
      removeModal()
      importTable(NULL)
    })

    returnDataset
  })
}

