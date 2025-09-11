#' Animal Mobility Import Module UI
#'
#' Creates a modal dialog interface for uploading animal mobility data with
#' file upload, column mapping, and preview functionality.
#'
#' @param id Character string. The namespace id for the module.
#' @return A modal dialog UI element for importing animal mobility data.
#'
#' @export
#' @importFrom shiny NS modalDialog fluidRow column tags fileInput uiOutput actionButton
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom esquisse dragulaInput
importAnimalMobilityUI <- function(id) {
  ns <- shiny::NS(id)

  modalDialog(
    title = "Import animal mobility data",
    size = "l",
    easyClose = FALSE,
    fade = TRUE,
    fluidRow(column(
      width = 10, offset = 1,
      tags$p("Import animal mobility data from file. Supported formats:"),
      tags$ul(
        tags$li("CSV files (.csv)"),
        tags$li("Tab-separated files (.tsv)"),
        tags$li("Text files (.txt)"),
        tags$li("Excel files (.xlsx)")
      ),
      fileInput(
        inputId = ns("file"),
        label = NULL,
        multiple = FALSE,
        accept = c(".csv", ".tsv", ".txt", ".xlsx"),
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
        disabled = TRUE
      ),
      actionButton(
        inputId = ns("cancel"),
        label = "Cancel",
        class = "btn-default"
      )
    )
  )
}

#' Animal Mobility Import Module Server
#'
#' Handles animal mobility file import with:
#' - File upload (CSV, TSV, TXT, Excel support)
#' - Interactive column mapping via drag-and-drop interface
#' - Map and table preview of imported data
#' - Data validation using riskintrodata validate_dataset
#'
#' @param id Character string. The namespace id for the module.
#' @param is_overwriting reactive logical. Does the dataset being imported already exist?
#' @return A reactive function returning the validated animal mobility dataset.
#'
#' @export
#' @importFrom purrr safely map
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom reactable reactableOutput renderReactable reactable
#' @importFrom shinyWidgets alert panel
#' @importFrom esquisse dragulaInput
#' @importFrom shinyjs enable disable
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom shiny
#'  moduleServer reactiveVal observeEvent renderUI req
#'  isTruthy observe showModal modalDialog fluidRow column fileInput uiOutput
#'  actionButton removeModal
#' @importFrom leaflet
#'  leafletOutput renderLeaflet
#' @importFrom riskintrodata
#'  validate_dataset is_dataset_valid extract_dataset
importAnimalMobilityServer <- function(id, is_overwriting = reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # File import ----
    importTable <- reactiveVal(NULL)
    observeEvent(input$file, {
      file <- input$file
      fp <- file$datapath
      ext <- tools::file_ext(file$name)

      safely_read <- safely(function(path, extension) {
        switch(extension,
          "csv" = readr::read_delim(path, show_col_types = FALSE),
          "tsv" = readr::read_delim(path, show_col_types = FALSE, col_names = TRUE),
          "txt" = readr::read_delim(path, show_col_types = FALSE),
          "xlsx" = readxl::read_excel(path),
          stop("Unsupported file format: ", extension)
        )
      })

      result <- safely_read(fp, ext)
      importTable(result)
    })

    # import_error ----
    output$import_error <- renderUI({
      if (is_overwriting() && is.null(importTable())) {
        return(alert(
          status = "warning",
          "Importing a new dataset will overwrite the existing one."
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
          sprintf("Successfully loaded %d rows and %d columns",
                  nrow(importTable()$result),
                  ncol(importTable()$result))
        )
      }
    })

    # Map preview ----
    output$map_preview <- renderLeaflet({
      req(importTable(), !isTruthy(importTable()$error))

      # Check if we have destination coordinates for preview
      data <- importTable()$result
      if (all(c("d_lng", "d_lat") %in% names(data))) {
        # Simple preview map with destination points
        leaflet(data) |>
          addTiles() |>
          addMarkers(
            lng = ~d_lng,
            lat = ~d_lat,
            popup = ~paste("Destination:", if("d_name" %in% names(data)) d_name else "Unknown")
          )
      } else {
        # Fallback map if coordinates not available yet
        leaflet() |>
          addTiles() |>
          setView(lng = 0, lat = 0, zoom = 2)
      }
    })

    # Table preview ----
    output$table_preview <- renderReactable({
      req(importTable(), !isTruthy(importTable()$error))
      reactable(importTable()$result,
                searchable = TRUE,
                defaultPageSize = 10,
                showPageSizeOptions = TRUE)
    })

    # Dragula UI ----
    output$dragula <- renderUI({
      req(importTable())
      req(importTable()$result)
      dataset <- req(importTable()$result)
      sources <- colnames(dataset)

      # Get animal mobility specification
      spec <- riskintrodata:::.spec_animal_mobility
      required <- map(spec, \(x) x[["required"]])
      optional <- names(required)[!unlist(required)]
      optional_label <- paste(optional, "(optional)")
      required <- names(required)[unlist(required)]

      targetsLabels <- c(required, optional_label)
      targetsIds <- c(required, optional)

      tagList(
        div(h4("Select column mapping for animal mobility data")),
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

    # configIsValid ----
    configIsValid <- reactive({
      req(importTable())
      dataset <- importTable()$result
      mapping <- req(input$col_mapping)
      args <- mapping$target
      args <- append(
        args, list(
          x = dataset,
          table_name = "animal_mobility"
        )
      )

      safely_validate_dataset <- safely(validate_dataset)
      res <- do.call(safely_validate_dataset, args)

      # Handle validation results
      if (is_error(res$error)) {
        status <- build_config_status(
          value = FALSE,
          msg = "Validation error occurred.",
          error = res$error
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
          msg = "Dataset is not valid, see validation details above."
        )
        res$status <- status
      }
      res
    })

    output$dataset_validation <- renderUI({
      validation_status_ui(configIsValid()$result)
    })

    output$config_is_valid <- renderUI({
      report_config_status(configIsValid()$status)
    })

    observe({
      if (isTruthy(configIsValid()$status) && isTRUE(configIsValid()$status)) {
        shinyjs::enable("apply")
      } else {
        shinyjs::disable("apply")
      }
    })

    # returnDataset ----
    returnDataset <- reactiveVal(NULL)

    observeEvent(input$apply, {
      removeModal()
      returnDataset(extract_dataset(configIsValid()$result))
      importTable(NULL)
    })

    observeEvent(input$cancel, {
      removeModal()
      importTable(NULL)
    })

    returnDataset
  })
}
