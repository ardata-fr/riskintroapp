#' @export
#' @importFrom bslib navset_card_underline nav_panel
#' @importFrom reactable reactableOutput renderReactable reactable
#' @import leaflet
importEpiUnitsUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$p("Import a geospatial file with Browse..."),
    tags$p("For shapefiles (shp), make sure to import all associated files."),
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
    navset_card_underline(
      id = ns("panel_ui"),
      nav_panel(
        title = "Map",
        leafletOutput(outputId = ns("map_preview"))
      ),
      nav_panel(
        title = "Table",
        reactableOutput(outputId = ns("table_preview"))
      )
    ),
    uiOutput(ns("dragula")),
    textOutput(ns("selection"))
  )
}

#' @export
#' @importFrom riskintrodata read_geo_file
#' @importFrom purrr safely
importEpiUnitsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    importTable <- reactiveVal(NULL)
    validationStatus <- reactiveVal(NULL)

    observeEvent(input$file, {
      file <- input$file

      # Handling shapefiles differently because they are made up of multiple files
      shp <- file$datapath[endsWith(file$datapath, suffix = ".shp")]
      if (length(shp) > 0) {
        # restore original basenames
        file$new_name <- file.path(
          dirname(file$datapath),
          file$name
        )
        file.rename(
          from = file$datapath,
          to = file$new_name
        )
      }

      safe_read_geo_file <- safely(read_geo_file)
      polygon <- safe_read_geo_file(file$new_name)
      importTable(polygon)
    })

    output$import_error <- renderUI({
      req(importTable())
      req(isTruthy(importTable()$error))
      shinyWidgets::alert(
        paste("Unable to import file: \n", importTable()$error),
        status = "danger"
          )
    })
    output$map_preview <- renderLeaflet({
      req(importTable(), !isTruthy(importTable()$error))
      leaflet(importTable()$result) |> addPolygons() |> addTiles()
    })
    output$table_preview <- renderReactable({
      req(importTable(), !isTruthy(importTable()$error))
      reactable(importTable()$result)
    })
    output$dragula <- renderUI({
      req(importTable())
      req(importTable()$result)
      browser()
      dataset <- req(importTable()$result)
      sources <- colnames(dataset)
      targets <- names(riskintrodata:::.spec_epi_units)
      targets <- targets[targets != "geometry"]
      dragulaInput(
        inputId = ns("col_mapping"),
        label = "Column mapping",
        sourceLabel = "Imported data columns",
        targetsLabels = targets,
        targetsIds = targets,
        choices = sources,
        selected = auto_select_cols(
          user_cols = sources,
          options = targets
          ),
        replace = TRUE,
        copySource = TRUE
      )
    })

    observe({
      req(importTable())
      dataset <- req()
      mapping <- req(input$col_mapping)
      browser()
      args <- mapping$target
      args <- append(args,
                     list(x = importTable()$result,
                          table_name = "epi_units"))

      validation_status <- do.call(validate_dataset, args)
      validationStatus(validation_status)
    })


    })
}

