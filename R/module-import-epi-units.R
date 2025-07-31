#' @export
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
    uiOutput(outputId = ns("table_preview")),
    uiOutput(outputId = ns("column_mapping")),
    uiOutput(outputId = ns("validation_message"))
  )
}

#' @export
#' @importFrom riskintrodata read_geo_file
#' @importFrom purrr safely
importEpiUnitsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    resultTable <- reactiveVal(NULL)

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
      resultTable(polygon)
    })

    output$table_preview <- renderUI({
      req(resultTable(), !isTruthy(resultTable()$error))
      DT::renderDataTable(DT::datatable(resultTable()$result, options = list(pageLength = 5)))
    })



  })
}

