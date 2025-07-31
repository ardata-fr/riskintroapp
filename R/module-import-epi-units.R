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
    navset_card_underline(
      nav_panel(
        title = "Map",
        leafletOutput(outputId = ns("map_preview"))
      ),
      nav_panel(
        title = "Table",
        reactableOutput(outputId = ns("table_preview"))
      )
    )
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

    output$map_preview <- renderLeaflet({
      req(resultTable(), !isTruthy(resultTable()$error))
      leaflet(resultTable()$result) |> addPolygons() |> addTiles()
    })

    output$table_preview <- renderReactable({
      req(resultTable(), !isTruthy(resultTable()$error))
      reactable(resultTable()$result)
    })



  })
}

