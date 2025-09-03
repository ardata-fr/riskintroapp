#' @export
exportUI <- function(id = "export_module") {
  ns <- NS(id)
  actionButton(
    inputId = ns("open_export"),
    label = "Export",
    icon = icon("file-export")
  )
}

#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shiny
#'  moduleServer reactiveVal reactive req isTruthy observeEvent renderUI
#'  selectInput downloadHandler removeModal downloadButton icon tags tagList
#' @importFrom readr write_csv
#' @importFrom ggplot2 ggsave
#' @importFrom utils zip
#' @importFrom tools file_path_sans_ext
#' @importFrom sf st_drop_geometry
#' @importFrom shinyWidgets updateAwesomeCheckboxGroup
#' @export
exportServer <- function(id = "export_module", files) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      file_list <- reactive({
        nullify(files())
      })

      file_types <- reactive({
        objs <- req(file_list())
        types <- lapply(file_list(), function(x){
          if (inherits(x, "sf")) {
            "geospatial"
          } else if (inherits(x, c("data.frame"))) {
            "dataset"
          } else if (inherits(x, "splatRaster")) {
            "raster"
          } else if (inherits(x, "ggplot")) {
            "plot"
          } else {
            warning("Export file type no supported!")
            "unknown"
          }
        })
        unlist(types)
      })

      # Modal ----
      observeEvent(input$open_export, {
        req(file_list())
        showModal(modalDialog(
          title = "Export",
          # CSS for styling
          tags$head(
            tags$style(HTML("
      .checkbox-select-wrapper {
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        background-color: #f9f9f9;
      }

      /* Inline layout styling */
      .inline-container {
        display: flex;
        align-items: flex-start;
        gap: 20px;
        flex-wrap: wrap;
      }

      .checkbox-section {
        flex: 1;
        min-width: 200px;
      }

      .select-section {
        flex: 0 0 220px;
        min-width: 220px;
      }

      .checkbox-section .awesome-checkbox {
        margin-bottom: 0;
      }

      /* Responsive behavior */
      @media (max-width: 600px) {
        .inline-container {
          flex-direction: column;
        }
        .select-section {
          flex: 1;
        }
      }

      .dataset-section {
        margin-bottom: 20px;
      }
      .section-header {
        background-color: #337ab7;
        color: white;
        padding: 8px 15px;
        border-radius: 4px;
        margin-bottom: 10px;
        font-weight: bold;
      }
    "))
          ),
          fluidRow(
            column(
              width = 10, offset = 1,
              uiOutput(ns("dataset_selection_ui"))
            )
          ),
          footer = list(
            downloadButton(
              outputId = ns("download"),
              label = "Export",
              icon = icon("file-export"),
              class = "btn-primary"
            ),
            modalButton(label = "Close")
          ),
          size = "l",
          easyClose = TRUE
        ))
      })

      # Generate dynamic UI
      output$dataset_selection_ui <- renderUI({
        datasets_info <- file_types()
        datasets_info <- unlist(datasets_info)

        if(length(datasets_info) == 0) {
          return(p("No files available"))
        }
        ui_elements <- list()
        for(i in seq_along(datasets_info)) {
          dataset_name <- names(datasets_info[i])
          dataset_type <- datasets_info[i]
          format_choices <- switch(
            dataset_type,
            "geospatial" = c("GeoPackage (.gpkg)" = "gpkg", "Shapefile (.shp)" = "shp"),
            "dataset" = c("Parquet (.parquet)" = "parquet", "CSV (.csv)" = "csv"),
            "raster"= c("tiff"),
            "plot" = c("PNG", "JPEG", "SVG")
          )

          checkbox_id <- ns(paste0("enabled_", i))
          format_id <- ns(paste0("format_", i))

          ui_elements[[i]] <- div(
            class = "checkbox-select-wrapper inline-layout",
            div(
              class = "inline-container",
              div(
                class = "checkbox-section",
                awesomeCheckbox(
                  inputId = checkbox_id,
                  label = dataset_name,
                  value = TRUE,
                  status = "primary"
                )
              ),
              div(
                class = "select-section",
                conditionalPanel(
                  condition = paste0("input['", checkbox_id, "'] == true"),
                  selectInput(
                    inputId = format_id,
                    label = NULL,
                    choices = format_choices,
                    selected = if(!is.null(format_choices)) format_choices[1] else NULL,
                    width = "200px"
                  )
                )
              )
            )
          )
        }
        return(tagList(ui_elements))
      })

      export_error <- reactiveVal(NULL)

      output$export_error_ui <- renderUI({
        req(export_error())
        alert_error(
          text = "Error: unable to export files",
          error = export_error()
        )
      })

      configIsValid <- reactive({TRUE})

      observe({
        if (configIsValid()) {
          shinyjs::enable("download")
        } else {
          shinyjs::disable("download")
        }
      })

      selections <- reactive({
        datasets_info <- req(file_types())
        enabled_selections <- list()
        for(i in seq_along(datasets_info)) {
          checkbox_id <- paste0("enabled_", i)
          format_id <- paste0("format_", i)

          if(isTruthy(input[[checkbox_id]])) {
            enabled_selections[[names(datasets_info[i])]] <- list(
              type = unname(datasets_info[i]),
              format = input[[format_id]]
            )
          }
        }
        enabled_selections
      })

      # Download handler
      output$download <- downloadHandler(
        filename = function() {
          enabled_selections <- selections()
          if (length(enabled_selections) == 1) {
            filename <- names(enabled_selections)[1]
            format <- enabled_selections[[1]]$format
            return(paste0(tools::file_path_sans_ext(filename), ".", format))
          } else {
            return(paste0("export_", Sys.Date(), ".zip"))
          }
        },

        content = function(file) {
          safely_export <- purrr::safely(export_helper)
          res <- safely_export(selections(), file_list(), file)
          if (isTruthy(res$error)) {
            export_error(res$error)
          } else {
            removeModal()
            export_error(NULL)
          }
        }
      )
      return(reactive(export_error()))
    }
  )
}

#' @keywords internal
export_helper <- function(enabled_selections, file_list, file) {
  if (length(enabled_selections) == 1) {
    filename <- names(enabled_selections)[1]
    obj <- file_list[[filename]]
    format <- enabled_selections[[1]]$format
    write_file_helper(obj, file, format)
  } else {
    browser()
    temp_dir <- tempfile(pattern = "ri-export-")
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    for (filename in names(enabled_selections)) {
      obj <- file_list[[filename]]
      format <- enabled_selections[[filename]]$format
      temp_file <- file.path(temp_dir, paste0(tools::file_path_sans_ext(filename), ".", format))
      write_file_helper(obj, temp_file, format)
    }
    workspace::pack_folder(temp_dir, file)
  }
}

#' @importFrom sf write_sf
#' @importFrom readr write_csv write_csv2 write_delim write_tsv
#' @importFrom arrow write_parquet
#' @importFrom terra writeRaster
#' @keywords internal
write_file_helper <- function(x, file, format) {
  if (format %in% c("gpkg", "shp")) {
    sf::write_sf(x, file)
  } else if (format == "csv") {
    readr::write_csv(x, file)
  } else if (format == "csv2") {
    readr::write_csv2(x, file)
  } else if (format == "txt") {
    readr::write_delim(x, file)
  } else if (format == "tsv") {
    readr::write_tsv(x, file)
  } else if (format == "parquet") {
    arrow::write_parquet(x, file)
  } else if (format == "raster") {
    terra::writeRaster(x, file)
  }
  file
}
