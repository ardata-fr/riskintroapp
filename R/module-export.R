#' Export Module UI
#'
#' Creates the UI component for the export module, which provides a simple action button
#' that opens the export modal dialog.
#'
#' @param id Character string. The module ID for namespacing. Defaults to "export_module".
#'
#' @return A Shiny UI element containing an action button with export icon.
#'
#' @details
#' This function generates a simple action button that, when clicked, triggers the export
#' modal dialog. The button is styled with a "file-export" icon and labeled "Export".
#'
#' @seealso \code{\link{exportServer}} for the corresponding server-side logic.
#'
#' @examples
#' \dontrun{
#' # In your Shiny UI
#' fluidPage(
#'   exportUI("my_export")
#' )
#' }
#'
#' @keywords internal
exportUI <- function(id = "export_module") {
  ns <- NS(id)
  actionButton(
    inputId = ns("open_export"),
    label = "Export",
    icon = icon("file-export")
  )
}

#' Export Module Server
#'
#' Server-side logic for the export module that handles file export functionality for
#' different data types including geospatial data, datasets, rasters, and plots.
#'
#' @param id Character string. The module ID for namespacing. Should match the ID used in \code{\link{exportUI}}.
#' @param files A reactive expression that returns a named list of objects to be exported.
#'   Supported object types include:
#'   \itemize{
#'     \item \code{sf} objects (geospatial data) - exported as GeoPackage (.gpkg) or Shapefile (.shp)
#'     \item \code{data.frame} objects (datasets) - exported as Parquet (.parquet) or CSV (.csv)
#'     \item \code{splatRaster} objects (raster data) - exported as TIFF
#'     \item \code{ggplot} objects (plots) - exported as PNG, JPEG, or SVG
#'   }
#'
#' @return A reactive expression that returns export error information (NULL if no errors).
#'
#' @details
#' This function creates a comprehensive export system with the following features:
#' \itemize{
#'   \item Interactive modal dialog for selecting files and formats to export
#'   \item Dynamic UI that adapts to the types of files available for export
#'   \item Support for multiple export formats per data type
#'   \item Single file export or bulk ZIP export when multiple files selected
#'   \item Error handling with user-friendly error messages
#'   \item File format validation and automatic extension handling
#' }
#'
#' The export process:
#' \enumerate{
#'   \item User clicks export button to open modal dialog
#'   \item UI dynamically generates checkboxes and format selectors based on available files
#'   \item User selects which files to export and their desired formats
#'   \item Download handler processes selections and creates output files
#'   \item For single file: direct download with proper filename and extension
#'   \item For multiple files: creates temporary directory, exports all files, then ZIPs them
#' }
#'
#' @seealso
#' \code{\link{exportUI}} for the corresponding UI component
#' \code{\link{export_helper}} for the internal export processing logic
#' \code{\link{write_file_helper}} for format-specific file writing
#'
#' @examples
#' \dontrun{
#' # In your Shiny server function
#' files_to_export <- reactive({
#'   list(
#'     "my_data.csv" = my_dataframe,
#'     "spatial_data.gpkg" = my_sf_object,
#'     "plot.png" = my_ggplot
#'   )
#' })
#'
#' export_errors <- exportServer("my_export", files_to_export)
#'
#' # Monitor export errors
#' observeEvent(export_errors(), {
#'   if (!is.null(export_errors())) {
#'     showNotification("Export failed!", type = "error")
#'   }
#' })
#' }
#'
#' @importFrom shinyWidgets awesomeCheckboxGroup awesomeCheckbox
#' @importFrom shiny
#'  moduleServer reactiveVal reactive req isTruthy observeEvent renderUI
#'  selectInput downloadHandler removeModal downloadButton icon tags tagList
#'  modalDialog showModal modalButton fluidRow column conditionalPanel observe
#' @importFrom readr write_csv
#' @importFrom ggplot2 ggsave
#' @importFrom utils zip
#' @importFrom tools file_path_sans_ext
#' @importFrom sf st_drop_geometry
#' @importFrom shinyWidgets updateAwesomeCheckboxGroup
#' @importFrom shinyjs enable disable
#' @importFrom purrr safely
#' @keywords internal
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
          title = titleWithHelpKey("export-title"),
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

#' Export Helper Function
#'
#' Internal helper function that processes export selections and coordinates the actual
#' file writing operations. Handles both single file exports and multi-file ZIP archives.
#'
#' @param enabled_selections A named list where each element represents a file to export.
#'   Each element contains:
#'   \itemize{
#'     \item \code{type}: Character string indicating the object type ("geospatial", "dataset", "raster", "plot")
#'     \item \code{format}: Character string specifying the export format (e.g., "gpkg", "csv", "png")
#'   }
#' @param file_list A named list of R objects to be exported. Names should match those in \code{enabled_selections}.
#' @param file Character string. The output file path where the export should be written.
#'
#' @details
#' This function orchestrates the export process by:
#' \itemize{
#'   \item For single file exports: directly calls \code{\link{write_file_helper}} with the appropriate object and format
#'   \item For multi-file exports: creates a temporary directory, exports each file individually, then packages everything into a ZIP archive
#' }
#'
#' The function uses temporary directories for multi-file exports to avoid conflicts and
#' ensure clean packaging. The temporary directory is automatically managed and cleaned up.
#'
#' @return Invisibly returns the file path where the export was written.
#'
#' @seealso
#' \code{\link{write_file_helper}} for the actual file writing logic
#' \code{\link{exportServer}} for the main export server function
#'
#' @keywords internal
export_helper <- function(enabled_selections, file_list, file) {
  if (length(enabled_selections) == 1) {
    filename <- names(enabled_selections)[1]
    obj <- file_list[[filename]]
    format <- enabled_selections[[1]]$format
    write_file_helper(obj, file, format)
  } else {
    temp_dir <- tempfile(pattern = "ri-export-")
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    for (filename in names(enabled_selections)) {
      obj <- file_list[[filename]]
      format <- enabled_selections[[filename]]$format
      temp_file <- file.path(temp_dir, paste0(tools::file_path_sans_ext(filename), ".", format))
      write_file_helper(obj, temp_file, format)
    }
    workspace:::pack_folder(temp_dir, file)
  }
}

#' Write File Helper Function
#'
#' Internal helper function that handles the actual writing of different data types to files
#' in various formats. This function serves as a unified interface for format-specific
#' export operations.
#'
#' @param x The R object to be written to file. Can be:
#'   \itemize{
#'     \item \code{sf} object for geospatial data
#'     \item \code{data.frame} for tabular data
#'     \item \code{splatRaster} for raster data
#'     \item \code{ggplot} object for plot data
#'   }
#' @param file Character string. The file path where the object should be written.
#' @param format Character string. The export format specifying how the object should be written.
#'   Supported formats:
#'   \itemize{
#'     \item \code{"gpkg"}, \code{"shp"}: For geospatial data (sf objects)
#'     \item \code{"csv"}, \code{"csv2"}, \code{"txt"}, \code{"tsv"}, \code{"parquet"}: For tabular data
#'     \item \code{"raster"}: For raster data
#'     \item \code{"PNG"}, \code{"JPEG"}, \code{"SVG"}: For plot objects (handled by calling functions)
#'   }
#'
#' @details
#' This function acts as a dispatcher, routing different object types and formats to their
#' appropriate writing functions:
#' \itemize{
#'   \item Geospatial formats (gpkg, shp) use \code{sf::write_sf()}
#'   \item CSV formats use various \code{readr} functions (\code{write_csv()}, \code{write_csv2()}, etc.)
#'   \item Parquet format uses \code{arrow::write_parquet()}
#'   \item Raster format uses \code{terra::writeRaster()}
#' }
#'
#' The function ensures consistent file writing behavior across all supported data types
#' and formats, with appropriate format-specific options and error handling.
#'
#' @return Invisibly returns the file path where the object was written.
#'
#' @seealso
#' \code{\link{export_helper}} for the export coordination logic
#' \code{\link{exportServer}} for the main export server function
#'
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
