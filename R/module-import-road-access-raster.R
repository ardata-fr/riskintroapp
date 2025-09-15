importRoadAccessUI <- function(id) {
  ns <- NS(id)
  modalDialog(
    title = titleWithHelpKey("import-road-access-title"),
    fluidRow(column(
      width = 10, offset = 1,
      tags$p("Import raster file (.tif, .tiff, .grd, .asc) containing risk values."),
      tags$p("Risk values will be extracted for each epidemiological unit using zonal statistics."),

      fileInput(
        inputId = ns("file"),
        label = NULL,
        multiple = FALSE,
        accept = c(".tif", ".tiff", ".grd", ".asc"),
        width = "100%"
      ),
      uiOutput(ns("error")),

      bslib::card(
        full_screen = FALSE,
        bslib::card_header(inlineComponents(
          "View raster plot ",
          awesomeCheckbox(
            inputId = ns("show_plot"),
            label = "(can be slow)", value = FALSE
          )
        )),
        bslib::card_body(
          class = "p-0",
          plotOutput(ns("map"), height = "400px")
        )
      )

    )),
    footer = list(
      uiOutput(ns("config_is_valid")),
      actionButton(
        inputId = ns("apply"),
        class = "btn-primary",
        label = "Import",
        disabled = TRUE
      ),
      modalButton(label = "Cancel")
    ),
    size = "xl", easyClose = TRUE
  )
}

importRoadAccessServer <- function(id) {

  safely_rast <- safely(terra::rast)

  moduleServer(
    id,
    function(input, output, session) {

      # Error ----
      importError <- reactiveVal(NULL)
      output$error <- renderUI({
        req(importError())
        alert_error(
          text = "Error while importing:",
          error = importError()
        )
      })

      # importData ----
      importData <- reactiveVal(NULL)

      # file ----
      observeEvent(input$file, {
        fp <- req(input$file$datapath)
        importData(NULL)

        raster <- safely_rast(fp)
        if(is_error(raster$error)) {
          importError(raster$error)
          return()
        }

        importError(NULL)
        importData(raster$result)
      })

      # configIsValid ----
      configIsValid <- reactive(label = paste0("configIsValid-import-", id), {

        if (!isTruthy(importData())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Import from file or download to continue."
          )
          return(status)
        }

        if (isTruthy(importError())) {
          status <- build_config_status(
            value = FALSE,
            msg = "There was an error while importing."
          )
          return(status)
        }

        build_config_status(
          value = TRUE,
          msg = "Configuration is valid."
        )
      })
      output$config_is_valid <- renderUI({
        report_config_status(configIsValid())
      })

      observe({
        if (isTruthy(configIsValid())) {
          shinyjs::enable(id = "apply")
        } else {
          shinyjs::disable(id = "apply")
        }
      })

      # footer ----
      returnData <- reactiveVal(NULL)
      observeEvent(input$apply, {
        returnData(importData())
        importData(NULL)
        removeModal()
      })

      observeEvent(input$cancel, {
        importData(NULL)
        removeModal()
      })

      # map ----
      output$map <- renderPlot({
        req(input$show_plot)
        req(importData())
        terra::plot(importData())
      })

      returnData
    }
  )
}
