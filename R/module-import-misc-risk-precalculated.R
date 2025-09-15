
#' @importFrom shinyWidgets numericRangeInput
importMiscRiskPrecalculatedUI <- function(id) {
  ns <- NS(id)

  modalDialog(
    title = titleWithHelpKey("import-misc-risk-precalculated-title"),
    fluidRow(column(
      width = 10, offset = 1,
      tags$p("Import csv or txt file."),
      tags$p("This file should be joinable to epidemioligcal units file using a unique identifier."),
      fileInput(
        inputId = ns("file"),
        label = NULL,
        multiple = TRUE,
        accept = c(".csv", ".txt"),
        width = "100%"
      ),
      uiOutput(ns("read_error_ui")),
      inlineComponents(
        selectInput(
          inputId = ns("risk_col"),
          label = "Select risk score columns",
          choices = character(),
          multiple = FALSE
        ),
        textInput(
          inputId = ns("risk_name"),
          label = "Risk name",
          value = character()
        )
      ),
      inlineComponents(
        selectInput(
          inputId = ns("join_by"),
          label = "Select identifier column",
          choices = character(),
          multiple = FALSE
        ),
        downloadButton(
          outputId = ns("download"),
          label = "Download template",
          icon = icon("download")
        )
      ),
      inlineComponents(
        shinyWidgets::numericRangeInput(
          inputId = ns("scale"),
          label = 'Risk score scale',
          value = c(0, 0)
        ),
        actionButton(
          inputId = ns("minmax_scale"),
          label = NULL,
          icon = icon("arrows-left-right-to-line"),
          width = "75px"
        )
      ),
      bslib::card(
        full_screen = FALSE,
        bslib::card_header("Precalculated risk score"),
        bslib::card_body(
          class = "p-0",
          leafletOutput(ns("map"), height = "350px")
        )
      )
    )
    ),
    footer = list(
      uiOutput(ns("config_ui")),
      actionButton(
        inputId = ns("apply"),
        class = "btn-primary",
        label = "Import",
        disabled = TRUE
        ),
      modalButton(
        label = "Cancel"
      )
    ),
    size = "xl", easyClose = TRUE
  )
}

#' @importFrom readr read_delim write_csv
#' @importFrom shinyWidgets updateNumericRangeInput
importMiscRiskPrecalculatedServer <- function(id, riskMetaData, epi_units) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      # Read data -----
      read_error <- reactiveVal(NULL)
      output$read_error_ui <- renderUI({
        req(read_error())
        alert_error(
          text = "Error: unable to read file",
          error = read_error()
        )
      })
      importRaw <- reactive({
        fp <- req(input$file$datapath)
        safely_read_delim <- safely(readr::read_delim)
        dataset <- safely_read_delim(fp, show_col_types = FALSE)
        if(is_error(dataset$error)) {
          read_error(dataset$error)
        } else {
          read_error(NULL)
          out <- dataset$result
        }
      })

      # Update col selectors ----
      observe({
        req(importRaw())
        updateSelectInput(
          inputId = "join_by",
          choices = colnames(importRaw()),
          selected = character()
        )
        updateSelectInput(
          inputId = "risk_col",
          choices = colnames(importRaw()),
          selected = character()
        )
      })
      observeEvent(input$risk_col, ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateTextInput(
          inputId = "risk_name",
          value = input$risk_col,
        )
      })

      observeEvent(input$minmax_scale, {
        req(importRaw(), input$risk_col)
        vec <- req(importRaw()[[input$risk_col]])
        req(is.numeric(vec))
        updateNumericRangeInput(
          inputId = "scale",
          value = c(min(vec), max(vec))
          )
      })

      # importData ----
      importData <- reactive({
        dat <- req(importRaw())
        req(input$risk_col)
        req(input$scale[[1]] < input$scale[[2]]) # validate scale input

        # clamp risk values outside of scale
        dat[[input$risk_name]] <- terra::clamp(
          dat[[input$risk_col]] ,
          lower = input$scale[[1]],
          upper = input$scale[[2]],
          values = TRUE
        )
        # Also rename the risk col with the new name provided by user
        if (input$risk_col != input$risk_name){
          dat[[input$risk_col]] <- NULL
        }
        dat
      })



      # map ----
      output$map <- renderLeaflet({
        req(configIsValid())
        risk_data <- importData()

        eu <- epi_units()

        risk_data_sf <- dplyr::left_join(
          x = eu,
          y = risk_data,
          by = c("eu_id" = input$join_by)
        )

        risk_values <- risk_data_sf[[input$risk_name]]
        pal <- riskintroanalysis::riskPalette(scale = input$scale)
        basemap() |>
          addPolygons(
            data = risk_data_sf,
            fillColor = pal(risk_values),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            label = generate_leaflet_labels(
              risk_data_sf,
              title_field = "eu_name",
              exclude_fields = c("user_id")
            ),
            group = "risk"
          ) |>
          riskintroanalysis::addRiskLegend(
            scale = input$scale,
            title = "Aggregated Score"
          )
      })

      # configIsValid ----
      configIsValid <- reactive(label = paste0("configIsValid-import-", id),{
        config_is_valid(
          x = "import_precalcuated_risk",
          import_success = isTruthy(importRaw()),
          dataset = importData(),
          risk_table = epi_units(),
          metadata = riskMetaData(),
          parameters = list(
            join_by = input$join_by,
            risk_col = input$risk_col,
            scale = input$scale,
            risk_name = input$risk_name
          )
        )
      })
      output$config_ui <- renderUI({
        report_config_status(configIsValid())
      })
      observe({
        if(isTruthy(configIsValid())) {
          shinyjs::enable("apply")
        } else {
          shinyjs::disable("apply")
        }
      })

      # download template ----
      output$download <- downloadHandler(
        filename = function() {
          paste("precalculated-risk-template-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          x <- st_drop_geometry(epi_units())
          x <- x[, c("eu_id", "eu_name")]
          readr::write_csv(x, file)
        }
      )

      # return values ----
      returnList <- reactiveVal(NULL)
      observeEvent(input$apply, {
        removeModal()
        dataset <- importData()
        attr(dataset, "scale") <- input$scale
        attr(dataset, "join_by") <- input$join_by
        attr(dataset, "risk_col") <- input$risk_name

        # some values are repeated here, it is useful to include them in the
        # the metadata so that it can be repopulated when loading data from
        # a workspace
        new_precalc_risk <- list(
            name = input$risk_name,
            type = "precalculated",
            join_by = input$join_by,
            initial_scale = input$scale,
            risk_col = input$risk_name,
            dataset = dataset,
            rescale_args = list(
              cols = input$risk_name,
              from = input$scale,
              to = c(0, 100),
              method = "linear",
              inverse = FALSE
            )
          )
        returnList(new_precalc_risk)
      })

      returnList
    })
}

#' @export
config_is_valid.import_precalcuated_risk <- function(x, ...) {
  params <- list(...)

  import_success <- params$import_success
  dataset <- params$dataset
  risk_table <- params$risk_table
  metadata <- params$metadata
  parameters <- params$parameters

  if(!isTruthy(import_success)) {
    status <- build_config_status(
      value = FALSE,
      msg = "A dataset must be imported."
    )
    return(status)
  }
  # dataset ----
  if(!isTruthy(dataset)) {
    status <- build_config_status(
      value = FALSE,
      msg = "A dataset must be imported."
    )
    return(status)
  }

  # risk_name ----
  if(!isTruthy(parameters$risk_name)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Provide a risk name."
    )
    return(status)
  }

  if (!is_valid_name(parameters$risk_name)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Risk name contains special characters other than '_' and spaces."
    )
    return(status)
  }

  if (parameters$risk_name %in% names(metadata)) {
    status <- build_config_status(
      value = FALSE,
      msg = "There is already a risk with this name."
    )
    return(status)
  }

  if (parameters$risk_name %in% c('epi_units', "emission_risk_factors", "overall_risk")) {
    status <- build_config_status(
      value = FALSE,
      msg = "This risk name is reserved."
    )
    return(status)
  }


  # risk col ----
  if(!isTruthy(parameters$risk_col)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Select a risk column to import."
    )
    return(status)
  }

  risk_data <- dataset[[parameters$risk_name]]

  if(!is.numeric(risk_data)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Selected risk column must contain numeric values."
    )
    return(status)
  }

  if(any(is.na(risk_data))) {
    status <- build_config_status(
      value = FALSE,
      msg = paste("Risk column contains missing values.")
    )
    return(status)
  }

  # scale ----
  if (is.null(parameters$scale)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale not found."
    )
    return(status)
  }
  if (any(is.na(parameters$scale))) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale input must be numeric."
    )
    return(status)
  }

  if (length(parameters$scale) != 2) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale must have length 2."
    )
    return(status)
  }
  if (!all(is.numeric(parameters$scale))) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale must be numeric."
    )
    return(status)
  }

  if (parameters$scale[[1]] == parameters$scale[[2]]) {
    status <- build_config_status(
      value = FALSE,
      msg = "Scale values should not be equal."
    )
    return(status)
  }

  if (parameters$scale[[1]] > parameters$scale[[2]]) {
    status <- build_config_status(
      value = FALSE,
      msg = "First scale value should be smaller than the second."
    )
    return(status)
  }

  # join by ----
  if(!isTruthy(parameters$join_by)) {
    status <- build_config_status(
      value = FALSE,
      msg = 'Select the identifier column to use to join with epidemiological units table.'
    )
    return(status)
  }

  if(parameters$join_by == parameters$risk_col) {
    status <- build_config_status(
      value = FALSE,
      msg = 'The join by column and the risk column cannot be the same.'
    )
    return(status)
  }

  id_not_found <- dataset[[parameters$join_by]][!dataset[[parameters$join_by]] %in% risk_table$eu_id]

  if (length(id_not_found) == length(dataset[[parameters$join_by]])) {
    status <- build_config_status(
      value = FALSE,
      msg = "No matching rows found between epidemiological units and imported dataset."
    )
    return(status)
  } else if (length(id_not_found) > 0) {
    status <- build_config_status(
      value = FALSE,
      msg = paste(
        "There are values for the identifier column that do not match",
        "any epidemiological units EU_ID values. The following cannot be joined: ",
        quote_and_collapse(id_not_found, max_out = 6))
    )
    return(status)
  }

  build_config_status(
    value = TRUE,
    msg = "Configuration is valid."
  )
}




