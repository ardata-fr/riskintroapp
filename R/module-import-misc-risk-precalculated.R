
#' @importFrom shinyWidgets numericRangeInput
importMiscRiskPrecalculatedUI <- function(id) {
  ns <- NS(id)

  modalDialog(
    title = "Import precalculated risk file",
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
      importData <- reactive({
        fp <- req(input$file$datapath)
        safely_read_delim <- safely(readr::read_delim)
        dataset <- safely_read_delim(fp, show_col_types = FALSE)
        if(is_error(dataset$error)) {
          read_error(dataset$error)
        } else {
          read_error(NULL)
          dataset$result
        }
      })

      # Update col selectors ----
      observe({
        req(importData())
        updateSelectInput(
          inputId = "join_by",
          choices = colnames(importData()),
          selected = character()
        )
        updateSelectInput(
          inputId = "risk_col",
          choices = colnames(importData()),
          selected = character()
        )
      })

      observeEvent(input$minmax_scale, {
        req(importData(), input$risk_col)
        vec <- req(importData()[[input$risk_col]])
        req(is.numeric(vec))
        updateNumericRangeInput(
          inputId = "scale",
          value = c(min(vec), max(vec))
          )
      })

      # configIsValid ----
      configIsValid <- reactive({
        config_is_valid(
          x = "import_precalcuated_risk",
          dataset = importData(),
          risk_table = epi_units(),
          metadata = riskMetaData(),
          parameters = list(
            join_by = input$join_by,
            risk_col = input$risk_col,
            scale = input$scale
          )
        )
      })

      output$config_ui <- renderUI({
        req(!is.null(configIsValid()))
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
        attr(dataset, "risk_col") <- input$risk_col

        new_precalc_risk <- list(
            name = input$risk_col,
            type = "precalculated",
            join_by = input$join_by,
            dataset = dataset,
            rescale_args = list()
          )
        returnList(new_precalc_risk)
      })

      returnList
    })
}

#' @export
config_is_valid.import_precalcuated_risk <- function(x, ...) {
  params <- list(...)
  dataset <- params$dataset
  risk_table <- params$risk_table
  metadata <- params$metadata
  parameters <- params$parameters

  # dataset ----
  if(!isTruthy(dataset)) {
    status <- build_config_status(
      value = FALSE,
      msg = "A dataset must be imported."
    )
    return(status)
  }

  # risk col ----
  if (parameters$risk_col %in% names(metadata)) {
    status <- build_config_status(
      value = FALSE,
      msg = "There is already a risk with this name."
    )
    return(status)
  }

  if(!isTruthy(parameters$risk_col)) {
    status <- build_config_status(
      value = FALSE,
      msg = "Select a risk column to import."
    )
    return(status)
  }

  if(!isTruthy(parameters$risk_col)) {
    status <- build_config_status(
      value = FALSE,
      msg = "A risk column already exists with that name."
    )
    return(status)
  }

  risk_data <- dataset[parameters$risk_col]
  numeric <- sapply(risk_data, is.numeric)
  notnumeric <- numeric[!numeric]
  if(length(notnumeric) > 0) {
    status <- build_config_status(
      value = FALSE,
      msg = "Selected risk column must contain numeric values."
    )
    return(status)
  }
  has_nas <- sapply(risk_data, \(x) any(is.na(x)))
  has_nas <- has_nas[has_nas]
  if(length(has_nas) > 0) {
    status <- build_config_status(
      value = FALSE,
      msg = paste("Risk columns (", quote_and_collapse(names(has_nas)), ") contain missing values.")
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
  id_not_found <- dataset[[parameters$join_by]][!dataset[[parameters$join_by]] %in% risk_table$eu_id]

  if (length(id_not_found) == length(dataset[[parameters$join_by]])) {
    status <- build_config_status(
      value = FALSE,
      msg = "None of the values for the identifier column match the epidemiological units EU_ID column. The datasets cannot be joined."
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




