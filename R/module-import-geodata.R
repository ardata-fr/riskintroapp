
#' @importFrom geodata country_codes
geodataUI <- function(id) {
  ns <- NS(id)

  cc <- country_codes()
  cc_choices <- cc$ISO3
  names(cc_choices) <- paste(cc$ISO3, '-', cc$NAME)

  modalDialog(
    title = titleWithHelpKey("import-geodata-title"),
    size = "xl",
    easyClose = TRUE,
    HTML(get_help("import-geodata-body")),
    panel(
      inlineComponents(
      valign = "top",
      selectizeInput(
        inputId = ns("cc"),
        label = "Select country",
        choices = cc_choices,
        multiple = FALSE
      ),
      awesomeRadio(
        inputId = ns("level"),
        label = "Level of administrative subdivision",
        choices = list(
          "Country level" = 0,
          "1st level" = 1,
          "2nd level" = 2,
          "3rd level" = 3
        ),
        selected = 1
      ),
      bslib::input_task_button(
        id = ns("dnld"),
        label = "Download",
        icon = icon("download"),
        label_busy = "Downloading..."
      )
    )),

    uiOutput(ns("is_overwriting")),
    uiOutput(ns("download_error")),
    uiOutput(ns("validation_error")),
    tags$br(),
    conditionalPanel(
      condition = "output.show_preview",
      ns = ns,

      navset_card_tab(
        id = ns("panel_ui"),
        nav_panel(
          title = "Map view",
          leafletOutput(outputId = ns("map"))
        ),
        nav_panel(
          title = "Table view",
          reactableOutput(outputId = ns("table"))
        )
      )
    ),

    footer = list(
      uiOutput(ns("config_is_valid")),
      actionButton(ns("apply"), "Import", class = "btn-primary", disabled = TRUE),
      modalButton("Cancel")
    )
  )
}

#' @importFrom geodata gadm
#' @importFrom sf st_as_sf
geodataServer <- function(id, is_overwriting) {
  moduleServer(
    id,
    function(input, output, session) {

      # ExtendedTask ----
      call_geodata <- ExtendedTask$new(
        function(country, level) {
          mirai::mirai(.expr = {
            x <- riskintroapp::safe_and_quiet(
              .fun = geodata::gadm,
              country = country,
              level = level,
              path = tempfile(pattern = "geodata-"),
              resolution = 2 #low res
            )
            if (!is.null(x$result)){
              x$result <- sf::st_as_sf(x$result)
            }
            x$level <- level
            x$country <- country
            x
          },
          .args = list(
            country = country,
            level = level
          )
          )
        }
      ) |> bind_task_button("dnld")

      # Button click starts process
      observeEvent(input$dnld, {
        call_geodata$invoke(
          country = input$cc,
          level = input$level
        )

      })

      # is_overwriting -----
      output$is_overwriting <- renderUI({
        req(is_overwriting())
        alert(status = "warning", "Importing a new dataset will overwrite the existing one.")
        })

      # download result ----
      downloadError <- reactiveVal(NULL)
      output$download_error <- renderUI(report_config_status(downloadError()))
      download_result <- reactive({
        res <- call_geodata$result()
        if (is_error(res$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while downloading:",
            error = res$error
          )
          downloadError(status)
          return(NULL)
        }
        # geodata returning nothing because no file exists results in a message
        if (is.null(res$result) && is.null(res$error) && !is.null(res$messages)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while downloading:",
            error = paste(res$messages, collapse = ". ")
          )
          downloadError(status)
          return(NULL)
        }
        show_toast(
          "Epidemiological units have finished downloading.",
          type = "info",
          timer = 5000
        )
        downloadError(NULL)
        res
      })

      # validation -----
      validationError <- reactiveVal(NULL)
      output$validation_error <- renderUI(report_config_status(validationError()))
      valid_dataset <- reactive({
        req(download_result())
        res <- download_result()
        dataset <- res$result
        level <- res$level

        if (level == 0) {
          eu_id_col <- "GID_0"
          eu_name_col <- "COUNTRY"
        } else {
          eu_id_col <- paste0("GID_", level)
          eu_name_col <- paste0("NAME_", level)
        }

        safe_val <- safely(validate_dataset)
        validation <- safe_val(
          dataset, "epi_units",
          eu_id = eu_id_col,
          eu_name = eu_name_col
        )

        if (is_error(validation$error)){
          status <- build_config_status(
            value = FALSE,
            msg = "Error while downloading:",
            error = validation$error
          )
          validationError(status)
          return(NULL)
        }
        validationError(NULL)
        extract_dataset(validation$result)
      })


      # map ----
      output$map <- renderLeaflet({
        req(valid_dataset())
        preview_map(valid_dataset())
      })
      # table ----
      output$table <- renderReactable({
        req(valid_dataset())
        reactable(valid_dataset())
      })

      # configIsValid -----
      configIsValid <- reactive({

        if (!isTruthy(download_result())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Download a administrative boundaries to continue."
          )
          return(status)
        }
        if (isTruthy(download_result()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error occured while downloading."
          )
          return(status)
        }
        if (!isTruthy(valid_dataset())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Data has not been validated."
          )
          return(status)
        }
        build_config_status(
          value = TRUE,
          msg = "Configuration is valid."
        )
      })
      output$config_is_valid <- renderUI(report_config_status(configIsValid()))
      output$show_preview <- reactive(isTruthy(valid_dataset()))
      outputOptions(output, "show_preview", suspendWhenHidden = FALSE)

      observe({
        if (isTruthy(configIsValid())) {
          shinyjs::enable("apply")
        } else {
          shinyjs::disable("apply")
        }
      })

      # returnData -----
      returnData <- reactiveVal(NULL)
      observeEvent(input$apply, {
        removeModal()
        returnData(valid_dataset())
      })
      return(returnData)
    })
}
