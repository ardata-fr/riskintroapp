
editEntryPointsUI <- function(id, config) {
  ns <- NS(id)

  modalDialog(
    title = if (is.null(config$name)){
      "Create new entry point"
    } else{
      paste("Edit", config$name)
    },
    div(
      tags$label(
        class = "control-label",
        `for` = ns("name"),
        "Provide a point name"
      ),
      helpPopup(get_help("point_name"))
    ),
    textInput(
      inputId = ns("name"),
      label = NULL,
      value = config$point_name
    ),
    div(
      tags$label(
        class = "control-label",
        `for` = ns("type"),
        "Select transport type"
      ),
      helpPopup(get_help("type"))
    ),
    selectInput(
      ns("type"),
      label = NULL,
      selected = config$type,
      choices = list(
       "Airport (AIR)" = "AIR",
       "Seaport (SEA)" = "SEA",
       "Border crossing (BC)" = "BC",
       "Contraband crossing (CC)" = "CC",
       "Transhumance crossing" = "TC"
       )
    ),
    div(
      tags$label(
        class = "control-label",
        `for` = ns("mode"),
        "Select mode"
      ),
      helpPopup(get_help("mode"))
    ),
    selectInput(
      ns("mode"),
      label = NULL,
      selected = config$mode,
      choices = list(
        "Controlled" = "C",
        "Not controlled" = "NC"
        )
    ),
    div(
      tags$label(
        class = "control-label",
        `for` = ns("sources"),
        "Select emission risk sources"
      ),
      helpPopup(get_help("sources"))
    ),
    selectInput(
      inputId = ns("sources"),
      label = NULL,
      choices = config$source_choices,
      multiple = TRUE,
      selected = config$sources
    ),

    # Modal settings
    size = "m",
    easyClose = TRUE,
    footer = list(
      uiOutput(ns("config_is_valid")),
      actionButton(ns("apply"), class = "btn-primary", "Apply", disabled = TRUE),
      actionButton(ns("cancel"), class = "btn-light", "Cancel"),
      actionButton(ns("delete"), class = "btn-danger", "Delete", disabled = TRUE)
    )
  )
}

editEntryPointsServer <- function(id, map_click) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      configIsValid <- reactive({
        if (!isTruthy(input$name)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Point name is missing"
          )
          return(status)
        }
        if (!isTruthy(input$mode)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Mode is missing"
          )
          return(status)
        }
        if (!isTruthy(input$type)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Type is missing"
          )
          return(status)
        }
        if (!isTruthy(input$sources)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Sources are missing"
          )
          return(status)
        }
        build_config_status(
          value = TRUE,
          msg = "Configuration is valid."
        )
      })
      output$config_is_valid <- renderUI({
        report_config_status(configIsValid(), in_panel = FALSE)
      })

      observe({
        if (isTruthy(configIsValid())) {
          shinyjs::enable("apply")
        } else {
          shinyjs::disable("apply")
        }
      })

      observe({
        if (isTruthy(configIsValid())) {
          shinyjs::enable("delete")
        } else {
          shinyjs::disable("delete")
        }
      })

      update_row <- reactive({
        req(configIsValid())
        clicky <- map_click()
        data.frame(
          point_id = clicky$id,
          point_name = input$name,
          mode = input$mode,
          sources = input$sources,
          type = input$type,
          lat = clicky$lat,
          lng = clicky$lng
        ) |>
          riskintrodata:::latlng_to_sf()
      })

      returnList <- reactiveVal(NULL)
      observeEvent(input$apply, {
        removeModal()
        returnList(list(
          row = update_row(),
          operation = map_click()$operation
        ))
      })

      observeEvent(input$delete, {
        removeModal()
        returnList(list(
          row = update_row(),
          operation = "delete"
        ))
      })

      observeEvent(input$cancel, {
        removeModal()
      })

      returnList
    })
}
