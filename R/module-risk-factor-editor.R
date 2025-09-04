
#' @importFrom riskintrodata iso3_to_name
riskFactorEditorUI <- function(id, country_id) {
  ns <- NS(id)

  modalDialog(
    title = "Edit emission risk factors",
    size = "xl",
    easyClose = TRUE,

    fluidRow(
      tags$h4(sprintf("Editing: %s (%s)", country_id, riskintrodata::iso3_to_name(country_id)))
    ),
    fluidRow(
      # Left column - Surveillance & Control
      shinyjs::hidden(textInput(
        inputId = ns("country_id"),
        label = NULL,
        value = country_id
        )),
      column(
        width = 5, offset = 1,

        # Surveillance measures
        tags$h4("Surveillance Measures"),
        shinyWidgets::prettySwitch(
          inputId = ns("disease_notification"),
          label = "Disease notification system",
          status = "success",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("targeted_surveillance"),
          label = "Targeted surveillance",
          status = "success",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("general_surveillance"),
          label = "General surveillance",
          status = "success",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("screening"),
          label = "Screening programs",
          status = "success",
          fill = TRUE
        ),

        tags$br(),

        # Control measures
        tags$h4("Control Measures"),
        shinyWidgets::prettySwitch(
          inputId = ns("precautions_at_the_borders"),
          label = "Border precautions",
          status = "success",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("slaughter"),
          label = "Emergency slaughter",
          status = "success",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("selective_killing_and_disposal"),
          label = "Selective culling & disposal",
          status = "success",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("zoning"),
          label = "Movement zoning",
          status = "success",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("official_vaccination"),
          label = "Official vaccination programs",
          status = "success",
          fill = TRUE
        )
      ),

      # Right column - Commerce & Status
      column(
        width = 5, offset = 1,

        # Commerce
        tags$h4("Animal Commerce"),
        shinyWidgets::prettySwitch(
          inputId = ns("commerce_legal"),
          label = "Legal trade present",
          status = "warning",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("commerce_illegal"),
          label = "Illegal trade present",
          status = "danger",
          fill = TRUE
        ),

        tags$br(),

        # Outbreak status
        tags$h4("Epidemiological Status"),
        shinyWidgets::radioGroupButtons(
          inputId = ns("outbreak_status"),
          label = "Outbreak status:",
          choices = list(
            "No outbreaks" = "none",
            "Past outbreak" = "past",
            "Current outbreak" = "current"
          ),
          selected = "none",
          status = "primary",
          direction = "vertical"
        ),

        conditionalPanel(
          condition = paste0("input['", ns("outbreak_status"), "'] == 'past'"),
          dateInput(
            inputId = ns("outbreak_date"),
            label = "End date of last outbreak:",
            value = NULL
          )
        )
      )
    ),

    footer = list(
      actionButton(ns("apply"), "Apply", class = "btn-success"),
      actionButton(ns("cancel"), "Cancel", class = "btn-secondary"),
      actionButton(ns("delete"), "Delete", class = "btn-danger")
    )
  )
}

#' @importFrom shinyWidgets updatePrettySwitch updateRadioGroupButtons
#' @importFrom shiny updateDateInput moduleServer req reactive observe
#' @importFrom dplyr filter if_else
#' @importFrom riskintrodata iso3_to_name
riskFactorEditorServer <- function(id, emission_risk_factors) {

  # Risk factor column definitions
  surveillance_factors <- c("disease_notification", "targeted_surveillance",
                            "general_surveillance", "screening")
  control_factors <- c("precautions_at_the_borders", "slaughter",
                       "selective_killing_and_disposal", "zoning",
                       "official_vaccination")
  commerce_factors <- c("commerce_illegal", "commerce_legal")
  all_risk_factors <- c(surveillance_factors, control_factors, commerce_factors)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check existing ---------
    # Get the row data for the selected country
    edit_row <- reactive({
      req(input$country_id, emission_risk_factors())

      current_data <- emission_risk_factors()
      target_country <- input$country_id

      # Find existing row for this country
      existing_row <- current_data |>
        filter(.data[["iso3"]] == target_country)

      if (nrow(existing_row) > 0) {
        existing_row[1, ] # Take first row if multiple, although it should never happen
      } else {
        # Return template_row if no existing data - will replace with values
        new_row <- emission_risk_factors()[1,]
        new_row[
          ,
          !colnames(new_row) %in% c("disease", "animal_category", "species")
        ] <- NA
        new_row$iso3 <- target_country
        new_row$country <- riskintrodata::iso3_to_name(target_country)
        new_row
      }
    })

    # Updates ---------
    # Populate inputs when modal opens (after UI is rendered)
    observe({
      country_data <- edit_row()

      # Convert data values (0/1/NA) to UI logic (TRUE/FALSE)
      # Data: 0 = measure in place (good), 1 = no measure (risk), NA = unknown (risk)

      # Surveillance & Control factors: UI shows "measure in place"
      for (factor in c(surveillance_factors, control_factors)) {
        current_value <- country_data[[factor]]
        # TRUE = measure in place (data value 0), FALSE = no measure (data value 1 or NA)
        ui_value <- if_else(is.na(current_value), FALSE, current_value == 0L)
        updatePrettySwitch(session, factor, value = ui_value)
      }

      # Commerce factors: UI shows "trade present"
      for (factor in commerce_factors) {
        current_value <- country_data[[factor]]
        # TRUE = trade present (data value 1), FALSE = no trade (data value 0 or NA)
        ui_value <- if_else(is.na(current_value), FALSE, current_value == 1L)
        updatePrettySwitch(session, factor, value = ui_value)
      }

      # Handle outbreak status
      outbreak_date <- country_data$last_outbreak_end_date
      if (!is.na(outbreak_date)) {
        if (outbreak_date == as.Date("1900-01-01")) {
          # No outbreaks
          updateRadioGroupButtons(session, "outbreak_status", selected = "none")
        } else if (outbreak_date == as.Date("2999-01-01")) {
          # Current outbreak
          updateRadioGroupButtons(session, "outbreak_status", selected = "current")
        } else {
          # Past outbreak with specific date
          updateRadioGroupButtons(session, "outbreak_status", selected = "past")
          updateDateInput(session, "outbreak_date", value = outbreak_date)
        }
      } else {
        # NA date - default to no outbreaks
        updateRadioGroupButtons(session, "outbreak_status", selected = "none")
      }

    })

    # Gather inputs -----------
    # Reactive value to store the updated row data
    returnList <- reactiveVal(NULL)

    # Handle Apply button
    observeEvent(input$apply, {
      new_row <- edit_row()

      # Gather factor inputs
      for (factor in all_risk_factors) {
        new_row[[factor]] <- input[[factor]] %||% FALSE
      }

      # Handle outbreak status
      new_row$last_outbreak_end_date <- switch(
        input$outbreak_status %||% "none",
        "none" = as.Date("1900-01-01"),
        "current" = as.Date("2999-01-01"),
        "past" = input$outbreak_date %||% as.Date("1900-01-01")
      )

      # Update data source
      new_row$data_source <- paste0("User ", Sys.info()[["user"]], " - ", Sys.Date())

      # Store the updated row and close modal
      returnList(list(
        operation = "upsert",
        id = input$country_id,
        data = new_row
      ))
      removeModal()
    })

    # Handle Cancel button
    observeEvent(input$cancel, {
      removeModal()
    })

    # Handle Delete button
    observeEvent(input$delete, {
      # Return special delete signal
      returnList(list(
        action = "delete",
        id = input$country_id,
        data = NULL
      ))
      removeModal()
    })

    # Return the updated row data
    return(returnList)
  })
}
