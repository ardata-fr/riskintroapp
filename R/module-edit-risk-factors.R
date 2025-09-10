
#' Risk Factor Editor Modal UI
#'
#' Creates a modal dialog interface for editing emission risk factors for a specific country.
#' The UI displays organized tables for surveillance, control, commerce, and epidemiological factors
#' with real-time risk score calculations.
#'
#' @description
#' This UI function generates a modal dialog with four main sections:
#' - **Surveillance Measures** (2/12 points): Disease notification, targeted surveillance,
#'   general surveillance, and screening programs
#' - **Control Measures** (3/12 points): Border precautions, emergency slaughter,
#'   selective culling, movement zoning, and vaccination programs
#' - **Commerce** (0-4/12 points): Legal and illegal animal trade status
#' - **Epidemiological Status** (0-3/12 points): Time since last outbreak
#'
#' The interface shows real-time risk score calculations and validates that the total
#' score matches the emission risk algorithm from [riskintroanalysis::calc_emission_risk()].
#'
#' @param id Character string. The namespace identifier for this module instance.
#' @param country_id Character string. The ISO3 country code (e.g., "USA", "FRA") for the
#'   country being edited.
#' @param current_weights List. The current emission risk factor weights for display in the UI.
#'
#' @return A [shiny::modalDialog()] object containing the risk factor editing interface
#'   with organized tables, real-time scoring, and action buttons.
#'
#' @section Risk Factor Categories:
#' The risk factors are organized by category with specific point contributions:
#'
#' **Surveillance Measures (2/12 total)**:
#' - Disease notification system
#' - Targeted surveillance
#' - General surveillance
#' - Screening programs
#'
#' **Control Measures (3/12 total)**:
#' - Border precautions
#' - Emergency slaughter
#' - Selective culling & disposal
#' - Movement zoning
#' - Official vaccination programs
#'
#' **Commerce (0-4/12 total)**:
#' - No trade: 0 points
#' - Legal trade only: 1 point
#' - Illegal trade only: 3 points
#' - Both legal and illegal: 4 points
#'
#' **Epidemiological Status (0-3/12 total)**:
#' - No outbreaks (>5 years): 0 points
#' - Time-based decay for recent outbreaks
#' - Current outbreak: 3 points
#'
#' @family risk-factor-editor
#' @seealso [riskFactorEditorServer()] for the corresponding server logic
#' @seealso [riskintroanalysis::calc_emission_risk()] for the risk calculation algorithm
#'
#' @importFrom shiny NS modalDialog tags fluidRow column actionButton conditionalPanel dateInput
#' @importFrom shinyWidgets prettySwitch radioGroupButtons
#' @importFrom riskintrodata iso3_to_name
#' @export
riskFactorEditorUI <- function(id, country_id, current_weights) {
  ns <- NS(id)
  weights <- current_weights

  modalDialog(
    title = sprintf("Edit emission risk factors for %s (%s)", riskintrodata::iso3_to_name(country_id), country_id),
    size = "xl",
    easyClose = TRUE,
    # CSS for colored table headers
    tags$style(HTML("
      .surveillance-header th {
        color: #28a745 !important;
        font-weight: bold !important;
      }
      .control-header th {
        color: #007bff !important;
        font-weight: bold !important;
      }
      .commerce-header th {
        color: #fd7e14 !important;
        font-weight: bold !important;
      }
      .epi-header th {
        color: #6f42c1 !important;
        font-weight: bold !important;
      }
    ")),

    # JavaScript for updating scores
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateScore', function(message) {
        var element = document.getElementById(message.id);
        if (element) {
          element.innerHTML = message.value;
        }
      });
    ")),

    # Surveillance Measures Table -----
    tags$table(
      class = "table",
      style = "margin-bottom: 20px; border: 1px solid #dee2e6;",
      tags$thead(
        tags$tr(
          class = "surveillance-header",
          tags$th(style = "width: 50%; padding: 8px; font-weight: bold;", "Surveillance Measures"),
          tags$th(style = "width: 30%; text-align: center; padding: 8px; font-weight: bold;", "No measure"),
          tags$th(style = "width: 10%; text-align: center; padding: 8px; font-weight: bold;", "Weight"),
          tags$th(style = "width: 10%; text-align: center; padding: 8px; font-weight: bold;", "Score")
        )
      ),
      tags$tbody(
        tags$tr(
          style = "border-bottom: 1px solid #dee2e6;",
          tags$td(style = "vertical-align: middle; padding: 8px;", "Disease notification system"),
          tags$td(style = "text-align: center; vertical-align: middle; padding: 8px;",
            shinyWidgets::prettySwitch(
              inputId = ns("disease_notification"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("weight_disease_notification"), as.character(weights$disease_notification))),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("score_disease_notification"), "0.25"))
        ),
        tags$tr(
          style = "border-bottom: 1px solid #dee2e6;",
          tags$td(style = "vertical-align: middle; padding: 8px;", "Targeted surveillance"),
          tags$td(style = "text-align: center; vertical-align: middle; padding: 8px;",
            shinyWidgets::prettySwitch(
              inputId = ns("targeted_surveillance"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("weight_targeted_surveillance"), as.character(weights$targeted_surveillance))),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("score_targeted_surveillance"), "0.50"))
        ),
        tags$tr(
          style = "border-bottom: 1px solid #dee2e6;",
          tags$td(style = "vertical-align: middle; padding: 8px;", "General surveillance"),
          tags$td(style = "text-align: center; vertical-align: middle; padding: 8px;",
            shinyWidgets::prettySwitch(
              inputId = ns("general_surveillance"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("weight_general_surveillance"), as.character(weights$general_surveillance))),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("score_general_surveillance"), "0.50"))
        ),
        tags$tr(
          style = "border-bottom: 1px solid #dee2e6;",
          tags$td(style = "vertical-align: middle; padding: 8px;", "Screening programs"),
          tags$td(style = "text-align: center; vertical-align: middle; padding: 8px;",
            shinyWidgets::prettySwitch(
              inputId = ns("screening"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("weight_screening"), as.character(weights$screening))),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("score_screening"), "0.75"))
        ),
        tags$tr(
          style = "border-top: 2px solid #28a745; background-color: #f8f9fa;",
          tags$td(style = "padding: 8px;", tags$strong("Subtotal")),
          tags$td(style = "padding: 8px;", ""),
          tags$td(style = "text-align: center; font-weight: bold; padding: 8px;", "2.0"),
          tags$td(style = "text-align: center; font-weight: bold; color: #28a745; padding: 8px;",
                  tags$span(id = ns("surveillance_subtotal"), "2.0"), " / 2")
        )
      )
    ),

    tags$br(),

    # Control Measures Table -----
    tags$table(
      class = "table",
      style = "margin-bottom: 20px; border: 1px solid #dee2e6;",
      tags$thead(
        tags$tr(
          class = "control-header",
          tags$th(style = "width: 50%; padding: 8px; font-weight: bold;", "Control Measures"),
          tags$th(style = "width: 30%; text-align: center; padding: 8px; font-weight: bold;", "No measure"),
          tags$th(style = "width: 10%; text-align: center; padding: 8px; font-weight: bold;", "Weight"),
          tags$th(style = "width: 10%; text-align: center; padding: 8px; font-weight: bold;", "Score")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(style = "width: 50%; vertical-align: middle;", "Border precautions"),
          tags$td(style = "width: 30%; text-align: center; vertical-align: middle;",
            shinyWidgets::prettySwitch(
              inputId = ns("precautions_at_the_borders"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "width: 10%; text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("weight_precautions_at_the_borders"), as.character(weights$precautions_at_the_borders))),
          tags$td(style = "width: 10%; text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("score_precautions_at_the_borders"), "1.0"))
        ),
        tags$tr(
          tags$td("Emergency slaughter"),
          tags$td(style = "text-align: center; vertical-align: middle;",
            shinyWidgets::prettySwitch(
              inputId = ns("slaughter"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("weight_slaughter"), as.character(weights$slaughter))),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("score_slaughter"), "0.5"))
        ),
        tags$tr(
          tags$td("Selective culling & disposal"),
          tags$td(style = "text-align: center; vertical-align: middle;",
            shinyWidgets::prettySwitch(
              inputId = ns("selective_killing_and_disposal"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("weight_selective_killing_and_disposal"), as.character(weights$selective_killing_and_disposal))),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("score_selective_killing_and_disposal"), "0.5"))
        ),
        tags$tr(
          tags$td("Movement zoning"),
          tags$td(style = "text-align: center; vertical-align: middle;",
            shinyWidgets::prettySwitch(
              inputId = ns("zoning"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("weight_zoning"), as.character(weights$zoning))),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("score_zoning"), "0.75"))
        ),
        tags$tr(
          tags$td("Official vaccination programs"),
          tags$td(style = "text-align: center; vertical-align: middle;",
            shinyWidgets::prettySwitch(
              inputId = ns("official_vaccination"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("weight_official_vaccination"), as.character(weights$official_vaccination))),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;",
                  tags$span(id = ns("score_official_vaccination"), "0.25"))
        ),
        tags$tr(
          style = "border-top: 2px solid #007bff;",
          tags$td(tags$strong("Subtotal")),
          tags$td(""),
          tags$td(style = "text-align: center; font-weight: bold;", "3.0"),
          tags$td(style = "text-align: center; font-weight: bold; color: #007bff;",
                  tags$span(id = ns("control_subtotal"), "3.0"), " / 3")
        )
      )
    ),

    tags$br(),

    # Commerce Table -----
    tags$table(
      class = "table",
      style = "margin-bottom: 20px; border: 1px solid #dee2e6;",
      tags$thead(
        tags$tr(
          class = "commerce-header",
          tags$th(style = "width: 50%; padding: 8px; font-weight: bold;", "Animal Commerce"),
          tags$th(style = "width: 30%; text-align: center; padding: 8px; font-weight: bold;", "Trade Present"),
          tags$th(style = "width: 10%; text-align: center; padding: 8px; font-weight: bold;", " "),
          tags$th(style = "width: 10%; text-align: center; padding: 8px; font-weight: bold;", "Score")
        )
      ),
      tags$tbody(
        tags$tr(
          style = "border-bottom: 1px solid #dee2e6;",
          tags$td(style = "vertical-align: middle; padding: 8px;", "Legal trade present"),
          tags$td(style = "text-align: center; vertical-align: middle; padding: 8px;",
            shinyWidgets::prettySwitch(
              inputId = ns("commerce_legal"), label = NULL,
              status = "warning", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(""),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("score_commerce_legal"), "0/1"))
        ),
        tags$tr(
          style = "border-bottom: 1px solid #dee2e6;",
          tags$td(style = "vertical-align: middle; padding: 8px;", "Illegal trade present"),
          tags$td(style = "text-align: center; vertical-align: middle; padding: 8px;",
            shinyWidgets::prettySwitch(
              inputId = ns("commerce_illegal"), label = NULL,
              status = "danger", fill = TRUE, inline = TRUE
            )
          ),
          tags$td(""),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("score_commerce_illegal"), "0/3"))
        ),
        tags$tr(
          style = "border-top: 2px solid #fd7e14; background-color: #f8f9fa;",
          tags$td(style = "padding: 8px;", tags$strong("Subtotal")),
          tags$td(style = "padding: 8px;", ""),
          tags$td(""),
          tags$td(style = "text-align: center; font-weight: bold; color: #fd7e14; padding: 8px;",
                  tags$span(id = ns("commerce_subtotal"), "0.0"), " / 4")
        )
      )
    ),

    tags$br(),

    # Epidemiological Status Table -----
    tags$table(
      class = "table",
      style = "margin-bottom: 20px; border: 1px solid #dee2e6;",
      tags$thead(
        tags$tr(
          class = "epi-header",
          tags$th(style = "width: 30%; padding: 8px; font-weight: bold;", "Epidemiological Status"),
          tags$th(style = "width: 30%; text-align: center; padding: 8px; font-weight: bold;", "Status"),
          tags$th(style = "width: 30%; text-align: center; padding: 8px; font-weight: bold;", "Date (if past)"),
          tags$th(style = "width: 10%; text-align: center; padding: 8px; font-weight: bold;", "Score")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(style = "vertical-align: middle; padding: 8px;", "Outbreak status"),
          tags$td(style = "text-align: center; vertical-align: middle; padding: 8px;",
            shinyWidgets::radioGroupButtons(
              inputId = ns("outbreak_status"),
              label = NULL,
              choices = list(
                "None" = "none",
                "Past" = "past",
                "Current" = "current"
              ),
              selected = "none",
              status = "primary",
              size = "sm",
              direction = "horizontal"
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; padding: 8px;",
            conditionalPanel(
              condition = paste0("input['", ns("outbreak_status"), "'] == 'past'"),
              dateInput(
                inputId = ns("outbreak_date"),
                label = NULL,
                value = NULL,
                format = "dd-mm-yyyy"
              )
            )
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;",
                  tags$span(id = ns("score_outbreak"), "0.0/3"))
        ),
        tags$tr(
          style = "border-top: 2px solid #6f42c1; background-color: #f8f9fa;",
          tags$td(style = "padding: 8px;", tags$strong("Subtotal")),
          tags$td(style = "padding: 8px;", ""),
          tags$td(style = "padding: 8px;", ""),
          tags$td(style = "text-align: center; font-weight: bold; color: #6f42c1; padding: 8px;",
                  tags$span(id = ns("epi_subtotal"), "0.0"), " / 3")
        )
      )
    ),

    # Grand Total
    tags$div(
      style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #dc3545; border-radius: 5px;",
      fluidRow(
        column(6, tags$h5("Total Emission Score:", style = "margin: 0;")),
        column(6, tags$h4(
          style = "margin: 0; text-align: right; color: #dc3545; font-weight: bold;",
          tags$span(id = ns("total_score"), "5.0"), " / 12"
        ))
      )
    ),

    footer = list(
      actionButton(ns("apply"), "Apply", class = "btn-success"),
      actionButton(ns("cancel"), "Cancel", class = "btn-secondary"),
      actionButton(ns("delete"), "Delete", class = "btn-danger")
    )
  )
}

#' Risk Factor Editor Server Logic
#'
#' Handles the server-side logic for the emission risk factor editor modal.
#' Manages data population, real-time score calculations, input validation, and data persistence.
#'
#' @param id Character string. The namespace identifier matching the UI function.
#' @param emission_risk_factors Reactive expression returning the current emission risk factors dataset.
#'   Should have the same structure as [riskintrodata::get_wahis_erf()].
#' @param country_id Reactive expression returning the ISO3 country code being edited.
#' @param current_weights reactive value containing list of emission risk factor weights.
#' @return A reactive expression that returns updated data when Apply is clicked,
#'   or delete operation when Delete is clicked, or NULL when Cancel is clicked.
#'
#' @section Data Format:
#' The server handles conversion between data storage format and UI format:
#'
#' **Storage format (in dataset)**:
#' - `0` = measure is in place (no risk)
#' - `1` = no measure in place (risk present)
#' - `NA` = unknown status (treated as risk present)
#'
#' **UI format (prettySwitch)**:
#' - `FALSE` = measure in place (switch OFF = no risk)
#' - `TRUE` = no measure in place (switch ON = risk present)
#'
#' @section Real-time Calculations:
#' The interface provides live feedback as users adjust inputs:
#' - Individual factor scores update immediately
#' - Subtotals for each category (surveillance, control, commerce, epidemiological)
#' - Grand total emission score out of 12
#' - Uses [riskintroanalysis::calc_emission_risk()] for consistency with actual calculations
#'
#' @section Return Values:
#' The server returns a list with the following structure:
#' - **Apply operation**: `list(operation = "upsert", id = country_id, data = updated_row)`
#' - **Delete operation**: `list(operation = "delete", id = country_id, data = NULL)`
#' - **Cancel operation**: Returns `NULL`
#'
#' @family risk-factor-editor
#' @seealso [riskFactorEditorUI()] for the corresponding UI function
#' @seealso [riskintroanalysis::calc_emission_risk()] for risk calculation details
#' @seealso [riskintrodata::get_erf_weights()] for emission factor weights
#'
#' @importFrom shiny moduleServer req reactive observe observeEvent removeModal
#' @importFrom shinyWidgets updatePrettySwitch updateRadioGroupButtons
#' @importFrom shiny updateDateInput
#' @importFrom dplyr filter if_else
#' @importFrom riskintrodata iso3_to_name get_erf_weights
#' @export
riskFactorEditorServer <- function(id, emission_risk_factors, country_id, current_weights) {

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
      req(country_id(), emission_risk_factors())

      current_data <- emission_risk_factors()
      target_country <- country_id()

      # Find existing row for this country
      existing_row <- current_data |>
        filter(.data[["iso3"]] == target_country)

      if (nrow(existing_row) > 0) {
        existing_row[1, ] # Take first row if multiple, although it should never happen
      } else {
        # Return template_row if no existing data - will replace with values

        study_settings <- attr(emission_risk_factors(), "study_settings")
        new_row <- riskintrodata::erf_row(
          iso3 = target_country,
          country = riskintrodata::iso3_to_name(target_country),
          disease = study_settings[["disease"]],
          animal_category = study_settings[["animal_category"]],
          species = study_settings[["species"]]
          # all other values take default high risk
        )
        new_row
      }
    })

    # calculate real-time scores using riskintroanalysis::calc_emission_risk ----
    current_scores <- reactive({

      # Create a temporary data frame with current input values
      temp_row <- edit_row()

      # Update with current input values
      for (factor in all_risk_factors) {
        temp_row[[factor]] <- if (input[[factor]] %||% FALSE) 1 else 0
      }
      # Handle outbreak status
      temp_row$last_outbreak_end_date <- switch(
        input$outbreak_status %||% "current",
        "none" = as.Date("1900-01-01"),
        "current" = as.Date("2999-01-01"),
        "past" = input$outbreak_date %||% as.Date("1900-01-01")
      )

      result <- riskintroanalysis::calc_emission_risk(
        temp_row,
        weights = current_weights(),
        keep_scores = TRUE
      )

      return(result)
    })

    # Extract individual scores from calc_emission_risk result
    surveillance_score <- reactive({
      current_scores()$sc_survmeasures[1]
    })

    control_score <- reactive({
      current_scores()$sc_control[1]
    })

    commerce_score <- reactive({
      current_scores()$sc_commerce[1]
    })

    outbreak_score <- reactive({
      current_scores()$sc_epistatus[1]
    })

    total_score <- reactive({
      current_scores()$emission_risk[1]
    })


    # display real-time scores ----
    observe({
      ## Individual scores ----
      session$sendCustomMessage("updateScore", list(
        id = ns("score_disease_notification"),
        value = if (input$disease_notification %||% FALSE) current_weights()$disease_notification else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_targeted_surveillance"),
        value = if (input$targeted_surveillance %||% FALSE) current_weights()$targeted_surveillance else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_general_surveillance"),
        value = if (input$general_surveillance %||% FALSE) current_weights()$general_surveillance else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_screening"),
        value = if (input$screening %||% FALSE) current_weights()$screening else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_precautions_at_the_borders"),
        value = if (input$precautions_at_the_borders %||% FALSE) current_weights()$precautions_at_the_borders else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_slaughter"),
        value = if (input$slaughter %||% FALSE) current_weights()$slaughter else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_selective_killing_and_disposal"),
        value = if (input$selective_killing_and_disposal %||% FALSE) current_weights()$selective_killing_and_disposal else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_zoning"),
        value = if (input$zoning %||% FALSE) current_weights()$zoning else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_official_vaccination"),
        value = if (input$official_vaccination %||% FALSE) current_weights()$official_vaccination else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_commerce_legal"),
        value = paste0(if (input$commerce_legal %||% FALSE) 1 else 0, "/1")
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_commerce_illegal"),
        value = paste0(if (input$commerce_illegal %||% FALSE) 3 else 0, "/3")
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_outbreak"),
        value = paste0(round(outbreak_score(), 1), "/3")
      ))

      ## Subtotals ----
      session$sendCustomMessage("updateScore", list(
        id = ns("surveillance_subtotal"),
        value = round(surveillance_score(), 2)
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("control_subtotal"),
        value = round(control_score(), 2)
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("commerce_subtotal"),
        value = round(commerce_score())
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("epi_subtotal"),
        value = round(outbreak_score(), 2)
      ))

      # Grand total
      session$sendCustomMessage("updateScore", list(
        id = ns("total_score"),
        value = round(total_score(), 1)
      ))
    })

    # Updates ---------
    # Populate inputs when modal opens (after UI is rendered)
    observe({
      country_data <- edit_row()

      # UI  : F = measure in place (good), T = no measure (risk), NA is converted to F
      # Data: 0 = measure in place (good), 1 = no measure (risk), NA = unknown (risk)

      for (factor in c(surveillance_factors, control_factors, commerce_factors)) {
        current_value <- country_data[[factor]]
        # Measures of control and surveillence, all are either 1 or 0
        #  0 = measure inplace, 1 = no measure inplace
        #  i.e. 1 = there is a risk, 0 = there is no risk).
        ui_value <- if_else(is.na(current_value), TRUE, as.logical(current_value))
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
        updateRadioGroupButtons(session, "outbreak_status", selected = "current")
      }

    })


    # Gather inputs -----------
    # Reactive value to store the updated row data
    returnList <- reactiveVal(NULL)

    # Counter ensures that each time time apply, cancel or delete are clicked
    # that the return value of this module changes, **even if** the data has not
    # changed. Required for reactivety, otherwise clicking the same country after
    # having just edited it without changing the data would not reopen the editor
    # modal.
    counter <- reactiveVal(1L)

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

      counter(counter() + 1)
      res <- list(
        operation = "upsert",
        id = country_id(),
        data = new_row,
        counter = counter()
      )
      returnList(res)
      removeModal()
    })

    # Handle Cancel button
    observeEvent(input$cancel, {
      counter(counter() + 1)
      res <- list(
        operation = NULL,
        counter = counter()
      )
      returnList(res)
      removeModal()
    })

    # Handle Delete button
    observeEvent(input$delete, {
      counter(counter() + 1)
      # Return special delete signal
      res <- list(
        operation = "delete",
        id = country_id(),
        data = NULL,
        counter = counter()
      )
      returnList(res)
      removeModal()
    })

    # Return the updated row data
    return(returnList)
  })
}
