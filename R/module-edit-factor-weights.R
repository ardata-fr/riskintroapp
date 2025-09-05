#' Emission Factor Weights Editor Modal UI
#'
#' Creates a modal dialog interface for editing emission factor weights.
#' The UI displays organized sliders for surveillance and control measure weights
#' with real-time subtotal calculations to ensure proper weighting constraints.
#'
#' @description
#' This UI function generates a modal dialog with two main sections:
#' - **Surveillance Measures** (must sum to 2.0): Disease notification, targeted
#'   surveillance, general surveillance, and screening programs
#' - **Control Measures** (must sum to 3.0): Border precautions, emergency slaughter,
#'   selective culling, movement zoning, and vaccination programs
#'
#' The interface shows real-time subtotals and validates that weights sum to the
#' correct totals (surveillance: 2.0, control: 3.0, overall: 5.0).
#'
#' @param id Character string. The namespace identifier for this module instance.
#'
#' @return A [shiny::modalDialog()] object containing the weight editing interface
#'   with organized sliders, real-time validation, and action buttons.
#'
#' @section Weight Constraints:
#' The weights must satisfy these constraints from the emission risk algorithm:
#' - **Surveillance measures total**: 2.0 points
#' - **Control measures total**: 3.0 points
#' - **Overall total**: 5.0 points
#' - **Individual range**: 0.0 to 3.0 for each factor
#'
#' @family emission-factor-weights
#' @seealso [emissionFactorWeightsServer()] for the corresponding server logic
#' @seealso [riskintrodata::get_erf_weights()] for the default weights
#' @seealso [riskintroanalysis::calc_emission_risk()] for the scoring algorithm
#'
#' @importFrom shiny NS modalDialog tags fluidRow column actionButton
#' @importFrom shinyjs useShinyjs
#' @export
emissionFactorWeightsUI <- function(id) {
  ns <- NS(id)

  # Get default weights from riskintrodata
  default_weights <- riskintrodata::get_erf_weights()

  modalDialog(
    title = "Edit emission factor weights",
    size = "l",
    easyClose = TRUE,

    shinyjs::useShinyjs(),

    # JavaScript for real-time validation
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateSubtotal', function(message) {
        var element = document.getElementById(message.id);
        if (element) {
          element.innerHTML = message.value;
          // Add visual feedback for validation
          if (message.valid) {
            element.style.color = '#28a745';
          } else {
            element.style.color = '#dc3545';
          }
        }
      });
    ")),

    tags$p(class = "text-muted",
           "Adjust the weights for each risk factor. Surveillance measures must sum to 2.0,
            control measures must sum to 3.0 (total = 5.0). The weights are used to calculate
            the total emission scores for each country."),

    fluidRow(
      # Surveillance Measures Column ---------
      column(6,
        tags$div(
          style = "border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-bottom: 15px;",
          tags$h5(
            style = "margin-top: 0; color: #28a745; border-bottom: 2px solid #28a745; padding-bottom: 5px;",
            "Surveillance Measures"
          ),

          # Disease notification
          tags$div(
            style = "margin-bottom: 15px;",
            tags$label(class = "control-label", "Disease notification system"),
            sliderInput(
              inputId = ns("disease_notification"),
              label = NULL,
              min = 0, max = 2, step = 0.25,
              value = default_weights$disease_notification
            )
          ),

          # Targeted surveillance
          tags$div(
            style = "margin-bottom: 15px;",
            tags$label(class = "control-label", "Targeted surveillance"),
            sliderInput(
              inputId = ns("targeted_surveillance"),
              label = NULL,
              min = 0, max = 2, step = 0.25,
              value = default_weights$targeted_surveillance
            )
          ),

          # General surveillance
          tags$div(
            style = "margin-bottom: 15px;",
            tags$label(class = "control-label", "General surveillance"),
            sliderInput(
              inputId = ns("general_surveillance"),
              label = NULL,
              min = 0, max = 2, step = 0.25,
              value = default_weights$general_surveillance
            )
          ),

          # Screening
          tags$div(
            style = "margin-bottom: 20px;",
            tags$label(class = "control-label", "Screening programs"),
            sliderInput(
              inputId = ns("screening"),
              label = NULL,
              min = 0, max = 2, step = 0.25,
              value = default_weights$screening
            )
          ),

          # Surveillance subtotal
          tags$div(
            style = "border-top: 2px solid #28a745; padding-top: 10px; text-align: center;",
            tags$strong("Surveillance Total: "),
            tags$strong(
              id = ns("surveillance_total"),
              style = "font-size: 1.1em;",
              "2.0 / 2"
            )
          )
        )
      ),

      # Control Measures Column -------
      column(6,
        tags$div(
          style = "border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-bottom: 15px;",
          tags$h5(
            style = "margin-top: 0; color: #007bff; border-bottom: 2px solid #007bff; padding-bottom: 5px;",
            "Control Measures"
          ),

          # Border precautions
          tags$div(
            style = "margin-bottom: 15px;",
            tags$label(class = "control-label", "Border precautions"),
            sliderInput(
              inputId = ns("precautions_at_the_borders"),
              label = NULL,
              min = 0, max = 3, step = 0.25,
              value = default_weights$precautions_at_the_borders
            )
          ),

          # Emergency slaughter
          tags$div(
            style = "margin-bottom: 15px;",
            tags$label(class = "control-label", "Emergency slaughter"),
            sliderInput(
              inputId = ns("slaughter"),
              label = NULL,
              min = 0, max = 3, step = 0.25,
              value = default_weights$slaughter
            )
          ),

          # Selective killing
          tags$div(
            style = "margin-bottom: 15px;",
            tags$label(class = "control-label", "Selective culling & disposal"),
            sliderInput(
              inputId = ns("selective_killing_and_disposal"),
              label = NULL,
              min = 0, max = 3, step = 0.25,
              value = default_weights$selective_killing_and_disposal
            )
          ),

          # Movement zoning
          tags$div(
            style = "margin-bottom: 15px;",
            tags$label(class = "control-label", "Movement zoning"),
            sliderInput(
              inputId = ns("zoning"),
              label = NULL,
              min = 0, max = 3, step = 0.25,
              value = default_weights$zoning
            )
          ),

          # Official vaccination
          tags$div(
            style = "margin-bottom: 20px;",
            tags$label(class = "control-label", "Official vaccination programs"),
            sliderInput(
              inputId = ns("official_vaccination"),
              label = NULL,
              min = 0, max = 3, step = 0.25,
              value = default_weights$official_vaccination
            )
          ),

          # Control subtotal
          tags$div(
            style = "border-top: 2px solid #007bff; padding-top: 10px; text-align: center;",
            tags$strong("Control Total: "),
            tags$strong(
              id = ns("control_total"),
              style = "font-size: 1.1em;",
              "3.0 / 3"
            )
          )
        )
      )
    ),

    # Overall total
    tags$div(
      style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #6c757d; border-radius: 5px;",
      fluidRow(
        column(6, tags$h5("Overall Total:", style = "margin: 0;")),
        column(6, tags$h4(
          style = "margin: 0; text-align: right; font-weight: bold;",
          tags$span(id = ns("overall_total"), "5.0"), " / 5"
        ))
      )
    ),

    footer = list(
      actionButton(ns("reset"), "Reset to Defaults", class = "btn-secondary"),
      actionButton(ns("apply"), "Apply", class = "btn-success"),
      actionButton(ns("cancel"), "Cancel", class = "btn-light")
    )
  )
}

#' Emission Factor Weights Editor Server Logic
#'
#' Handles the server-side logic for the emission factor weights editor modal.
#' Manages weight validation, real-time calculations, and data persistence.
#'
#' @param id Character string. The namespace identifier matching the UI function.
#' @param current_weights Reactive expression returning the current weights list.
#'   Should have the same structure as [riskintrodata::get_erf_weights()].
#'
#' @return A reactive expression that returns updated weights when Apply is clicked,
#'   or NULL when Cancel is clicked or modal is closed.
#'
#' @section Weight Validation:
#' The server enforces these validation rules:
#' - Surveillance measures must sum to exactly 2.0
#' - Control measures must sum to exactly 3.0
#' - Overall total must sum to exactly 5.0
#' - Individual weights must be between 0.0 and 3.0
#'
#' @section Real-time Updates:
#' The interface provides live feedback as users adjust sliders:
#' - Subtotals update immediately
#' - Color coding shows validation status (green=valid, red=invalid)
#' - Apply button is disabled when totals are incorrect
#'
#' @family emission-factor-weights
#' @seealso [emissionFactorWeightsUI()] for the corresponding UI function
#'
#' @importFrom shiny moduleServer req reactive observe updateSliderInput
#' @importFrom shinyjs enable disable
#' @export
emissionFactorWeightsServer <- function(id, current_weights) {

  # Weight factor names
  surveillance_factors <- c("disease_notification", "targeted_surveillance",
                           "general_surveillance", "screening")
  control_factors <- c("precautions_at_the_borders", "slaughter",
                      "selective_killing_and_disposal", "zoning",
                      "official_vaccination")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Real-time weight calculations
    surveillance_total <- reactive({
      sum(
        input$disease_notification,
        input$targeted_surveillance,
        input$general_surveillance,
        input$screening
      )
    })

    control_total <- reactive({
      sum(
        input$precautions_at_the_borders,
        input$slaughter,
        input$selective_killing_and_disposal,
        input$zoning,
        input$official_vaccination
      )
    })

    overall_total <- reactive({
      surveillance_total() + control_total()
    })

    # Validation status
    is_valid <- reactive({
      abs(surveillance_total() - 2.0) < 0.001 &&
      abs(control_total() - 3.0) < 0.001 &&
      abs(overall_total() - 5.0) < 0.001
    })

    # Update displays and validation in real-time
    observe({
      # Update surveillance total
      session$sendCustomMessage("updateSubtotal", list(
        id = ns("surveillance_total"),
        value = sprintf("%.2f / 2", surveillance_total()),
        valid = abs(surveillance_total() - 2.0) < 0.001
      ))

      # Update control total
      session$sendCustomMessage("updateSubtotal", list(
        id = ns("control_total"),
        value = sprintf("%.2f / 3", control_total()),
        valid = abs(control_total() - 3.0) < 0.001
      ))

      # Update overall total
      session$sendCustomMessage("updateSubtotal", list(
        id = ns("overall_total"),
        value = sprintf("%.2f", overall_total()),
        valid = is_valid()
      ))

      # Enable/disable Apply button based on validation
      if (is_valid()) {
        shinyjs::enable("apply")
      } else {
        shinyjs::disable("apply")
      }
    })

    # Return value for the updated weights
    returnValue <- reactiveVal(riskintrodata::emission_risk_weights)

    # Handle Apply button
    observeEvent(input$apply, {
      req(is_valid())

      # Create updated weights list
      new_weights <- list(
        disease_notification = input$disease_notification,
        targeted_surveillance = input$targeted_surveillance,
        general_surveillance = input$general_surveillance,
        screening = input$screening,
        precautions_at_the_borders = input$precautions_at_the_borders,
        slaughter = input$slaughter,
        selective_killing_and_disposal = input$selective_killing_and_disposal,
        zoning = input$zoning,
        official_vaccination = input$official_vaccination
      )

      returnValue(new_weights)
      removeModal()
    })

    # Handle Reset button
    observeEvent(input$reset, {
      default_weights <- riskintrodata::get_erf_weights()

      for (factor in surveillance_factors) {
        updateSliderInput(session, factor, value = default_weights[[factor]])
      }
      for (factor in control_factors) {
        updateSliderInput(session, factor, value = default_weights[[factor]])
      }
    })

    # Handle Cancel button
    observeEvent(input$cancel, {
      removeModal()
    })

    return(returnValue)
  })
}
