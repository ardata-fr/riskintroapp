
#' @importFrom riskintrodata iso3_to_name
riskFactorEditorUI <- function(id, country_id) {
  ns <- NS(id)

  modalDialog(
    title = sprintf("Edit emission risk factors for %s (%s)", riskintrodata::iso3_to_name(country_id), country_id),
    size = "xl",
    easyClose = TRUE,
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
          style = "background-color: #f8f9fa; border-bottom: 2px solid #dee2e6;",
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
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;", "0.25"),
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
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;", "0.50"),
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
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;", "0.50"),
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
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; padding: 8px;", "0.75"),
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
          style = "background-color: #f8f9fa; border-bottom: 2px solid #dee2e6;",
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
          tags$td(style = "width: 10%; text-align: center; vertical-align: middle; font-weight: bold;", "1.0"),
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
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;", "0.5"),
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
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;", "0.5"),
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
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;", "0.75"),
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
          tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;", "0.25"),
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
          style = "background-color: #f8f9fa; border-bottom: 2px solid #dee2e6;",
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
          style = "background-color: #f8f9fa; border-bottom: 2px solid #dee2e6;",
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
                value = NULL
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

#' @importFrom shinyWidgets updatePrettySwitch updateRadioGroupButtons
#' @importFrom shiny updateDateInput moduleServer req reactive observe
#' @importFrom dplyr filter if_else
#' @importFrom riskintrodata iso3_to_name
riskFactorEditorServer <- function(id, emission_risk_factors, country_id) {

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

    # Get weights from riskintrodata
    weights <- riskintrodata::get_erf_weights()

    # calculate real-time scores using riskintroanalysis::calc_emission_risk ----
    current_scores <- reactive({
      # Create a temporary data frame with current input values
      temp_row <- edit_row()

      # Update with current input values (convert UI TRUE/FALSE to data 0/1)
      for (factor in all_risk_factors) {
        temp_row[[factor]] <- if (input[[factor]] %||% FALSE) 1 else 0
      }

      # Handle outbreak status
      temp_row$last_outbreak_end_date <- switch(
        input$outbreak_status %||% "none",
        "none" = as.Date("1900-01-01"),
        "current" = as.Date("2999-01-01"),
        "past" = input$outbreak_date %||% as.Date("1900-01-01")
      )

      # Use calc_emission_risk to get all scores consistently
      result <- riskintroanalysis::calc_emission_risk(temp_row, keep_scores = TRUE)

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
      # Individual surveillance scores
      session$sendCustomMessage("updateScore", list(
        id = ns("score_disease_notification"),
        value = if (input$disease_notification %||% FALSE) weights$disease_notification else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_targeted_surveillance"),
        value = if (input$targeted_surveillance %||% FALSE) weights$targeted_surveillance else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_general_surveillance"),
        value = if (input$general_surveillance %||% FALSE) weights$general_surveillance else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_screening"),
        value = if (input$screening %||% FALSE) weights$screening else 0
      ))

      # Individual control scores
      session$sendCustomMessage("updateScore", list(
        id = ns("score_precautions_at_the_borders"),
        value = if (input$precautions_at_the_borders %||% FALSE) weights$precautions_at_the_borders else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_slaughter"),
        value = if (input$slaughter %||% FALSE) weights$slaughter else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_selective_killing_and_disposal"),
        value = if (input$selective_killing_and_disposal %||% FALSE) weights$selective_killing_and_disposal else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_zoning"),
        value = if (input$zoning %||% FALSE) weights$zoning else 0
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_official_vaccination"),
        value = if (input$official_vaccination %||% FALSE) weights$official_vaccination else 0
      ))

      # Commerce scores
      session$sendCustomMessage("updateScore", list(
        id = ns("score_commerce_legal"),
        value = paste0(if (input$commerce_legal %||% FALSE) 1 else 0, "/1")
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("score_commerce_illegal"),
        value = paste0(if (input$commerce_illegal %||% FALSE) 3 else 0, "/3")
      ))

      # Outbreak score (rounded to 1 decimal)
      session$sendCustomMessage("updateScore", list(
        id = ns("score_outbreak"),
        value = paste0(round(outbreak_score(), 1), "/3")
      ))

      # Subtotals
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
        value = round(commerce_score(), 1)
      ))
      session$sendCustomMessage("updateScore", list(
        id = ns("epi_subtotal"),
        value = round(outbreak_score(), 1)
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

      # Convert data values (0/1/NA) to UI logic (TRUE/FALSE)
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
        id = country_id(),
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
        id = country_id(),
        data = NULL
      ))
      removeModal()
    })

    # Return the updated row data
    return(returnList)
  })
}
