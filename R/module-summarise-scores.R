
#' @importFrom shinyWidgets
#'  awesomeCheckboxGroup awesomeRadio awesomeCheckbox
summariseScoresUI <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      # Show panel when risk table exists
      condition = "output.show_panel",
      ns = NS(id),

      tags$h4("Summarise scores"),
      awesomeCheckboxGroup(
        inputId = ns("score_selector"),
        label = "Select scores",
        choices = character(0)
      ),
      awesomeRadio(
        inputId = ns("summary_method"),
        label = "Select method",
        choices = list(
          "Average" = "mean",
          "Max" = "max",
          "Min" = "min"
        ),
        selected = NULL
      ),
      # awesomeCheckbox(
      #   inputId = ns("use_override"),
      #   label = "Use override",
      #   value = FALSE,
      #   status = "primary",
      #   width = NULL
      # ),
      tags$hr()
    )

  )
}

#' @importFrom riskintroanalysis summarise_scores
summariseScoresServer <- function(id, epi_units, misc_risk_table, core_risks, overwriter_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # riskScoresTable ----
      riskScoresTable <- reactive({
        req(epi_units())

        # Build complete risk table here
        # Initlise table
        rt <- riskintroanalysis::risk_table(
          epi_units = epi_units(),
          scale = c(0, 100)
        )

        # Add core risk tables
        if (isTruthy(core_risks())) {
          risk_data <- nullify(core_risks())
          for (risk_table in risk_data) {
            rt <- add_risk(
              risk_table = rt,
              risk_data = risk_table
            )
          }
        }

        # Add misc risk tables
        if (isTruthy(misc_risk_table())) {
          misc_risk_cols <- attr(misc_risk_table(), "risk_cols")
          for (col in misc_risk_cols) {
            rt <- add_risk(
              risk_table = rt,
              risk_data = misc_risk_table(),
              cols = col,
              join_by = "eu_id"
            )
          }
        }

        rt
      })

      riskScoresNames <- reactive({
        req(riskScoresTable())
        attr(riskScoresTable(), "risk_cols")
      })

      # conditionaPanel ----
      output$show_panel <- reactive({
        req(riskScoresNames())
        length(riskScoresNames()) > 1
      })
      outputOptions(output, "show_panel", suspendWhenHidden = FALSE)

      observe({
        req(riskScoresNames())
        updateAwesomeCheckboxGroup(
          inputId = "score_selector",
          choices = riskScoresNames(),
          selected = riskScoresNames()
        )
      })

      returnDataset <- reactiveVal(NULL)
      observe({
        rt <- req(riskScoresTable())
        existing_cols <- colnames(rt)
        summarised_rt <- summarise_scores(
          risk_table = rt,
          cols = intersect(input$score_selector, existing_cols),
          method = input$summary_method,
          name_to = "overall_risk",
          keep_cols = TRUE,
          overwrite_table = overwriter_data()
        )

        returnDataset(summarised_rt)
      })

      returnDataset
    })
}
