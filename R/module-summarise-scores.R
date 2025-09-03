
#' @importFrom shinyWidgets
#'  awesomeCheckboxGroup awesomeRadio awesomeCheckbox
summariseScoresUI <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      # Show panel when risk table exists
      condition = "output.show_panel",
      ns = NS(id),

      div(h4("Summarise risk scores")),
      awesomeCheckboxGroup(
        inputId = ns("score_selector"),
        label = "Select scores to summarise",
        choices = character(0L)
      ),
      awesomeRadio(
        inputId = ns("summary_method"),
        label = "Select summary method",
        choices = list(
          "Average" = "mean",
          "Max" = "max",
          "Min" = "min"
        ),
        selected = NULL
      ),
      awesomeCheckbox(
        inputId = ns("use_override"),
        label = "Use override",
        value = FALSE,
        status = "primary",
        width = NULL
      )
    )

  )
}

summariseScoresServer <- function(id, risk_table, misc_risk_table) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      returnDataset <- reactiveVal(NULL)

      introRiskTable <- reactive({
        rt <- req(risk_table())
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
          rt
        } else {
          rt
        }
      })

      output$show_panel <- reactive({
        isTruthy(introRiskTable())
      })

      observe({
        rt <- req(introRiskTable())
        summarised_rt <- summarise_risk_scores(
          risk_table = rt,
          cols = input$score_selector,
          method = input$summary_method,
          name_to = "overall_risk",
          keep_cols = TRUE
        )
        returnDataset(summarised_rt)
      })

      returnDataset
    })
}
