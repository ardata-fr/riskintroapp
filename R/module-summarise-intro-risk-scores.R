
#' @importFrom shinyWidgets
#'  awesomeCheckboxGroup awesomeRadio awesomeCheckbox
summariseRiskScoresUI <- function(id) {
  ns <- NS(id)
  tagList(


    conditionalPanel(
      # Show panel when risk table exists
      condition = "input.breaks == 'show_panel'",
      ns=NS(id),

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
      )#,
      # awesomeCheckbox(
      #   inputId = ns("use_override"),
      #   label = "Use override",
      #   value = FALSE,
      #   status = "primary",
      #   width = NULL
      # )
    )

  )
}

summariseRiskScoresServer <- function(id, riskTable) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      returnDataset <- reactiveVal(NULL)

      output$show_panel <- reactive({
        isTruth(riskTable())
      })

      observe({
        rt <- req(riskTable())
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
