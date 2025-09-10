
jokerManagerUI <- function(id) {
  ns <- NS(id)
  tagList(
    dropMenu(
      tag = actionButton(
        inputId = ns("dropMenu"),
        label = "Joker manager",
        icon = icon("mask"),
      ),
      placement = "right",
      tags$table(
        class = 'table table-borderless table-match-columns',
        tags$tr(tags$td("Add new joker")),
        tags$tr(
          tags$td(
            textInput(
              inputId = ns("name"),
              label = NULL,
              placeholder = "New risk source name"
            )
          ),
          tags$td(
            actionButton(
              inputId = ns("add"),
              label = "",
              icon = icon("plus")
            )
          )
        ),
        tags$tr(tags$td("Select joker to edit")),
        tags$tr(
          tags$td(
            selectInput(
              inputId = ns("select"),
              label = NULL,
              choices = character()
            )
          ),
          tags$td(
            actionButton(
              inputId = ns("edit"),
              label = "",
              icon = icon("pen-to-square")
            )
          )
        )
      )
    )
  )

}

#' @importFrom shinyWidgets show_alert
#' @importFrom stats setNames
jokerManagerServer <- function(id, emission_risk_factors) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    returnList <- reactiveVal(NULL)
    counter <- reactiveVal(1L)

    joker_data <- reactive({
      req(emission_risk_factors())
      emission_risk_factors()[grepl(pattern = '^JOKER-', x = emission_risk_factors()$iso3), ]
    })

    observe({
      req(joker_data())
      choices <- joker_data()$iso3
      choices <- setNames(choices, nm = joker_data()$country)
      updateSelectInput(
        inputId = "select",
        choices = choices
      )
    })

    observeEvent(input$add, {
      req(joker_data())
      req(input$name)
      req(!input$name %in% joker_data()$country)
      updateTextInput(inputId = "name", value = "")

      study_settings <- emission_risk_factors()[1,c("disease", "animal_category", "species")]
      joker_id <- sprintf("JOKER-%05i", nrow(joker_data()) + 1)

      new_row <- riskintrodata::erf_row(
        iso3 = joker_id,
        country = input$name,
        disease = study_settings$disease,
        animal_category = study_settings$animal_category,
        species = study_settings$species
      )

      counter(counter() + 1)
      res <- list(
        operation = "upsert",
        id = joker_id,
        data = new_row,
        counter = counter()
      )
      print(res)
      returnList(res)
    })

    observeEvent(input$edit, {
      req(input$select)
      hideDropMenu(id = "dropMenu_dropmenu")
      counter(counter() + 1)
      returnList(list(
        operation = "open_editor",
        id = input$select,
        data = NULL,
        counter = counter()
      ))
    })


    return(returnList)

  })
}



