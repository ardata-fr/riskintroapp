importEmissionRiskFactorsUI <- function(id) {
  ns <- NS(id)
  dropMenu(
    tag = actionButton(
      inputId = ns("dropMenu"),
      label = "Import"
    ),
    placement = "right",
    tagList(
      div(
        actionButton(
          inputId = ns("wahis_open"),
          label = "Import WAHIS data",
          style = "width: 200px; margin-bottom: 7px;"
        )
      ),
      div(
        actionButton(
          inputId = ns("import"),
          label = "Manual data entry",
          style = "width: 200px; margin-bottom: 7px;"
        )
      ),
      div(
        actionButton(
          inputId = ns("new"),
          label = "Import dataset",
          style = "width: 200px;"
        )
      )
    )
  )
}

#' @importFrom datamods select_group_server
#' @importFrom reactable reactable renderReactable reactableOutput
importEmissionRiskFactorsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns

      returnDataset <- reactiveVal(NULL)

      # WAHIS ------
      filtered_wahis <- select_group_server(
        id = "my_filters",
        data_r = reactive(as.data.frame(riskintrodata::wahis_emission_risk_factors)),
        vars_r = reactive(c("disease", "species", "animal_category"))
      )

      output$table <- reactable::renderReactable({
        reactable::reactable(
          dplyr::select(
            filtered_wahis(),
            !!!list(
              `ISO3 Code` = "iso3",
              `Country name` = "country",
              Disease = "disease",
              Species = "species",
              `Animal Category` = "animal_category"
            )
          )
        )
      })

      observeEvent(input$wahis_open,{
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(modalDialog(
          fluidRow(
            column(
              width = 10, offset = 1,
              tags$h3("Select filters for your study"),
              shinyWidgets::panel(
                select_group_ui(
                  id = ns("my_filters"),
                  params = list(
                    list(inputId = "disease", label = "Disease", multiple = FALSE),
                    list(inputId = "species", label = "Species", multiple = FALSE),
                    list(inputId = "animal_category", label = "Animal Category", multiple = FALSE)
                  ),
                  vs_args = list(
                    search = TRUE,
                    hideClearButton = FALSE
                  ),
                  btn_reset_label = NULL
                ),
                status = "primary"
              ),
              reactable::reactableOutput(outputId = ns("table"))
            )
          ),
          footer = list(
            actionButton(
              inputId = ns("wahis_apply"),
              class = "btn-primary",
              label = "Import",
              disabled = TRUE),
            actionButton(
              inputId = ns("wahis_cancel"),
              label = "Cancel",
              class = "btn-default"
              )
          ), size = "xl",easyClose = TRUE
        ))

        apply_btn_observer <<- observe({
          valid_selection <- all(nchar(unlist(attr(filtered_wahis(), "inputs"))) > 0)
          if (valid_selection) enable("wahis_apply") else disable(id = "wahis_apply")
        })
      })
      observeEvent(input$wahis_apply,{
        removeModal()
        if (!is.null(apply_btn_observer)) {
          apply_btn_observer$destroy()
          apply_btn_observer <- NULL
        }
        # TODO:
        # validate_table_content()
        returnDataset(filtered_wahis())
      })
      observeEvent(input$wahis_cancel,{
        removeModal()
        if (!is.null(apply_btn_observer)) {
          apply_btn_observer$destroy()
          apply_btn_observer <- NULL
        }
      })

      # IMPORT ----

      # MANUAL ENTRY ----

      returnDataset
    })}

