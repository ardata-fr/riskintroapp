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
          inputId = ns("import_open"),
          label = "Manual data entry",
          style = "width: 200px; margin-bottom: 7px;"
        )
      ),
      div(
        actionButton(
          inputId = ns("manual_open"),
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
              tags$text("INSERT LABEL HERE"),
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

        wahis_btn_observer <<- observe({
          valid_selection <- all(nchar(unlist(attr(filtered_wahis(), "inputs"))) > 0)
          if (valid_selection) enable("wahis_apply") else disable(id = "wahis_apply")
        })
      })
      observeEvent(input$wahis_apply,{
        removeModal()
        if (!is.null(wahis_btn_observer)) {
          wahis_btn_observer$destroy()
          wahis_btn_observer <- NULL
        }
        filters <- attr(filtered_wahis(), "inputs")
        # safe_stat here
        erf <- get_wahis_erf(
          disease = filters$disease,
          species = filters$species,
          animal_category = filters$animal_category
        )
        returnDataset(erf)
      })

      observeEvent(input$wahis_cancel,{
        removeModal()
        if (!is.null(wahis_btn_observer)) {
          wahis_btn_observer$destroy()
          wahis_btn_observer <- NULL
        }
      })

      # IMPORT ----
      observeEvent(input$import_open,{
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(modalDialog(
          fluidRow(column(
              width = 10, offset = 1,
              fluidRow(column(
                width = 10, offset = 1,
                tags$h3("Import custom dataset"),
                fileInput(
                  inputId = ns("file"),
                  label = NULL,
                  multiple = FALSE,
                  accept = c("csv", "txt", "tsv"),
                  width = NULL,
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                ),
                div("reactable output"),
                div("Mapping UI"),
                div("Data validation output"),
              )),
            )),
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

        wahis_btn_observer <<- observe({
          valid_selection <- all(nchar(unlist(attr(filtered_wahis(), "inputs"))) > 0)
          if (valid_selection) enable("wahis_apply") else disable(id = "wahis_apply")
        })
      })

      # MANUAL ENTRY ----
      observeEvent(input$manual_open, {
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(modalDialog(
          tags$h3("Set study settings for manual entry")
          textInputIcon(
            inputId = ns("man_disease"),
            label = NULL,
            value = "",
            placeholder = "Disease name",
            icon = icon("virus")
          ),
          textInputIcon(
            inputId = ns("man_species"),
            label = NULL,
            value = "",
            placeholder = "Species name",
            icon = icon("paw")
          ),
          radioButtons(
            inputId = ns("man_animal_category"),
            label = "Animal Category",
            choices = list(Domestic = "Domestic", Wild = "Wild"),
            inline = TRUE
          ),
        footer = list(
          actionButton(
            inputId = ns("import_apply"),
            class = "btn-primary",
            label = "Import",
            disabled = TRUE),
          actionButton(
            inputId = ns("import_cancel"),
            label = "Cancel",
            class = "btn-default"
          )
        ),
        size = "xl",easyClose = TRUE
        ))

        man_btn_observer <<- observe({
          valid_selection <- all(nchar(c(input$man_disease, input$man_species, input$man_animal_category)) > 0)
          if (valid_selection) enable("man_apply") else disable(id = "man_apply")
        })
      })

      observeEvent(input$man_apply,{
        removeModal()
        if (!is.null(man_btn_observer)) {
          man_btn_observer$destroy()
          man_btn_observer <- NULL
        }
        empty_erf <- riskintrodata::wahis_emission_risk_factors[0, ]
        empty_erf$disease <- empty_erf$species <- empty_erf$animal_category <- NULL
        attr(empty_erf, "study_settings") <- c(
          disease = input$man_disease,
          species = input$man_species,
          animal_category = input$man_animal_category
          )
        attr(empty_erf, "table_name") <- "emission_risk_factors"
        returnDataset(empty_erf)
      })
      observeEvent(input$wahis_cancel,{
        removeModal()
        if (!is.null(man_btn_observer)) {
          man_btn_observer$destroy()
          man_btn_observer <- NULL
        }
      })


      returnDataset
    })}

