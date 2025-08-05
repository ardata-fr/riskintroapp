#' @export
#' @importFrom shinyWidgets dropMenu
#' @importFrom shiny NS actionButton
importEmissionRiskFactorsUI <- function(id) {
  ns <- NS(id)
  dropMenu(
    tag = actionButton(
      inputId = ns("dropMenu"),
      label = "Import",
      width = '100%'
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
          label = "Import file",
          style = "width: 200px; margin-bottom: 7px;"
        )
      ),
      div(
        actionButton(
          inputId = ns("manual_open"),
          label = "Manual data entry",
          style = "width: 200px;"
        )
      )
    )
  )
}

#' @export
#' @importFrom shinyWidgets alert panel
#' @importFrom shinyjs enable disable
#' @importFrom datamods select_group_server select_group_ui
#' @importFrom reactable reactable renderReactable reactableOutput
#' @importFrom dplyr select
#' @importFrom riskintrodata get_wahis_erf read_emission_risk_factor_file
#' @importFrom shiny moduleServer reactiveVal renderUI req reactive observeEvent showModal modalDialog fluidRow column uiOutput actionButton observe removeModal reactiveValues renderText fileInput verbatimTextOutput isTruthy textInput radioButtons
#' @importFrom shinyWidgets hideDropMenu
importEmissionRiskFactorsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns

      returnDataset <- reactiveVal(NULL)
      output$existing_dataset_warning <- renderUI({
        req(returnDataset())
        shinyWidgets::alert(
          "Warning: this action will erase the existing emission risk factor dataset",
          status = "danger"
        )
      })

      # WAHIS ------
      filtered_wahis <- select_group_server(
        id = "my_filters",
        data_r = reactive(riskintrodata::wahis_emission_risk_factors),
        vars_r = reactive(c("disease", "species", "animal_category"))
      )

      output$wahis_reactable <- reactable::renderReactable({
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
              reactable::reactableOutput(outputId = ns("wahis_reactable")),
              uiOutput(outputId = ns("existing_dataset_warning"))
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
      importDataset <- reactiveVal()
      import_is_valid <- reactiveValues(valid = NULL, msg = NULL)
      output$import_validation <- renderText({
        import_is_valid$msg
      })
      output$import_reactable <- reactable::renderReactable({
        req(importDataset())
        reactable::reactable(importDataset())
      })
      observeEvent(input$file, ignoreInit = TRUE, ignoreNULL = TRUE, {
        fp <- req(input$file$datapath)
        import_is_valid$valid <- import_is_valid$msg <- NULL
        dataset <- safe_eval({
          riskintrodata::read_emission_risk_factor_file(fp)
        })
        if (is_error(dataset)) {
          import_is_valid$valid <- FALSE
          import_is_valid$msg <- get_error_message(dataset)
          NULL
        } else {
          import_is_valid$valid <- TRUE
          importDataset(dataset)
        }
      })
      observeEvent(input$import_open,{
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(modalDialog(
          fluidRow(column(
              width = 10, offset = 1,
              fluidRow(column(
                width = 10, offset = 1,
                tags$h3("Import file"),
                fileInput(
                  inputId = ns("file"),
                  label = NULL,
                  multiple = FALSE,
                  accept = c("csv", "txt", "tsv"),
                  width = NULL,
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                ),
                reactable::reactableOutput(outputId = ns("import_reactable")),
                verbatimTextOutput(outputId = ns("import_validation")),
                uiOutput(outputId = ns("existing_dataset_warning"))
              )),
            )),
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
          ), size = "xl", easyClose = TRUE
        ))

        import_btn_observer <<- observe({
          if (isTruthy(import_is_valid$valid)) enable("import_apply") else disable(id = "import_apply")
        })
      })
      observeEvent(input$import_cancel,{
        removeModal()
        if (!is.null(import_btn_observer)) {
          import_btn_observer$destroy()
          import_btn_observer <- NULL
        }
      })
      observeEvent(input$import_apply,{
        removeModal()
        if (!is.null(import_btn_observer)) {
          import_btn_observer$destroy()
          import_btn_observer <- NULL
        }
        returnDataset(importDataset())
      })

      # MANUAL ENTRY ----
      observeEvent(input$manual_open, {
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(modalDialog(
          tags$h3("Set study settings for manual entry"),
          textInput(
            inputId = ns("man_disease"),
            label = "Disease",
            value = "",
            placeholder = "Disease name",
            updateOn = "blur"
          ),
          textInput(
            inputId = ns("man_species"),
            label = "Species",
            value = "",
            placeholder = "Species name",
            icon = icon("paw"),
            updateOn = "blur"
          ),
          radioButtons(
            inputId = ns("man_animal_category"),
            label = "Animal Category",
            choices = list(Domestic = "Domestic", Wild = "Wild"),
            inline = TRUE
          ),
          uiOutput(outputId = ns("existing_dataset_warning")),
        footer = list(
          actionButton(
            inputId = ns("man_apply"),
            class = "btn-primary",
            label = "Import",
            disabled = TRUE),
          actionButton(
            inputId = ns("man_cancel"),
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
        attr(empty_erf, "study_settings") <- c(
          disease = input$man_disease,
          species = input$man_species,
          animal_category = input$man_animal_category
          )
        attr(empty_erf, "table_name") <- "emission_risk_factors"
        returnDataset(empty_erf)
      })
      observeEvent(input$man_cancel,{
        removeModal()
        if (!is.null(man_btn_observer)) {
          man_btn_observer$destroy()
          man_btn_observer <- NULL
        }
      })

      returnDataset
    })}

