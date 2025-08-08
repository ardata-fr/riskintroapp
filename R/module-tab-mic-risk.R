#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shinyWidgets dropMenu
miscRiskUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Other risks",
      dropMenu(
        arrow = FALSE,
        tag = actionButton(
          inputId = ns("dropMenu"),
          label = "Import",
          width = '100%'
        ),
        placement = "right",
        tagList(
          div(
            actionButton(
              inputId = ns("open_raster"),
              label = "From raster file",
              style = "width: 200px; margin-bottom: 7px;"
            )
          ),
          div(
            actionButton(
              inputId = ns("open_precalc"),
              label = "Precalculated risk",
              style = "width: 200px; margin-bottom: 7px;"
            )
          )
        )
      ),
      selectInput(
        inputId = ns("select_risk"),
        label = "Select current risk",
        choices = character(0L)
      ),
      actionButton(
        inputId = ns("delete_risk"),
        label = "Remove current risk"
      ),
      actionButton(
        inputId = ns("open_risk_scaling"),
        label = "Edit risk scaling"
      ),
    ),
    navset_card_tab(
      id = ns("panel_ui"),
      nav_panel(
        title = "Map view",
        leafletOutput(ns("map"), width = "100%", height = "85vh")
      ),
      nav_panel(
        title = "Table view",
        reactableOutput(outputId = ns("table"))
      )
    )
  )
}

#' @importFrom sf st_drop_geometry
#' @importFrom leaflet
#'  renderLeaflet leafletProxy addPolygons
#' @importFrom reactable
#'  reactable renderReactable
miscRiskServer <- function(id, epi_units) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      miscRiskTable <- reactiveVal(NULL)

      # Initialise maps ----
      baseMap <- reactive({basemap()})
      output$map <- renderLeaflet({
        req(baseMap())
        baseMap()
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)


      # Init risk table ----
      observeEvent(epi_units(),{
        req(epi_units())
        init_risk_table <- risk_table(
          epi_units(),
          scale = c(0, 100)
        )
        miscRiskTable(init_risk_table)
        ll <- leafletProxy(mapId = "map")
        labels <- generate_leaflet_labels(init_risk_table)
        leaflet::addPolygons(
          ll, data = init_risk_table, label = labels
        )
      })

      # table ----
      output$table <- renderReactable({
        mrt <- req(miscRiskTable())
        mrt <- sf::st_drop_geometry(mrt)
        mrt$eu_id <- NULL
        reactable::reactable(mrt)
      })

      # Import raster ----
      observeEvent(input$open_raster, {
        showModal(modalDialog(
          fluidRow(column(
            width = 10, offset = 1,

            div("Not yet implemented")

          )),
          footer = list(
            actionButton(
              inputId = ns("apply_raster"),
              class = "btn-primary",
              label = "Import",
              disabled = TRUE),
            actionButton(
              inputId = ns("cancel_raster"),
              label = "Cancel",
              class = "btn-default"
            )
          ), size = "xl", easyClose = TRUE
        ))
      })


      # Import precalc -----


      importData <- reactive({
        fp <- req(input$precalc_file$datapath)
        safely_read_delim <- safely(readr::read_delim)
        safely_read_delim(fp)
      })
      observeEvent(importData(), ignoreNULL = TRUE{
        updateSelectInput(
          inputId= "join_by",
          choices = colnames(importData())
        )
      })
      observeEvent(importData(), ignoreNULL = TRUE{
        updateSelectInput(
          inputId= "join_by",
          choices = colnames(importData())
        )
      })
      observe({
        req(importData(), input$join_by)
        rt <- req(miscRiskTable())
        riskintroanalysis::add_risk(
          risk_table = rt,
          risk_data = importData(),
          cols = input$risk_cols,
          scale =
          )
      })

      observeEvent(input$open_precalc, {
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(modalDialog(
          fluidRow(column(
            width = 10, offset = 1,
            title = "Import precalculated risk file",
            tags$p("Import csv, txt, excel, or parqet file."),
            tags$p("This file should be joinable to epidemioligcal units file using a unique identifier."),
            fileInput(
              inputId = ns("precalc_file"),
              label = NULL,
              multiple = TRUE,
              accept = c(
                ".csv", ".txt"
              ),
              width = "100%"
            ),
            textInput(
              inputId = ns("precalc_name"),
              label = "Risk name"
            ),
            selectInput(
              inputId = ns("risk_cols"),
              label = "Select risk columns to import",
              choices = character(),
              multiple = TRUE
            ),
            selectInput(
              inputId = ns("join_by"),
              label = "Select identifier column",
              choices = character(),
              multiple = FALSE
            ),
            div(
              tags$text("Select the initial risk scale"),
              numericInput(
                inputId = ns("from"),
                label = "from",
                value = 0, width = "75px",
              ),
              numericInput(
                inputId = ns("to"),
                label = "to",
                value = NULL, width = "75px",
              ),
            )
          )),
          footer = list(
            actionButton(
              inputId = ns("apply_precalc"),
              class = "btn-primary",
              label = "Import",
              disabled = TRUE),
            actionButton(
              inputId = ns("cancel_precalc"),
              label = "Cancel",
              class = "btn-default"
            )
          ), size = "xl", easyClose = TRUE
        ))
      })

      # edit risk scaling -----
      observeEvent(input$open_risk_scaling, {
        hideDropMenu(id = "dropMenu_dropmenu")
        showModal(modalDialog(
          fluidRow(column(
            width = 10, offset = 1,

            div("Not yet implemented")

          )),
          footer = list(
            actionButton(
              inputId = ns("apply_rescale"),
              class = "btn-primary",
              label = "Import",
              disabled = TRUE),
            actionButton(
              inputId = ns("cancel_rescale"),
              label = "Cancel",
              class = "btn-default"
            )
          ), size = "xl", easyClose = TRUE
        ))
      })


      observeEvent(input$dropMenu, ignoreNULL = TRUE,{
        if(!isTruthy(miscRiskTable())) {
          hideDropMenu(id = "dropMenu_dropmenu")
          show_alert(
            title = "Epidemiological units not initialised",
            text = "Epidemiological units need to be added to your workspace before these risks can be imported. You can do this in the sidebar of the Epidemiological units tab."
          )
        }
      })



    }
  )
}
