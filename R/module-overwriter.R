
#' @importFrom shiny textAreaInput
overwriterUI <- function(id, conf) {
  ns <- NS(id)

  # format table ----
  t_table <- as.data.frame(t(conf$table))
  t_table <- cbind(rownames(t_table), data.frame(t_table, row.names=NULL))
  colnames(t_table) <- c("Variables", "Values")
  t_table$Values <- ifelse(is.na(t_table$Values), " - ", t_table$Values)

  # moadl ----
  modalDialog(
    title = paste0("Override introduction risk in ", conf$eu_name, " (", conf$eu_id, ")"),
    fluidRow(
      column(
        width = 6,
        tags$p("This slider sets a new introduction risk score that will override calculated values."),
        sliderInput(
          inputId = ns("overwrite_risk"),
          label = "Override score",
          min = 0,
          max = 100,
          value = if (isTruthy(conf$overwrite_risk)) conf$overwrite_risk else 0
        ),
        shiny::textAreaInput(
          inputId = ns("overwrite_risk_comm"),
          label = "Add an optional explaination for this risk score:",
          value = if (isTruthy(conf$overwrite_risk_comm)) conf$overwrite_risk_comm else "",
          updateOn = "blur"
        ),
      ),
      column(
        width = 6,
        tags$p(paste0("Current values for ",conf$eu_name, ".")),
        reactable::reactable(t_table)
      )
    ),

    # Modal settings
    size = "xl",
    footer = list(
      actionButton(ns("apply"), class = "btn-primary", "Apply"),
      actionButton(ns("cancel"), class = "btn-light", "Cancel"),
      actionButton(ns("reset"), class = "btn-danger", "Reset overwrite")
    ))
}

overwriterServer <- function(id, clicky, overwriter_data, intro_risk) {
  moduleServer(
    id,
    function(input, output, session) {

      returnList <- reactiveVal(NULL)

      observeEvent(input$apply, {
        out <- list(
          operation = "update",
          eu_id = clicky()$id,
          overwrite_risk = input$overwrite_risk,
          overwrite_risk_comm = input$overwrite_risk_comm
        )
        removeModal()
        returnList(out)
      })

      observeEvent(input$cancel, {
        removeModal()
      })

      observeEvent(input$reset, {
        removeModal()
        out <- list(
          operation = "delete",
          eu_id = clicky()$id
        )
        returnList(out)
      })

      observeEvent(returnList(), {
        overwrite <- returnList()
        if (identical(overwrite$operation, "update")) {

          # initialise table
          if (is.null(overwriter_data())) {
            new_row <-  data.frame(
              eu_id = overwrite$eu_id,
              overwrite_risk = overwrite$overwrite_risk,
              overwrite_risk_comm = overwrite$overwrite_risk_comm
            )
            overwriter_data(new_row)

            # add new data to table
          } else if (!overwrite$eu_id %in% overwriter_data()$eu_id) {
            new_row <-  data.frame(
              eu_id = overwrite$eu_id,
              overwrite_risk = overwrite$overwrite_risk,
              overwrite_risk_comm = overwrite$overwrite_risk_comm
            )
            overwriter_data(rbind(overwriter_data(),new_row))

            # edit data in table
          } else {
            ovw <- overwriter_data()
            ovw[ovw$eu_id %in% overwrite$eu_id, "overwrite_risk"] <- overwrite$overwrite_risk
            ovw[ovw$eu_id %in% overwrite$eu_id, "overwrite_risk_comm"] <- overwrite$overwrite_risk_comm
            overwriter_data(ovw)
          }

          # delete data in table
        } else if (identical(overwrite$operation, "delete")) {
          ovw <- overwriter_data()
          ovw[ovw$eu_id %in% overwrite$eu_id, "overwrite_risk"] <- NA
          ovw[ovw$eu_id %in% overwrite$eu_id, "overwrite_risk_comm"] <- NA
          overwriter_data(ovw)
        }
      })

    })
}
