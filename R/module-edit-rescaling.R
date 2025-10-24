#' Rescale Risk Scores Module UI
#'
#' Creates a modal dialog interface for risk score rescaling with transformation
#' method selection and real-time visualization of scaling effects.
#'
#' @param id Character string. Module namespace identifier.
#'
#' @return A modal dialog UI element with transformation controls and visualization panels.
#'
#' @details
#' The UI provides:
#' \itemize{
#'   \item Radio buttons for transformation method selection (linear, quadratic, exponential, sigmoid)
#'   \item Checkbox for inverse transformation option
#'   \item Two visualization panels: transformation curve and boxplot comparison
#'   \item Apply and Close action buttons
#' }
#'
#' @importFrom shinyWidgets awesomeCheckbox awesomeRadio numericRangeInput
#' @importFrom bslib card layout_column_wrap card_body card_header
#' @importFrom shiny NS modalDialog fluidRow column tags div actionButton icon plotOutput modalButton
#'
#' @keywords internal
rescaleRiskUI <- function(id, rescaling_args) {
  ns <- NS(id)

  modalDialog(
    title = div(
      div(
        style = "display: inline-flex; align-items: center; gap: 8px;",
        h4(get_label("rescale-risk-title", "en"), style = "margin: 0;"),
        actionLink(
          inputId = ns("help_link"),
          label = NULL,
          icon = icon("circle-question"),
          style = "font-size: 1.2em; color: #0d6efd;"
        )
      ),
    ),
    fluidRow(column(
      width = 12,
      tags$p(
        HTML(get_help("rescale-risk-title", "en"))
      ),
      div(
        style = "display: flex; align-items: center; gap: 20px;",
        awesomeRadio(
          inputId = ns("method"),
          label = "Transformation function",
          choices = c("linear", "quadratic", "exponential", "sigmoid"),
          selected = rescaling_args$method
        ),

        div(
          style = "display: flex; flex-direction: column; gap: 10px;",
          awesomeCheckbox(
            inputId = ns("inverse"),
            label = "Inverse function",
            value = rescaling_args$inverse
          ),
          awesomeCheckbox(
            inputId = ns("reverse"),
            label = "Reverse risk scale",
            value = rescaling_args$reverse
          )
        ),

        div(
          style = "display: flex; flex-direction: column; gap: 10px;",
          numericRangeInput(
            inputId = ns("to_range"),
            label = "Target scale range",
            value = rescaling_args$to,
            min = 0,
            max = 100,
            step = 1,
            width = "180px"
          )
        )
      ),

      uiOutput(ns("rescale_error_ui")),

      layout_column_wrap(
        width = 1/2,
        height = 450,
        fill = FALSE,

        bslib::card(
          full_screen = FALSE,
          bslib::card_header("Transformation visualisation"),
          bslib::card_body(
            class = "p-0",
            plotOutput(ns("points_plot"))
          )
        ),
        bslib::card(
          full_screen = FALSE,
          bslib::card_header("Boxplot comparison"),
          bslib::card_body(
            class = "p-0",
            plotOutput(ns("boxplot"))
          )
        )
      ),
    )),
    footer = list(
      uiOutput(ns("config_is_valid")),
      actionButton(
        inputId = ns("apply"),
        class = "btn-primary",
        label = "Apply",
        disabled = FALSE),
      modalButton(label = "Close")
    ),
    size = "xl",
    easyClose = TRUE

  )
}

#' Rescale Risk Scores Module Server
#'
#' Server logic for risk score rescaling with interactive transformation method
#' selection and real-time visualization of scaling effects.
#'
#' @param id Character string. Module namespace identifier.
#' @param dataset Reactive expression returning a data frame with risk scores.
#'   Dataset must have 'scale' and 'risk_col' attributes set.
#' @param risk_col Character string or NULL. Risk column name (automatically
#'   detected from dataset attributes if NULL).
#' @param from Numeric vector of length 2 or NULL. Initial scale range
#'   (automatically detected from dataset attributes if NULL).
#' @param to Numeric vector of length 2. Target scale range. Defaults to c(0, 100).
#'
#' @return Reactive expression returning a list with rescaling parameters:
#' \itemize{
#'   \item \code{cols}: Risk column name
#'   \item \code{from}: Original scale range
#'   \item \code{to}: Target scale range
#'   \item \code{method}: Transformation method ("linear", "quadratic", "exponential", "sigmoid")
#'   \item \code{inverse}: Logical indicating if inverse transformation should be applied
#' }
#'
#' @details
#' The server function:
#' \itemize{
#'   \item Automatically detects scale and risk column from dataset attributes
#'   \item Provides real-time visualization of transformation effects
#'   \item Handles transformation errors gracefully with user feedback
#'   \item Returns rescaling parameters for use in risk calculation pipeline
#' }
#'
#' @importFrom riskintroanalysis rescale_risk_scores
#' @importFrom shiny
#'  moduleServer reactiveValues observe reactive req isTruthy eventReactive
#'  updateNumericInput observeEvent renderPlot actionLink
#' @importFrom shinyjs runjs
#'
#' @keywords internal
rescaleRiskServer <- function(
    id,
    dataset,
    risk_col = NULL,
    from = NULL,
    to = c(0, 100)
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Help button ----
      observeEvent(input$help_link, {
        url <- "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/risk-scaling.html"
        shinyjs::runjs(paste0("window.open('", url, "', 'help', 'width=1200,height=800,scrollbars=yes,resizable=yes');"))
      })


      configIsValid <- reactive({

        if (!isTruthy(dataset())) {
          status <- build_config_status(
            value = FALSE,
            msg = "No dataset found."
          )
          return(status)
        }

        if (!isTruthy(input$method)) {
          status <- build_config_status(
            value = FALSE,
            msg = "No rescaling method found."
          )
          return(status)
        }

        if (is.null(input$inverse)) {
          status <- build_config_status(
            value = FALSE,
            msg = "No value for inverse parameter found."
          )
          return(status)
        }

        if (is.null(input$reverse)) {
          status <- build_config_status(
            value = FALSE,
            msg = "No value for reverse parameter found."
          )
          return(status)
        }

        range_val <- input$to_range
        if (range_val[1] >= range_val[2]) {
          status <- build_config_status(
            value = FALSE,
            msg = "Target minimum must be less than target maximum."
          )
          return(status)
        }

        # Validate that target range is within risk_table scale (0-100)
        if (range_val[1] < 0 || range_val[2] > 100) {
          status <- build_config_status(
            value = FALSE,
            msg = "Target range must be within 0-100."
          )
          return(status)
        }

        if (!isTruthy(rescaledDatasetRes())) {
          status <- build_config_status(
            value = FALSE,
            msg = "Unable to rescale dataset."
          )
          return(status)
        }

        if (is_error(rescaledDatasetRes()$error)) {
          status <- build_config_status(
            value = FALSE,
            msg = "Error while rescaling:",
            error = rescaledDatasetRes()$error
          )
          return(status)
        }

        build_config_status(
          value = TRUE,
          msg = "Configuration is valid."
        )
      })
      output$config_is_valid <- renderUI({
        report_config_status(configIsValid(), in_panel = FALSE)
      })

      # Used to display the graphs
      rescaledDatasetRes <- reactive({
        req(input$to_range, input$method, dataset())
        req(!is.null(input$inverse), !is.null(input$reverse))
        safely_rescale_risk_scores <- safely(rescale_risk_scores)
        result <- safely_rescale_risk_scores(
          dataset = dataset(),
          cols = attr(dataset(), "risk_col"),
          from = attr(dataset(), "scale"),
          to = input$to_range,
          method = input$method,
          inverse = input$inverse,
          reverse = input$reverse,
          keep_cols = TRUE,
          names_to = "rescaled"
        )
      })


      rescaledDataset <- reactive({
        req(rescaledDatasetRes)
        if (is_error(rescaledDatasetRes()$error)) {
          return(NULL)
        } else {
          rescaledDatasetRes()$result
        }
      })

      output$points_plot <- renderPlot({
        req(rescaledDataset())
        plot_rescaling_line(
          dataset = rescaledDataset(),
          method = input$method,
          inverse = input$inverse,
          reverse = input$reverse,
          initial_scale = attr(dataset(), "scale"),
          initial_column = attr(dataset(), "risk_col"),
          target_scale = input$to_range
        )
      })
      output$boxplot <- renderPlot({
        req(rescaledDataset())
        plot_rescaling_boxplot(
          dataset = rescaledDataset(),
          method = input$method,
          inverse = input$inverse,
          reverse = input$reverse,
          initial_scale = attr(dataset(), "scale"),
          initial_column = attr(dataset(), "risk_col"),
          target_scale = input$to_range
        )
      })


      observe({
        if (configIsValid()) {
          shinyjs::enable("apply")
        } else {
          shinyjs::disable("apply")
        }
      })

      # Create scaled output
      newRescaleArgs <- reactiveVal(NULL)
      observeEvent(input$apply, {
        removeModal()
        args <- list(
          cols = attr(dataset(), "risk_col"),
          from = attr(dataset(), "scale"),
          to = input$to_range,
          method = input$method,
          inverse = input$inverse,
          reverse = input$reverse
        )
        newRescaleArgs(args)
      })

      return(newRescaleArgs)
    })
}


#' Plot Risk Score Transformation Curve
#'
#' Creates a scatter plot showing the mathematical transformation function
#' with actual data points overlaid to visualize scaling effects.
#'
#' @param dataset Data frame containing risk scores with rescaled column.
#' @param method Character string. Transformation method used ("linear", "quadratic", "exponential", "sigmoid").
#' @param inverse Logical. Whether inverse transformation was applied.
#' @param reverse Logical. Whether risk scale was reversed.
#' @param initial_scale Numeric vector of length 2. Original scale range.
#' @param initial_column Character string. Name of the original risk score column.
#' @param target_scale Numeric vector of length 2. Target scale range. Defaults to c(0, 100).
#'
#' @return A ggplot object showing transformation curve and data points.
#'
#' @details
#' The plot displays:
#' \itemize{
#'   \item Grey smooth line showing the continuous transformation function
#'   \item Jittered colored points representing actual data with slight random displacement to avoid overlap
#'   \item Alpha transparency (0.6) for better visualization of overlapping points
#'   \item Viridis color scale representing transformed values
#'   \item Fixed 1:1 aspect ratio for accurate visual comparison
#' }
#'
#' @importFrom ggplot2
#'  ggplot aes geom_smooth geom_jitter coord_fixed scale_color_viridis_c xlab ylab theme_minimal
#' @keywords internal
plot_rescaling_line <- function(
    dataset,
    method,
    inverse,
    reverse,
    to,
    initial_scale,
    initial_column,
    target_scale = c(0, 100)
){
  scaled_col <- "rescaled"
  line_df <- rescale_risk_scores(
    data.frame(before = seq(from = initial_scale[[1]],
                            to = initial_scale[[2]])),
    cols = "before",
    from = initial_scale,
    to = target_scale,
    method = method,
    inverse = inverse,
    reverse = reverse,
    names_to = "after",
    keep_cols = TRUE
  )

  gg <- ggplot()
  gg <- gg + geom_smooth(
      data = line_df,
      aes(
        x = .data$before,
        y = .data$after
      ), color = "grey",
      method = 'loess',
      formula = 'y ~ x',
      se = FALSE
    )
  gg <- gg + geom_jitter(
      data = dataset,
      aes(
        x = .data[[initial_column]],
        y = .data[[scaled_col]],
        colour = .data[[scaled_col]]
      ),
      alpha = 0.6,
      width = diff(initial_scale) * 0.01,
      height = 1
    )
  gg <- gg + ggplot2::scale_color_viridis_c(
    limits = c(0, 100),
    direction = -1
  )
  gg <- gg + ggplot2::scale_y_continuous(
    limits = c(0, 100)
  )
  gg <- gg + labs(
      x = "Original",
      y = "Rescaled"
    )
  gg <- gg + theme_minimal()
  gg <- gg + theme(aspect.ratio=1)
  gg
}

#' Plot Risk Score Boxplot Comparison
#'
#' Creates side-by-side boxplots comparing risk score distributions
#' before and after transformation.
#'
#' @param dataset Data frame containing risk scores with rescaled column.
#' @param method Character string. Transformation method used ("linear", "quadratic", "exponential", "sigmoid").
#' @param inverse Logical. Whether inverse transformation was applied.
#' @param reverse Logical. Whether risk scale was reversed.
#' @param initial_scale Numeric vector of length 2. Original scale range.
#' @param initial_column Character string. Name of the original risk score column.
#'
#' @return A ggplot object showing before/after boxplot comparison.
#'
#' @details
#' The plot displays:
#' \itemize{
#'   \item Side-by-side boxplots for "Before" (red) and "After" (blue) distributions
#'   \item Jittered points overlaid on boxplots showing individual data points
#'   \item Statistical summary including quartiles, median, and outliers
#'   \item Clear visual comparison of distribution changes
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter scale_fill_manual labs theme_minimal theme element_text
#' @keywords internal
plot_rescaling_boxplot <- function(
    dataset,
    method,
    inverse,
    reverse,
    initial_scale,
    initial_column,
    target_scale
){
  scaled_col <- "rescaled"

  # Prepare comparison data
  comparison_data <- data.frame(
    values = c(dataset[[initial_column]], dataset[[scaled_col]]),
    type = rep(c("Original", "Rescaled"), each = nrow(dataset))
  )
  comparison_data$type <- factor(comparison_data$type, levels = c("Original", "Rescaled"))


  # Used to ensure limits of axes are shown properly.
  facet_lims <- data.frame(
    values = c(initial_scale, c(0, 100)),
    type = factor(rep(c("Original", "Rescaled"), each = 2))
  )

  gg <- ggplot2::ggplot(
    comparison_data,
    ggplot2::aes(y = .data$values, fill = .data$type)
  ) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.6) +
    ggplot2::scale_fill_manual(
      values = c("Original" = "#E31A1C", "Rescaled" = "#1F78B4"),
      name = ""
    ) +
    ggplot2::geom_blank(data = facet_lims, ggplot2::aes(y = .data$values, fill = .data$type)) +
    ggplot2::labs(
      x = "",
      y = "Risk Score"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text.x = element_blank(), axis.ticks.x = element_blank()
    ) +
    ggplot2::facet_wrap(~.data$type, scales = "free_y")

  gg <- gg + theme(aspect.ratio=1)
  gg
}
