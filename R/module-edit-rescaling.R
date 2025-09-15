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
#' @importFrom shinyWidgets awesomeCheckbox awesomeRadio
#' @importFrom bslib card layout_column_wrap card_body card_header
#' @importFrom shiny NS modalDialog fluidRow column tags div numericInput actionButton icon plotOutput modalButton
#'
#' @export
rescaleRiskUI <- function(id) {
  ns <- NS(id)

  modalDialog(
    title = titleWithHelpKey("rescale-risk-title"),
    fluidRow(column(
      width = 12,
      div(
        style = "display: flex; align-items: center; gap: 20px;",
        awesomeRadio(
          inputId = ns("method"),
          label = "Transformation function",
          choices = c("linear", "quadratic", "exponential", "sigmoid"),
          selected = "linear"
        ),

        awesomeCheckbox(
          inputId = ns("inverse"),
          label = "Inverse function",
          value = FALSE
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
#'  updateNumericInput observeEvent renderPlot
#'
#' @export
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

      rescale_error <- reactiveVal(NULL)
      output$rescale_error_ui <- renderUI({
        req(rescale_error())
        alert_error(
          text = "Error: unable to rescale dataset",
          error = rescale_error()
        )
      })

      # Used to display the graphs
      rescaledDataset <- reactive({
        req(dataset(), input$method, !is.null(input$inverse))
        safely_rescale_risk_scores <- safely(rescale_risk_scores)
        result <- safely_rescale_risk_scores(
          dataset = dataset(),
          cols = attr(dataset(), "risk_col"),
          from = attr(dataset(), "scale"),
          to = to,
          method = input$method,
          inverse = input$inverse,
          keep_cols = TRUE,
          names_to = "rescaled"
        )
        if (is_error(result$error)) {
          rescale_error(result$error)
          return(NULL)
        } else {
          rescale_error(NULL)
          return(result$result)
        }
      })

      output$points_plot <- renderPlot({
        req(rescaledDataset(), dataset())
        plot_rescaling_line(
          dataset = rescaledDataset(),
          method = input$method,
          inverse = input$inverse,
          initial_scale = attr(dataset(), "scale"),
          initial_column = attr(dataset(), "risk_col")
        )
      })
      output$boxplot <- renderPlot({
        req(rescaledDataset(), dataset())
        plot_rescaling_boxplot(
          dataset = rescaledDataset(),
          method = input$method,
          inverse = input$inverse,
          initial_scale = attr(dataset(), "scale"),
          initial_column = attr(dataset(), "risk_col")
        )
      })

      # Create scaled output
      newRescaleArgs <- reactiveVal(NULL)
      observeEvent(input$apply, {
        removeModal()
        args <- list(
          cols = attr(dataset(), "risk_col"),
          from = attr(dataset(), "scale"),
          to = c(0, 100),
          method = input$method,
          inverse = input$inverse
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
#' @param initial_scale Numeric vector of length 2. Original scale range.
#' @param initial_column Character string. Name of the original risk score column.
#'
#' @return A ggplot object showing transformation curve and data points.
#'
#' @details
#' The plot displays:
#' \itemize{
#'   \item Grey smooth line showing the continuous transformation function
#'   \item Colored points representing actual data positioned on the curve
#'   \item Viridis color scale representing transformed values (0-100)
#'   \item Fixed 1:1 aspect ratio for accurate visual comparison
#' }
#'
#' @importFrom ggplot2
#'  ggplot aes geom_smooth geom_point coord_fixed scale_color_viridis_c xlab ylab theme_minimal
#' @keywords internal
plot_rescaling_line <- function(
    dataset,
    method,
    inverse,
    initial_scale,
    initial_column
){
  scaled_col <- "rescaled"
  line_df <- rescale_risk_scores(
    data.frame(before = seq(from = initial_scale[[1]],
                            to = initial_scale[[2]])),
    cols = "before",
    from = initial_scale,
    to = c(0, 100),
    method = method,
    inverse = inverse,
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
  gg <- gg + geom_point(
      data = dataset,
      aes(
        x = .data[[initial_column]],
        y = .data[[scaled_col]],
        colour = .data[[scaled_col]]
      )
    )
  gg <- gg + ggplot2::scale_color_viridis_c(
    limits = c(0,100),
    direction = -1
  )
  gg <- gg + labs(
      x = "Before rescaling",
      y = "After rescaling"
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
    initial_scale,
    initial_column
){
  scaled_col <- "rescaled"

  # Prepare comparison data
  comparison_data <- data.frame(
    values = c(dataset[[initial_column]], dataset[[scaled_col]]),
    type = rep(c("Before", "After"), each = nrow(dataset))
  )
  comparison_data$type <- factor(comparison_data$type, levels = c("Before", "After"))

  gg <- ggplot2::ggplot(comparison_data, ggplot2::aes(x = .data$type, y = .data$values, fill = .data$type)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.6) +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
    ggplot2::scale_fill_manual(
      values = c("Before" = "#E31A1C", "After" = "#1F78B4"),
      name = ""
    ) +
    ggplot2::labs(
      x = "",
      y = "Risk Score"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text.x = ggplot2::element_text(size = 11, face = "bold")
    )
  gg <- gg + theme(aspect.ratio=1)
  gg
}
