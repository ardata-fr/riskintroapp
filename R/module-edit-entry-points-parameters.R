#' Entry Points Parameters Module UI
#'
#' Creates a modal dialog interface for configuring entry point risk calculation
#' parameters with real-time interactive visualization using the univariate method.
#'
#' @param id Character string. Module namespace identifier.
#' @param saved_params List of saved parameters to restore
#'
#' @return A modal dialog UI element with parameter controls and visualization panels.
#'
#' @details
#' The UI provides:
#' \itemize{
#'   \item Parameter sliders for coef_legal (α), coef_illegal (β), illegal_factor (λ)
#'   \item max_risk (m) is disabled - scaling is handled by rescale_risk_scores()
#'   \item Real-time parameter value display with mathematical notation
#'   \item 2x2 visualization grid: bivariate heatmap, 3D surface, equivalent points curve, example transform
#'   \item Apply and Close action buttons
#' }
#'
#' @importFrom bslib card layout_column_wrap card_body card_header
#' @importFrom shiny NS modalDialog fluidRow column tags div sliderInput actionButton icon modalButton
#' @importFrom plotly plotlyOutput
#' @importFrom shinyjs disabled
#'
#' @keywords internal
entryPointsParametersUI <- function(id, saved_params) {
  ns <- NS(id)

  modalDialog(
    title = titleWithHelpButton(
      key = "entry-points-parameters-title",
      ns = ns,
      help_url = "https://astre.gitlab.cirad.fr/riskintro-app/riskintroanalysis/articles/entry-points-analysis.html"
    ),
    fluidRow(
      # Left sidebar with controls
      column(
        width = 3,
        div(
          style = "padding-right: 15px;",

          # # Parameter controls ----
          # shinyjs::disabled(
          #   numericInput(
          #     inputId = ns("max_risk"),
          #     label = "Maximum Risk (m):",
          #     value = 100
          #   )
          # ),
          sliderInput(
            inputId = ns("coef_legal"),
            label = "Legal Coefficient (\u03b1):",
            value = saved_params$coef_legal,
            min = 0.1,
            max = 10,
            step = 0.1
          ),
          sliderInput(
            inputId = ns("coef_illegal"),
            label = "Illegal Coefficient (\u03b2):",
            value = saved_params$coef_illegal,
            min = 0.1,
            max = 10,
            step = 0.1
          ),
          sliderInput(
            inputId = ns("illegal_factor"),
            label = "Illegal Factor (\u03bb):",
            value = saved_params$illegal_factor,
            min = 1.1,
            max = 10,
            step = 0.1
          ),

          # Parameter display with mathematical notation ----
          div(
            class = "parameter-display",
            style = "display: grid; grid-template-columns: 1fr; gap: 5px; margin: 15px 0; padding: 10px; background: #f8f9fa; border-radius: 5px; font-family: 'Courier New', monospace;",
            # div(tags$strong("m = "), textOutput(ns("m_display"), inline = TRUE)),
            div(tags$strong("\u03b1 = "), textOutput(ns("alpha_display"), inline = TRUE)),
            div(tags$strong("\u03b2 = "), textOutput(ns("beta_display"), inline = TRUE)),
            div(tags$strong("\u03bb = "), textOutput(ns("lambda_display"), inline = TRUE))
          ),

          # Mathematical equation display -----
          div(
            class = "math-container",
            style = "background: white; padding: 10px; border-radius: 5px; margin: 10px 0; text-align: center; border: 1px solid #dee2e6;",
            tags$p(
              style = "margin-bottom: 5px; font-weight: bold; font-size: 12px;",
              "Univariate Conversion:"
            ),
            div(
              style = "font-size: 11px; font-family: 'Times New Roman', serif; line-height: 1.2;",
              "x̂", tags$sub("u"), " = s", tags$sup("-1"), "(s(\u03b2 \u00b7 x", tags$sub("c"), ") / \u03bb) / \u03b1"
            ),
            tags$br(),
            div(
              style = "font-size: 11px; font-family: 'Times New Roman', serif; line-height: 1.2;",
              "x", tags$sub("total"), " = x", tags$sub("u"), " + x̂", tags$sub("u")
            ),
            tags$br(),
            div(
              style = "font-size: 10px; color: #6c757d; line-height: 1.1;",
              "where s(x) = (1 - e", tags$sup("-x"), ") / (1 + e", tags$sup("-x"), ")"
            )
          )
        )
      ),

      # Right side with data visuals grid ----
      column(
        width = 9,
        card(
          full_screen = FALSE,
          card_header("Equivalent Uncontrolled Points"),
          card_body(
            class = "p-0",
            plotlyOutput(ns("equivalent_plot"), height = "600px")
          )
        )
      )
        # layout_column_wrap(
        #   width = 1/2,
        #   height = 700,
        #   fill = TRUE,
        #
        #   card(
        #     full_screen = FALSE,
        #     card_header("2D Heatmap"),
        #     card_body(
        #       class = "p-0",
        #       plotlyOutput(ns("heatmap_plot"), height = "300px")
        #     )
        #   ),
        #   card(
        #     full_screen = FALSE,
        #     card_header("3D Surface"),
        #     card_body(
        #       class = "p-0",
        #       plotlyOutput(ns("surface_plot"), height = "300px")
        #     )
        #   ),
        #   card(
        #     full_screen = FALSE,
        #     card_header("Equivalent Uncontrolled Points"),
        #     card_body(
        #       class = "p-0",
        #       plotlyOutput(ns("equivalent_plot"), height = "300px")
        #     )
        #   ),
        #   card(
        #     full_screen = FALSE,
        #     card_header("Example Risk Transform"),
        #     card_body(
        #       class = "p-0",
        #       plotlyOutput(ns("scaling_plot"), height = "300px")
        #     )
        #   )
        # )
    ),
    footer = list(
      actionButton(
        inputId = ns("apply"),
        class = "btn-primary",
        label = "Apply Parameters",
        icon = icon("check")
      ),
      modalButton(label = "Close")
    ),
    size = "xl",
    easyClose = FALSE
  )
}

#' Entry Points Parameters Module Server
#'
#' Server logic for entry point risk parameter configuration with interactive
#' visualization and real-time parameter updates using the univariate method.
#'
#' @param id Character string. Module namespace identifier.
#'
#' @return Reactive expression returning a list with entry point parameters:
#' \itemize{
#'   \item \code{max_risk}: Fixed at 12 (for legacy compatibility)
#'   \item \code{coef_legal}: Controlled entry points coefficient (α)
#'   \item \code{coef_illegal}: Uncontrolled entry points coefficient (β)
#'   \item \code{illegal_factor}: Relative risk factor for uncontrolled vs controlled points (λ)
#' }
#'
#' @details
#' The server function:
#' \itemize{
#'   \item Provides real-time visualization of parameter effects on conversion
#'   \item Updates mathematical displays with current parameter values
#'   \item Generates interactive plots using plotly
#'   \item Returns parameter configuration for calc_entry_point_risk()
#'   \item Risk scaling (0-100) is applied separately via rescale_risk_scores()
#' }
#'
#' @importFrom riskintroanalysis scale_entry_points scale_controlled sigmoid
#' @importFrom plotly plot_ly add_surface add_trace layout renderPlotly
#' @importFrom shiny moduleServer reactive renderText observeEvent removeModal
#'
#' @keywords internal
entryPointsParametersServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Parameter displays ----
      # output$m_display <- renderText({
      #   req(input$max_risk)
      #   as.character(input$max_risk)
      # })

      output$alpha_display <- renderText({
        req(input$coef_legal)
        sprintf("%.2f", input$coef_legal)
      })

      output$beta_display <- renderText({
        req(input$coef_illegal)
        sprintf("%.2f", input$coef_illegal)
      })

      output$lambda_display <- renderText({
        req(input$illegal_factor)
        sprintf("%.2f", input$illegal_factor)
      })

      # Generate data for visualizations ----
      heatmap_data <- reactive({
        req(
          # input$max_risk,
          input$coef_legal, input$coef_illegal, input$illegal_factor)
        prepare_heatmap_data(
          max_risk = 100,
          coef_legal = input$coef_legal,
          coef_illegal = input$coef_illegal,
          illegal_factor = input$illegal_factor
        )
      })

      equivalent_data <- reactive({
        req(input$coef_legal, input$coef_illegal, input$illegal_factor)
        prepare_equivalent_points_data(
          coef_legal = input$coef_legal,
          coef_illegal = input$coef_illegal,
          illegal_factor = input$illegal_factor
        )
      })

      scaling_data <- reactive({
        req(
          # input$max_risk,
          input$coef_illegal)
        prepare_scaling_curve_data(
          max_risk = 100,
          coef_illegal = input$coef_illegal
        )
      })

      # Render plots ----
      output$heatmap_plot <- renderPlotly({
        data <- heatmap_data()
        plot_heatmap_plot(data)
      })

      output$surface_plot <- renderPlotly({
        data <- heatmap_data()
        plot_surface_plot(data)
      })

      output$equivalent_plot <- renderPlotly({
        data <- equivalent_data()
        plot_equivalent_points_plot(data)
      })

      output$scaling_plot <- renderPlotly({
        data <- scaling_data()
        plot_scaling_curve_plot(data)
      })

      # Return parameters when Apply is clicked
      parameters <- reactiveVal(NULL)
      observeEvent(input$apply, {
        params <- list(
          max_risk = 100,
          coef_legal = input$coef_legal,
          coef_illegal = input$coef_illegal,
          illegal_factor = input$illegal_factor
        )
        parameters(params)
        removeModal()
      })

      return(parameters)
    }
  )
}

# Helper functions for data generation ----

#' Prepare Heatmap Data
#' @param max_risk Maximum risk value
#' @param coef_legal Legal coefficient
#' @param coef_illegal Illegal coefficient
#' @param illegal_factor Illegal factor
#' @noRd
prepare_heatmap_data <- function(max_risk, coef_legal, coef_illegal, illegal_factor) {
  x_legal <- seq(0, 4, length.out = 40)
  x_illegal <- seq(0, 4, length.out = 40)

  # Create matrices for plotly
  z_matrix <- outer(x_legal, x_illegal, function(xl, xi) {
    riskintroanalysis::scale_entry_points(
      x_legal = xl,
      x_illegal = xi,
      illegal_factor = illegal_factor,
      coef_legal = coef_legal,
      coef_illegal = coef_illegal,
      max_risk = max_risk
    )
  })

  list(
    x = x_legal,
    y = x_illegal,
    z = z_matrix
  )
}

#' Prepare Equivalent Points Data
#' @param coef_legal Legal coefficient (alpha)
#' @param coef_illegal Illegal coefficient (beta)
#' @param illegal_factor Illegal factor (lambda)
#' @noRd
prepare_equivalent_points_data <- function(coef_legal, coef_illegal, illegal_factor) {
  x_legal <- seq(0, 4, length.out = 100)

  # Calculate equivalent uncontrolled points using scale_controlled
  equivalent_uncontrolled <- riskintroanalysis::scale_controlled(
    x = x_legal,
    alpha = coef_legal,
    beta = coef_illegal,
    lambda = illegal_factor
  )

  list(x = x_legal, y = equivalent_uncontrolled)
}

#' Prepare Scaling Curve Data
#'
#' Shows an example sigmoid transformation of total equivalent uncontrolled exposure.
#' This is for visualization only - actual scaling is done via rescale_risk_scores().
#'
#' @param max_risk Maximum risk value
#' @param coef_illegal Coefficient for visualization (used as alpha in sigmoid)
#' @noRd
prepare_scaling_curve_data <- function(max_risk, coef_illegal) {
  # x represents total equivalent uncontrolled exposure
  x_total <- seq(0, 4, length.out = 100)

  # Show example sigmoid transformation (what rescale_risk_scores might apply)
  # Note: Actual transformation is chosen by user in the risk scaling module
  y <- 100 * riskintroanalysis::sigmoid(coef_illegal * x_total)

  list(x = x_total, y = y)
}



# Helper functions for data generation ----

#' Plot Heatmap data
#' @param data Heatmap data from prepare_heatmap_data
#' @noRd
plot_heatmap_plot <- function(data) {
  plotly::plot_ly(
    x = data$x,
    y = data$y,
    z = data$z,
    type = "heatmap",
    colorscale = "Viridis",
    showscale = TRUE
  ) |>
    plotly::layout(
      xaxis = list(title = "Legal Entry Points (x_c)", range = c(0, 4)),
      yaxis = list(title = "Illegal Entry Points (x_u)", range = c(0, 4)),
      margin = list(t = 30, r = 50, b = 50, l = 60)
    )
}

#' Plot Surface data
#' @param data Heatmap data from prepare_heatmap_data
#' @noRd
plot_surface_plot <- function(data) {
  plotly::plot_ly(
    x = data$x,
    y = data$y,
    z = data$z,
    type = "surface",
    colorscale = "Viridis",
    showscale = FALSE
  ) |>
    plotly::layout(
      scene = list(
        xaxis = list(title = "Legal Entry Points (x_c)", range = c(0, 4)),
        yaxis = list(title = "Illegal Entry Points (x_u)", range = c(0, 4)),
        zaxis = list(title = "Risk Score"),
        camera = list(
          eye = list(x = 1.5, y = -1.5, z = 1)
        ),
        aspectratio = list(x = 1, y = 1, z = 0.7)
      ),
      margin = list(t = 10, r = 10, b = 10, l = 10)
    )
}

#' Plot Equivalent Points data
#' @param data Equivalent points data from prepare_equivalent_points_data
#' @noRd
plot_equivalent_points_plot <- function(data) {
  plotly::plot_ly(
    x = data$x,
    y = data$y,
    type = "scatter",
    mode = "lines",
    line = list(color = "rgb(31, 119, 180)", width = 3),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = list(title = "Controlled Entry Points (x_c)", range = c(0, 4)),
      yaxis = list(title = "Equiv. Uncontrolled Points (x̂_u)"),
      margin = list(t = 10, r = 10, b = 50, l = 60)
    )
}

#' Plot Scaling Curve data
#' @param data Scaling curve data from prepare_scaling_curve_data
#' @noRd
plot_scaling_curve_plot <- function(data) {
  plotly::plot_ly(
    x = data$x,
    y = data$y,
    type = "scatter",
    mode = "lines",
    line = list(color = "rgb(214, 39, 40)", width = 3),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = list(title = "Total Equiv. Uncontrolled (x_total)", range = c(0, 4)),
      yaxis = list(title = "Example Sigmoid Transform"),
      margin = list(t = 10, r = 10, b = 50, l = 60)
    )
}
