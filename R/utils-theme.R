#' @importFrom systemfonts register_font
#' @importFrom gdtools font_family_exists
register_fontawesome <- function() {
  if (!font_family_exists("Font Awesome")) {
    register_font(
      name = "Font Awesome",
      plain = system.file(package = "riskintroapp", "www/fonts", "Font-Awesome-Solid-900.otf"),
      bold = system.file(package = "riskintroapp", "www/fonts", "Font-Awesome-Solid-900.otf")
    )
  }
  font_family_exists("Font Awesome")
}

#' @importFrom ggplot2 theme_set theme_void theme element_rect element_text margin
cirad_gg_theme_set <- function() {
  theme_set(
    theme_void(base_size = 11, base_family = "Liberation Sans") +
      theme(
        plot.background = element_rect(
          fill = "transparent",
          color = "transparent"
        ),
        panel.background = element_rect(
          fill = "transparent",
          color = "transparent"
        ),
        plot.title = element_text(
          family = "Liberation Sans",
          size = 16,
          face = "bold",
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          family = "Liberation Sans",
          size = 14,
          face = "bold",
          hjust = 0.5
        ),
        legend.title = element_text(
          family = "Liberation Sans",
          size = 12,
          face = "bold",
          hjust = 0
        ),
        legend.margin = margin(4, 4, 4, 4, unit = "pt"),
        legend.text = element_text(family = "Liberation Sans", size = 9),
        legend.justification = c("right", "top")
      )
  )
}

