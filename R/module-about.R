#' @title About Module UI
#' @description
#' Module for displaying information about the application.
#' @param id The module id.
#' @noRd
#' @importFrom utils packageVersion
aboutUI <- function(id = "about") {
  ns <- NS(id)

  si <- sessionInfo()

  packages_loaded <- grep("^riskintro", si$loadedOnly |> names(), value = TRUE)
  packages_attached <- grep("^riskintro", si$otherPkgs |> names(), value = TRUE)
  packages <- c(packages_loaded, packages_attached)
  packages <- c(
    "base",
    sort(
      c(
        "workspace",
        packages
      )
    )
  )
  packages <- unique(packages)
  versions <- lapply(packages, function(x) {
    tags$li(paste(x, "-", packageVersion(x)))
  })
  versions <- do.call(tags$ul, versions)

  div(
    class = "about-bar",
    fluidRow(
      column(
        width = 12,
        versions
      )
    )
  )
}

