#' Interactive Entry Points Editor Module UI
#'
#' Creates an interactive editor that allows users to click on the map to create
#' or edit entry points.
#'
#' @param id Character string. Module namespace identifier.
#'
#' @return UI elements for the interactive editor
#'
#' @importFrom bslib card card_header card_body
#' @importFrom shiny NS modalDialog fluidRow column textInput actionButton modalButton icon
#' @importFrom shinyWidgets pickerInput
#'
#' @keywords internal
interactiveEntryPointsEditorUI <- function(id) {
  ns <- NS(id)
  # This module doesn't render its own UI - it manages modals
  tagList()
}

#' Interactive Entry Points Editor Module Server
#'
#' Server logic for interactive entry point editing via map clicks.
#' Integrates with the main entry points module to provide click-to-edit functionality.
#'
#' @param id Character string. Module namespace identifier.
#' @param input_data Reactive containing current entry points dataset
#' @param emission_scores Reactive containing emission risk scores for source country selection
#' @param map_proxy Leaflet proxy object for the map
#' @param map_click Reactive containing map click events
#' @param map_marker_click Reactive containing marker click events
#'
#' @return Reactive expression returning updated entry points dataset
#'
#' @details
#' This module provides:
#' \itemize{
#'   \item Click-to-create new entry points
#'   \item Click-on-marker to edit existing points
#'   \item Modal-based editing with validation
#'   \item Integration with emission risk scores for source country selection
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal reactiveValues observeEvent req showModal removeModal showNotification
#' @importFrom shinyWidgets pickerInput
#' @importFrom dplyr filter mutate bind_rows select
#' @importFrom sf st_as_sf st_crs st_drop_geometry st_coordinates st_transform st_geometry
#' @importFrom riskintrodata validate_dataset
#' @importFrom bslib card card_header card_body
#'
#' @keywords internal
interactiveEntryPointsEditorServer <- function(id, input_data, emission_scores, map_proxy, map_click, map_marker_click) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Data storage ----
      updated_data <- reactiveVal(NULL)
      editing_point <- reactiveVal(NULL)

      # Click management ----
      clicks <- reactiveValues(
        map = NULL,
        marker = NULL,
        last = NULL,
        counter = 0L
      )

      lastMarkerClick <- reactiveVal(NULL)

      # Initialize with input data
      observe({
        req(input_data())
        if (is.null(updated_data())) {
          updated_data(input_data())
        }
      })

      # Map event handlers ----
      # Handle map clicks
      observeEvent(map_click(), {
        req(map_click())
        clicks$map <- map_click()
        clicks$marker <- lastMarkerClick()
        clicks$last <- if (is.null(clicks$marker)) {
          "map"
        } else {
          "marker"
        }
        clicks$counter <- clicks$counter + 1L
      })

      # Handle marker clicks
      observeEvent(map_marker_click(), {
        req(map_marker_click())
        lastMarkerClick(map_marker_click())
      })

      # Modal creation logic ----
      observeEvent(clicks$counter, {
        req(updated_data(), emission_scores())

        this_click <- if (clicks$last == "map") clicks$map else clicks$marker

        # Create new entry point
        if (clicks$last == "map") {
          new_point_id <- sprintf("%05i",
            if (nrow(updated_data()) > 0) {
              as.integer(max(updated_data()[["point_id"]], na.rm = TRUE)) + 1
            } else {
              1
            }
          )

          # Create new point and convert click coordinates to sf geometry
          new_point_data <- data.frame(
            point_id = new_point_id,
            point_name = paste0("Entry Point ", new_point_id),
            type = NA_character_,
            mode = NA_character_,
            stringsAsFactors = FALSE
          )

          # Use same CRS as existing data
          crs_to_use <- if (nrow(updated_data()) > 0) {
            st_crs(updated_data())
          } else {
            4326  # Default to WGS84
          }

          # Convert click lng/lat to sf geometry
          click_coords <- data.frame(
            lng = this_click$lng,
            lat = this_click$lat
          )

          click_sf <- st_as_sf(
            click_coords,
            coords = c("lng", "lat"),
            crs = 4326  # Click coordinates are always WGS84
          )

          # Transform to match existing data CRS if needed
          if (st_crs(click_sf) != crs_to_use) {
            click_sf <- st_transform(click_sf, crs_to_use)
          }

          # Combine attributes with geometry
          edit_row <- cbind(new_point_data, st_geometry(click_sf)) |>
            st_as_sf()

          title <- "Add new entry point"
          existing_sources <- character(0)

        # Edit existing entry point
        } else if (clicks$last == "marker") {
          edit_row <- updated_data() |>
            filter(point_id == this_click$id)

          if (nrow(edit_row) == 0) return()

          title <- paste("Edit", edit_row$point_name[1], "entry point")
          existing_sources <- character(0) # Sources functionality simplified for now
        }

        editing_point(edit_row)

        # Reset click events
        lastMarkerClick(NULL)
        clicks$map <- NULL
        clicks$last <- NULL

        # Show modal
        showEditModal(title, edit_row, emission_scores(), existing_sources)
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
      )

      # Modal UI creation function ----
      showEditModal <- function(title, edit_row, emission_data, existing_sources) {
        # Get available source countries from emission scores
        if (nrow(emission_data) > 0 && "iso3" %in% names(emission_data) && "country" %in% names(emission_data)) {
          country_choices <- setNames(
            emission_data$iso3,
            paste(emission_data$country, emission_data$iso3, sep = " - ")
          )
        } else {
          country_choices <- character(0)
        }

        showModal(modalDialog(
          title = title,
          size = "m",
          easyClose = TRUE,

          textInput(
            ns("point_name"),
            label = "Entry point name:",
            value = if (!is.na(edit_row$point_name[1])) edit_row$point_name[1] else ""
          ),

          pickerInput(
            ns("point_type"),
            label = "Transport type:",
            choices = c(
              "Air" = "AIR",
              "Sea" = "SEA",
              "Border crossing" = "BC",
              "Contraband crossing" = "CC",
              "Transhumance crossing" = "TC"
            ),
            selected = if (!is.na(edit_row$type[1])) edit_row$type[1] else NULL
          ),

          pickerInput(
            ns("point_mode"),
            label = "Legality mode:",
            choices = c(
              "Legal" = "C",
              "Illegal" = "NC"
            ),
            selected = if (!is.na(edit_row$mode[1])) edit_row$mode[1] else NULL
          ),

          footer = list(
            actionButton(
              ns("apply"),
              class = "btn-primary",
              "Apply Changes",
              icon = icon("check")
            ),
            if (clicks$last == "marker") {
              actionButton(
                ns("delete"),
                class = "btn-danger",
                "Delete Point",
                icon = icon("trash")
              )
            },
            modalButton("Cancel")
          )
        ))
      }

      # Apply changes ----
      observeEvent(input$apply, {
        req(editing_point())

        edit_row <- editing_point()
        current_data <- updated_data()

        # Update the point with new values
        updated_point <- edit_row |>
          mutate(
            point_name = trimws(input$point_name),
            type = input$point_type,
            mode = input$point_mode
          )

        # Basic validation
        if (is.na(updated_point$point_name[1]) || trimws(updated_point$point_name[1]) == "") {
          showNotification("Entry point name is required", type = "error")
          return()
        }

        # Update the dataset - entry points always use sf geometry
        if (updated_point$point_id[1] %in% current_data$point_id) {
          # Update existing point - keep existing geometry, update attributes only
          updated <- current_data |>
            mutate(
              point_name = ifelse(point_id == updated_point$point_id[1], updated_point$point_name[1], point_name),
              type = ifelse(point_id == updated_point$point_id[1], updated_point$type[1], type),
              mode = ifelse(point_id == updated_point$point_id[1], updated_point$mode[1], mode)
            )
        } else {
          # Add new point - updated_point is already in proper sf format
          updated <- bind_rows(current_data, updated_point)
        }

        updated_data(updated)
        removeModal()
      })

      # Delete point ----
      observeEvent(input$delete, {
        req(editing_point())

        edit_row <- editing_point()
        current_data <- updated_data()

        # Remove the point
        updated <- current_data |>
          filter(point_id != edit_row$point_id[1])

        updated_data(updated)
        removeModal()
      })

      # Return updated data ----
      return(updated_data)
    }
  )
}
