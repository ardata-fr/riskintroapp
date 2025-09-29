
#' Workspace Management Module UI
#'
#' Creates a navigation menu with save and load options for managing analysis workspaces.
#' Appears in the application navigation bar.
#'
#' @param id Character string. The namespace id for the module.
#' @return A navigation menu with workspace management options.
#'
#' @keywords internal
#' @importFrom bslib nav_item nav_menu
#' @importFrom shiny actionLink icon NS
workspaceUI <- function(id) {
  ns <- NS(id)
  nav_menu(
    title = "Workspace",
    align = "right",
    icon = icon("briefcase"),
    nav_item(
      actionLink(
        inputId = ns("save"),
        label = "Save",
        icon("download"),
      )),
    nav_item(
      actionLink(
        inputId = ns("load"),
        label = "Load",
        icon("upload")
      ))
  )
}

#' Workspace Management Module Server
#'
#' Handles saving and loading of analysis workspaces:
#' - Save: Exports datasets and settings to ZIP file with user-defined filename
#' - Load: Imports workspace from ZIP file, validates datasets, and restores session
#' - Error handling: Shows alerts for save/load failures
#'
#' @param id Character string. The namespace id for the module.
#' @param datasets Reactive values containing analysis datasets.
#' @param settings Analysis settings to include in workspace.
#' @param misc_risks Reactive value containing miscellaneous risks table.
#' @return A reactive function returning loaded workspace data.
#'
#' @keywords internal
#' @importFrom shiny
#'  downloadButton modalButton downloadHandler reactiveValuesToList
workspaceServer <- function(id, datasets, core_config, misc_risks) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Save ----
    observeEvent(input$save, {
      showModal(
        modalDialog(
          title = titleWithHelpKey("save-workspace-title"),
          textInput(
            inputId = ns("modal_save_name"),
            label = "File name"
          ),
          uiOutput(outputId = ns("modal_save_info")),
          footer = tagList(
            downloadButton(
              outputId = ns("modal_save_dnld"),
              label = "Save workspace",
              icon = icon("download"),
              class = "btn-default"
            ),
            modalButton(
              label = "Cancel",
              icon = icon("xmark")
            )
          )
        )
      )
    })

    output$modal_save_dnld <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(trimws(input$modal_save_name)), ".zip")
      },
      content = function(file) {
        to_save <- datasets()
        # only save input datasets
        to_save <- nullify(to_save)
        misc_risk_data <- lapply(misc_risks(), function(x) {x$dataset})
        misc_risk_settings <- lapply(misc_risks(), function(x) {x[!names(x) %in% "dataset"]})

        to_save <- append(to_save, misc_risk_data)
        safely_save_workspace <- purrr::safely(save_workspace)
        save_result <- safely_save_workspace(
          file = file,
          datasets = to_save,
          settings = list(
            core_config = core_config,
            misc_risks = misc_risk_settings
          )
        )
        removeModal()
        if (isTruthy(save_result$error)) {
          popup_alert_error(
            title = "Unable to save workspace",
            text = "Error while saving workspace:",
            error = save_result$error
            )
        }
      }
    )

    # Load ----
    observeEvent(input$load, {
      showModal(
        modalDialog(
          title = titleWithHelpKey("load-workspace-title"),
          fileInput(
            inputId = ns("modal_load_file"),
            label = "Choose a file",
            multiple = FALSE,
            accept = c(".zip", ".ws")
          ),
          footer = tagList(
            modalButton(
              label = "Cancel",
              icon = icon("xmark")
            )
          )
        )
      )
    })

    updated_workspace <- reactiveVal(NULL)
    observeEvent(input$modal_load_file, {
      safely_load_workspace <- purrr::safely(load_workspace)
      result <- safely_load_workspace(input$modal_load_file$datapath)
      removeModal()
      if (is_error(result$error)) {
        popup_alert_error(
          text = "Error while loading workspace:",
          error = result$error
        )
        return()
      }
      datasets <- result$result$datasets
      tablenames_to_validate <- c("animal_mobility", "epi_units", "entry_points", "emission_risk_factors")

      # Validate input datasets ---------
      inputs_to_validate <- nullify(datasets[names(datasets) %in% tablenames_to_validate])
      # Add input datasets with proper validation names

      validated_datasets <- list()
      validate_msg <- list()
      safely_validate_dataset <- safely(validate_dataset)
      for (name in names(inputs_to_validate)) {
        validation_status <- safely_validate_dataset(
          x = inputs_to_validate[[name]], table_name = name
        )
        if(is_error(validation_status$error)){
          popup_alert_error(
            title = "Error while loading workspace",
            text = "Unable to load workspace due to the following error. Workspace file maybe be corrupt",
            error = validation_status$error
          )
          break
        } else {
          validation_status <- validation_status$result
        }

        if(is_dataset_valid(validation_status)) {
          validated_datasets[[name]] <- extract_dataset(validation_status)
        } else {
          validate_msg[[name]] <- validation_status_ui(validation_status)
        }
      }
      if(length(validate_msg) > 0) {
        showModal(modalDialog(
          title = titleWithHelpKey("workspace-validation-error-title"),
          div(h4("Some datasets where not validated and will not be loaded.")),
          do.call(tagList, validate_msg)
        ))
      }

      # Non-validated datasets ----
      validated_datasets$input_raster <- datasets$input_raster
      validated_datasets$border_input <- datasets$border_input

      # misc_data ---------
      misc_settings <- result$result$settings$misc_risks
      misc_dataset_names <- names(misc_settings)
      # add datasets into misc risk settings
      for (name in misc_dataset_names) {
        curr_dataset <- result$result$datasets[[name]]
        curr_settings <- misc_settings[[name]]
        attr(curr_dataset, "scale") <- curr_settings$initial_scale
        attr(curr_dataset, "join_by") <- curr_settings$join_by
        attr(curr_dataset, "risk_col") <- curr_settings$risk_col
        misc_settings[[name]]$dataset <- curr_dataset
      }
      out <- list(
        datasets = validated_datasets,
        settings = result$core_config,
        misc_settings = misc_settings
      )
      updated_workspace(out)
    })

    updated_workspace
  })
}
