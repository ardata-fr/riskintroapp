#' @importFrom shinyWidgets alert panel list_group
#' @importFrom shiny tagList
validation_status_ui <- function(status){
  if(is.null(status)) return(NULL)
  if(is_dataset_valid(status)) {
    panel_status <- "success"
  } else {
    panel_status <- "danger"
  }
  details <- status$validate_rules$details
  details <- details[!details$valid, ]
  # When data set is valud, details_msg is an empty list()
  # the UI will show only the title saying dataset is valid.
  details_msg <- unnest_validatation_status_details(details)
  args <- list()
  args$val_rules <- shinyWidgets::panel(
    heading = status$validate_rules$msg,
    extra = do.call(list_group, details_msg),
    status = panel_status
  )
  do.call(tagList, args)
}

unnest_validatation_status_details <- function(details){
  details_msg <- list()
  for (i in seq_len(nrow(details))) {
    r <- details[i, ]
    if (r$vectorised_check) {
      # Vectorised checks get precise messages detailing which values are invalid
      # and where
      details_msg[[i]] <- tagList(
        tags$text(tags$strong(r$msg)),
        tags$ul(
          tags$li(paste("Invalid values:", quote_and_collapse(unlist(r$value), max_out = 4))),
          tags$li(paste("At lines:", quote_and_collapse(unlist(r$index), max_out = 4, quote_char = "")))
        )
      )
    } else {
      # Non-vectorised checks get short msg that applies to the whole column
      details_msg[[i]] <- tagList(tags$text(tags$strong(r$msg)))
    }
  }
  details_msg
}
