#' @importFrom shinyWidgets alert panel list_group
validation_status_ui <- function(status){
  args <- list()

  if(is.null(status)) return(NULL)

  if(is_dataset_valid(status)) {
    return(
      shinyWidgets::alert(
        tags$text("Dataset has been validated."),
        status = "success"
      ))
  } else {
    details <- status$validate_rules$details
    details <- details[!details$valid, ]
    details_msg <- unnest_validatation_status_details(details)
    args$val_rules <- shinyWidgets::panel(
      heading = status$validate_rules$msg,
      extra = do.call(list_group, details_msg),
      status = "danger"
    )
  }
  if (length(args) > 0) do.call(tagList, args) else NULL
}

unnest_validatation_status_details <- function(details){
  details_msg <- list()
  for (i in seq_len(nrow(details))) {
    r <- details[i, ]
    if (length(unlist((r$value))) > 1) {
      details_msg[[i]] <- tagList(
        tags$text(tags$strong(r$msg)),
        tags$ul(
          tags$li(paste("Invalid values:", quote_and_collapse(unlist(r$value), max_out = 4))),
          tags$li(paste("At lines:", quote_and_collapse(unlist(r$index), max_out = 4, quote_char = "")))
        )
      )
    } else {
      # short msg (no example values, no 'lines at')
      details_msg[[i]] <- tagList(tags$text(tags$strong(r$msg)))
    }
  }
  details_msg
}

