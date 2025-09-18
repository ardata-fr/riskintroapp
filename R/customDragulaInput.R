#' @title Dragula HTML dependency
#' @description
#' Define the HTML dependency that makes possible to use 'Dragula' inputs
#' in your Shiny application.
#' @return An [htmltools::htmlDependency] object.
#' @importFrom htmltools htmlDependency
#' @noRd
dragula_input_deps <- function() {
  htmlDependency(
    name = "dragula",
    version = format(packageVersion("riskintroapp")),
    src = c(file = system.file("dragula-inputs", package = "riskintroapp")),
    stylesheet = c(
      "css/dragula-styles.css",
      "css/dragula.min.css",
      "css/input-dragula.css"
    ),
    script = c(
      "js/dragula.min.js",
      "js/input-dragula.js"
    )
  )
}


#' @importFrom stringi stri_trans_general stri_trans_tolower stri_replace_all_regex
clean_string <- function(str) {
  str <- stri_trans_general(str = str, id = "Latin-ASCII")
  str <- stri_trans_tolower(str)
  str <- make.unique(str)
  str <- stri_replace_all_regex(
    str = str,
    pattern = "[^a-zA-Z0-9_]+",
    replacement = "_"
  )
  return(str)
}

list1 <- function(x) {
  if (!is.null(x)) {
    if (length(x) == 1 && !is.list(x)) {
      list(x)
    } else {
      x
    }
  }
}

choicesWithNames <- function(choices) {
  listify <- function(obj) {
    makeNamed <- function(x) {
      if (is.null(names(x))) {
        names(x) <- character(length(x))
      }
      x
    }
    res <- lapply(obj, function(val) {
      if (is.list(val)) {
        listify(val)
      } else if (length(val) == 1 && is.null(names(val))) {
        val
      } else {
        makeNamed(as.list(val))
      }
    })
    makeNamed(res)
  }
  choices <- listify(choices)
  if (length(choices) == 0) {
    return(choices)
  }
  choices <- mapply(
    function(choice, name) {
      if (!is.list(choice)) {
        return(choice)
      }
      if (name == "") {
        stop("All sub-lists in \"choices\" must be named.")
      }
      choicesWithNames(choice)
    },
    choices,
    names(choices),
    SIMPLIFY = FALSE
  )
  missing <- names(choices) == ""
  names(choices)[missing] <- as.character(choices)[missing]
  choices
}

dragulaNormalizeChoicesArgs <- function(choices) {
  choices <- choicesWithNames(choices)
  choiceNames <- names(choices)
  choiceValues <- unname(choices)
  return(list(
    choiceNames = as.list(choiceNames),
    choiceValues = as.list(as.character(choiceValues))
  ))
}

dragulaTargetChoices <- function(x, values) {
  if (!is.null(values)) {
    ind <- vapply(
      x$choiceValues,
      function(x) {
        x %in% values
      },
      FUN.VALUE = logical(1)
    )
    list(
      choiceNames = x$choiceNames[ind],
      choiceValues = x$choiceValues[ind]
    )
  }
}

dragulaMakeChoices <- function(inputId, args, status = NULL, badge = FALSE) {
  lapply(seq_along(args$choiceNames), function(i) {
    tags$div(
      class = "dragula-block",
      tags$span(
        class = "label-dragula",
        class = if (badge) {
          "label badge-dragula"
        },
        class = if (badge & !is.null(status)) {
          paste0("label-", status)
        },
        id = paste(
          inputId,
          "target-label",
          clean_string(args$choiceValues[[i]]),
          sep = "-"
        ),
        `data-value` = args$choiceValues[[i]],
        args$choiceNames[[i]]
      )
    )
  })
}

#' @importFrom jsonlite toJSON
#' @importFrom esquisse dragulaInput
#' @title Custom Dragula Input Widget
#' @description
#' A modified version of `esquisse::dragulaInput()` with enhanced styling and layout
#' options. Creates a drag-and-drop interface with a source panel containing draggable
#' items and multiple target panels where items can be dropped. Commonly used for
#' column mapping interfaces in data import workflows.
#'
#' @inheritParams esquisse::dragulaInput
#'
#' @param targets Named character vector defining the target drop zones. Names become
#'   the panel labels, values become the target IDs for server-side handling.
#' @param sourceActions List of HTML elements (buttons, icons, etc.) to display in
#'   the source panel header for additional functionality.
#' @param targetActions Named list where names correspond to target IDs and values
#'   are lists of HTML elements to display in each target panel header.
#' @param ncolGrid Integer specifying the number of columns in the target grid layout.
#'   If NULL (default), uses the number of targets.
#' @param containerStyle CSS style string for the main container div.
#' @param gridStyle CSS style string for the dragula grid container.
#' @param panelStyle CSS style string applied to all panels (source and targets).
#' @param panelContainerStyle CSS style string for the container holding target panels.
#' @param boxStyle CSS style string for the draggable content areas within panels.
#'
#' @return A Shiny UI element (HTML) containing the dragula input widget with source
#'   and target panels. The widget includes all necessary HTML dependencies for
#'   dragula functionality.
#'
#' @details
#' This function creates a drag-and-drop interface with:
#' \itemize{
#'   \item A source panel containing all available draggable items
#'   \item Multiple target panels where items can be dropped
#'   \item Configurable grid layout with flexible styling options
#'   \item Optional action buttons in panel headers
#'   \item Support for item replacement limits in target panels
#' }
#'
#' @importFrom rlang is_named set_names
#' @importFrom purrr pmap
#' @noRd
customDragulaInput <- function(
    inputId,
    sourceLabel,
    targets,
    label = NULL,
    choices = NULL,
    selected = NULL,
    replace = FALSE,
    sourceActions = NULL,
    targetActions = NULL,
    ncolSource = "auto",
    ncolGrid = NULL,
    flip = TRUE,
    choice_status = "primary", # choices status
    target_status = "primary",
    badge = FALSE,
    containerStyle = NULL,
    gridStyle = NULL,
    panelStyle = NULL,
    panelContainerStyle = NULL,
    boxStyle = NULL,
    dragulaOpts = list()
) {
  args <- dragulaNormalizeChoicesArgs(choices)
  if (!is.null(selected) && !is.list(selected)) {
    stop("If provided 'selected' must be a list.", call. = FALSE)
  }
  if (!(is.character(targets) && length(targets) > 0 && all(nzchar(targets)))) {
    stop("Targets must be a character vector", call. = FALSE)
  }
  if (!is_named(targets)) {
    targets <- choicesWithNames(targets)
  }

  if (is.numeric(replace)) {
    maxValues <- replace
    replace <- TRUE
  } else if (isTRUE(replace)) {
    maxValues <- set_names(rep_len(1, length(targets)), names(targets))
  } else if (isFALSE(replace)) {
    maxValues <- numeric(0)
  } else {
    stop("Invalid replace argument")
  }
  if (!any(length(target_status) == 1, length(target_status) == length(targets))) {
    stop("Length of `targets_status` should be same length as `targets` or 1.")
  }
  target_list <- purrr::pmap(list(targets, names(targets), target_status), function(id, label, tar_status) {
    dragulaChoices <- NULL
    if (!is.null(selected)) {
      choicesTarget <- dragulaTargetChoices(args, selected[[id]])
      dragulaChoices <- dragulaMakeChoices(
        inputId = inputId,
        args = choicesTarget,
        status = choice_status,
        badge = badge
      )
    }
    panelX(
      status = tar_status,
      class = "panel-dragula",
      style = panelStyle,
      panelXHeading(
        title = label,
        tags$span(
          class = "dragula-actions",
          if (is.list(targetActions) && is.list(targetActions[[id]])) {
            targetActions[[id]]
          }
        )
      ),
      panelXBody(
        id = paste(inputId, "target", id, sep = "-"),
        `data-max-length` = if (label %in% names(maxValues)) maxValues[[label]],
        class = "dragula-target",
        style = boxStyle,
        dragulaChoices
      )
    )
  })


  if (is.null(ncolGrid)) {
    ncolGrid <- length(targets)
  }
  if (identical(ncolSource, "auto")) {
    ncolSource <- ncolGrid
  }
  cellCount <- ncolSource + length(targets)
  sourcePanelStyle <- NULL
  targetPanelStyle <- NULL

  ncols <- ncolGrid
  nrows <- 2  # Row 1: source panel, Row 2: target grid
  sourcePanelStyle <- paste0("grid-area: 1 / 1 / auto / span ", ncols, ";")
  defaultGridStyle <- paste(
    sprintf("grid-template-rows: %s;", paste(rep("auto", nrows), collapse = ' ')),
    sprintf("grid-template-columns: %s;", paste(rep("auto", ncols), collapse = ' '))
  )
  targetPanelStyle <- paste(
    "grid-template-columns:",
    sprintf("repeat(%s, minmax(50px, auto));", ncolGrid)
  )
  tagList(
    dragula_input_deps(),
    tags$label(
      `for` = inputId,
      class = "control-label",
      class = if (is.null(label)) {
        "shiny-label-null"
      },
      label
    ),
    tags$div(
      class = "form-group shiny-input-container shiny-input-container-inline",
      class = "shiny-input-dragula-custom",
      style = containerStyle,
      id = inputId,
      tags$div(
        class = "dragula-grid",
        style = defaultGridStyle,
        style = gridStyle,
        panelX(
          status = "dragula",
          class = "panel-dragula-source",
          style = panelStyle,
          style = sourcePanelStyle,
          style = panelContainerStyle,
          panelXHeading(
            title = sourceLabel,
            tags$span(
              class = "dragula-actions",
              if (!is.null(sourceActions)) {
                sourceActions
              }
            )
          ),
          panelXBody(
            id = paste(inputId, "source", sep = "-"),
            class = "dragula-source",
            style = boxStyle,
            dragulaMakeChoices(
              inputId = inputId,
              args = args,
              status = choice_status,
              badge = badge
            )
          )
        ),
        tags$div(
          class = "dragula-target-container",
          style = targetPanelStyle,
          style = panelContainerStyle,
          target_list
        )
      ),
      tags$script(
        type = "application/json",
        `data-for` = inputId,
        toJSON(
          list(
            source = list1(paste(inputId, "source", sep = "-")),
            targets = list1(paste(
              inputId,
              "target",
              as.character(targets),
              sep = "-"
            )),
            replace = replace,
            options = dragulaOpts
          ),
          auto_unbox = TRUE,
          json_verbatim = TRUE
        )
      )
    )
  )
}

esqNormalizeChoicesArgs <- esquisse:::normalizeChoicesArgs
esqMakeDragulaChoices <- esquisse:::makeDragulaChoices
esqDropNulls <- esquisse:::dropNulls

#' @noRd
#' @title Div panel
#' @description Create a div of class 'panel'
#' with content inside
#' @param ... HTML content
#' @param status a status for the 'panel' content
#' @importFrom rlang is_string
panelX <- function(..., status = c("default", "primary", "success", "info", "warning", "danger")) {
  if (length(status) > 1) {
    status <- status[[1]]
  }
  tags$div(
    class = "panel",
    class = if (is_string(status)) paste0("panel-", status),
    ...
  )
}

#' @noRd
#' @title Div panel heading
#' @description Create a div of class 'panel-heading'
#' with content inside
#' @param ... HTML content
#' @param title a string to use as title for the 'panel-heading' content.
panelXHeading <- function(..., title = NULL) {
  tags$div(
    class = "panel-heading",
    if (is_string(title)) tags$span(class = "panel-title", title),
    ...
  )
}

#' @noRd
#' @title Div panel body
#' @description Create a div of class 'panel-body'
#' with content inside
#' @param ... HTML content
panelXBody <- function(...) {
  tags$div(
    class = "panel-body",
    ...
  )
}
