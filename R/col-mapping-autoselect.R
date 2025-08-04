
.auto_select_dict <- list(
  point_name = c("name","name", "descr", "nom", "id"),
  mode       = c("mode", "legal"),
  type       = c("type", "transport"),
  lat        = c("lat", "ycoord"),
  lng        = c("lng", "long", "xcoord"),
  source     = c("source", "origin"),
  sources     = c("source", "origin"),
  geometry   = c("geometry"),
  eu_name    = c("name", "descr", 'nom', "id"),
  geometry   = c("geom"),
  o_name     = c("name","nom", "ori"),
  o_lat      = c("lat", "ori", "start"),
  o_lng      = c("lng", "long", "ori", "start"),
  d_name     = c("name","nom", "dest"),
  d_lat      = c("lat", "dest", "end"),
  d_lng      = c("lng", "long", "dest", "end"),
  quantity   = c("headcount", "qty", "quantity", "weight", "flow", "volume")
)

#' @importFrom purrr map map_int
#' @importFrom stringr str_detect fixed
auto_select_cols <- function(user_cols, options) {
  dict <- .auto_select_dict[options]
  dict <- Filter(function(x) !is.null(x), dict)
  matched_cols <- map(
    dict,
    function(keywords) {
      matches <- map_int(user_cols, ~ sum(str_detect(.x, fixed(keywords, ignore_case = TRUE))))
      best_match <- user_cols[which.max(matches)]
      if (all(matches == 0)) {
        NULL
      } else {
        best_match
      }
    }
  )
  if (length(matched_cols) < 1) return(NULL)
  return(matched_cols)
}
