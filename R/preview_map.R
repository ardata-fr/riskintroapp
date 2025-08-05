
#' @importFrom cli cli_abort
#' @importFrom riskintroanalysis basemap
#' @importFrom leaflet addPolygons addCircleMarkers
preview_map <- function(x){
  if(!inherits(x, "sf")) {
    cli_abort("{.arg x} must inherit {.cls sf}")
  }

  ll <- basemap(data = x)

  geom_type <- unique(sf::st_geometry_type(x))

  if(all(c("POINT", "POLYGON", "MULTIPOLYGON") %in% geom_type)) {
    cli_abort("Points and polygons in the same dataset is not supported.")
  } else if ("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) {
    ll <- ll |> addPolygons(label = generate_leaflet_labels(x))
  } else if ("POINT" %in% geom_type) {
    ll <- ll |> addCircleMarkers(label = generate_leaflet_labels(x))
  }
  ll
}


#' @importFrom sf st_drop_geometry
#' @importFrom purrr map_df map pmap
generate_leaflet_labels <- function(dat) {
  dat <- st_drop_geometry(dat)
  dat <- purrr::map_df(
    dat,
    function(x){
      if (is.double(x)) {
        sprintf("%.2f", x) # round doubles
      } else if (is.character(x)){
        sprintf("%.47s", x) # truncate long strings
      } else {
        x
      }
    }
  )
  labels <- pmap(
    as.list(dat),
    ~ {
      table_rows <- mapply(
        function(name, value) {
          paste0("<tr><td><strong>", textify(name), "</strong></td><td>", value, "</td></tr>")
        },
        names(dat), c(...)
      )
      paste0(
        '<table style="border-collapse: collapse; width: 100%;">',
        paste0(table_rows, collapse = ""),
        '</table>'
      )
    }
  ) |>
    map(HTML)
  labels
}

textify <- function(string) {
  gsub("_", " ", string) |>
    stringr::str_to_sentence()
}
