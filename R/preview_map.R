#' @importFrom cli cli_abort
#' @importFrom riskintroanalysis basemap
#' @importFrom leaflet addPolygons addCircleMarkers
#' @importFrom riskintroanalysis generate_leaflet_labels
preview_map <- function(x){
  if(!inherits(x, "sf")) {
    cli_abort("{.arg x} must inherit {.cls sf}")
  }

  ll <- basemap(data = x)

  geom_type <- unique(sf::st_geometry_type(x))

  if(all(c("POINT", "POLYGON", "MULTIPOLYGON") %in% geom_type)) {
    cli_abort("Points and polygons in the same dataset is not supported.")

  } else if ("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) {

    ll <- ll |>
      addPolygons(label = generate_leaflet_labels(x))

  } else if ("POINT" %in% geom_type) {
    ll <- ll |>
      addCircleMarkers(label = generate_leaflet_labels(x))

  }
  ll
}
