
#' @importFrom leaflet fitBounds
#' @importFrom sf st_bbox
setBoundsFromSF <- function(ll, x) {
  bb <- sf::st_bbox(x)
  leaflet::fitBounds(
    map = ll,
    lng1 = bb$xmin[[1]], lat1 = bb$ymin[[1]],
    lng2 = bb$xmax[[1]], lat2 = bb$ymax[[1]]
  )
}

