#' @title Compute continous distance matrix from geographical background
#' @description
#' From a geographical background computes (and threshold) a distance matrix.
#' @param tab the input flow dataset.
#' @param dist.method euclidian calculation
#' @param result take the vallue "flowdist" or "dist" allows to parameter the resulting distance dataset (flows filtered by a distance criterion or not)
#' @return (1) A flowdata set with continuous euclidian distances calculations, see dist.method parameter
#' @return (2) A flowdata set with movement from euclidian distances calculations.
#' @return (3) A flowmap filtered by a global distance criterion.
#' @details
#' -- result = "dist" is the resulting tab of the distance\cr
#' -- result = "flowdist" with all the calculated parameters
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils tail
#' @examples
#' library(cartograflow)
#' library(sf)
#' data(flowdata)
#' map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))
#' tabflow <- flowjointure(
#'   geom = "area", bkg = map, DF.flow = flows, origin = "i",
#'   destination = "j", id = "EPT_NUM", x = "X", y = "Y"
#' )
#'
#' # Format long with only origin, destination and distance parameters:
#' tab.distance <- flowdist(tabflow, dist.method = "euclidian", result = "dist")
#' # Format long with with all parameters: coordinates, distance, mouvement
#' tab.distance <- flowdist(tabflow, dist.method = "euclidian", result = "flowdist")
#' @export

flowdist <- function(tab, dist.method, result) {
  if (dist.method == "euclidian") {
    tabflow <- tab %>%
      mutate(distance = sqrt((.data$Xi - .data$Xj)^2 + (.data$Yi - .data$Yj)^2)) %>%
      mutate(mouvement = .data$ydata * .data$distance)

    if (result == "flowdist") {
      return(tabflow)
    }
    if (result == "dist") {
      tab.reduction <- select(tabflow, -.data$ydata, -.data$Xi, -.data$Yi, -.data$Xj, -.data$Yj, -.data$mouvement)
      return(tab.reduction)
    }
  }

  if (dist.method == "xxx") {}
}
