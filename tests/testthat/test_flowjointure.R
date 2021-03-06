#' @title Create a spatial join with flow
#' @description Create an attribute spatial join between a flow dataset table and a map background
#' @param DF.flow the input flow dataset table in long format
#' @param origin ID origin head of dataset flow
#' @param destination ID destination head of dataset flow
#' @param DF.point point file
#' @param id code of point file
#' @param x coordinate X of the point file
#' @param y coordinate Y of the point file
#' @param geom choices files pt or area
#' @param bkg the map background file, ie. a shapefile or json
#' @return Resulting jointure table between flow dataset and map background
#' @export
#' @importFrom maptools readShapeSpatial
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @import sp
#' @examples
#' library(cartograflow)
#' library(sf)
#' data(flowdata)
#' map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))
#' tabflow <- flowjointure(
#'   geom = "area", bkg = map, DF.flow = flows, origin = "i", destination = "j",
#'   id = "EPT_NUM", x = "X", y = "Y"
#' )
#' @export


flowjointure <- function(geom, bkg, DF.flow, origin, destination,
                         DF.point, id, x, y) {
  if (geom == "pt") {
    if (missing(bkg)) {
      bkg <- NULL
    }
    if (missing(id) && missing(x) && missing(y)) {
      message("It's necessary to put the variable id,x,y")
    }

    df.point <- DF.point %>% select(id, x, y)

    dff.jointure <- DF.flow %>%
      left_join(df.point, by = setNames(id, nm = origin)) %>%
      rename(Xi = x, Yi = y) %>%
      left_join(df.point, by = setNames(id, nm = destination)) %>%
      rename(Xj = x, Yj = y)
    colnames(dff.jointure) <- c("i", "j", "ydata", "Xi", "Yi", "Xj", "Yj")
    return(dff.jointure)
  }

  if (geom == "area") {
    if (missing(DF.point)) {
      DF.point <- NULL
    }
    if (missing(bkg) && missing(DF.flow)) {
      message("It's necessary to put the variable bkg and DF.flow")
    }
    if (missing(id) && missing(x) && missing(y)) {
      message("It's necessary to put the variable id,x,y")
    }

    pt_centroid <- bkg %>% sf::st_centroid()
    pts <- sf::st_coordinates(pt_centroid)

    p <- cbind(pts, pt_centroid)

    df.point <- p %>% select(id, x, y)

    dfp.jointure <- DF.flow %>%
      left_join(df.point, by = setNames(id, nm = origin)) %>%
      rename(Xi = x, Yi = y) %>%
      left_join(df.point, by = setNames(id, nm = destination)) %>%
      rename(Xj = x, Yj = y)
    colnames(dfp.jointure) <- c("i", "j", "ydata", "Xi", "Yi", "geometry.X", "Xj", "Yj", "geometry.Y")
    return(dfp.jointure)
  }
}
