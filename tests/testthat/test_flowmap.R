#' @title Mapping a flow matrix origin-destination
#' @param tab the input flow dataset .csv
#' @param fij colnames of the flow dataset
#' @param origin.f colnames Origin
#' @param destination.f colnames Destination
#' @param x crs 
#' @param nodes the input pt file
#' @param code is the column with the spatial units ID
#' @param nodes.X coordinate X of the point file
#' @param nodes.Y coordinate Y of the point file
#' @param filter allows you to filter (or not) the flow dataset. See details
#' @param threshold is the value of the threshold criterion used to filter the values. The default is 1.
#' @param taille is a graphical parameter for modifying the width of the feature
#' @param bkg the geographical background file .shp or json ...
#' @param a.head  integer code, determining the kind of arrows to be drawn. See Details
#' @param a.length length of the edges of the arrow head (in inches).
#' @param a.angle angle from the shaft of the arrow to the edge of the arrow head.
#' @param a.col color of the arrows
#' @param plota acces au fond de carte
#' @return a matrix or a list with the correct tabflow ID code
#' @return The resulting flowmap

#' @details
#' The flow dataset must be converted to a dataframe for optimal performance (troubles remains with tibble format)
#'
#' If filter = FALSE, all the matrice values are plot [(n*(n-1)] cells, i.e. all links out of the main diagonal.
#' If filter = TRUE only non-zero values are plotted, i.e. existing links with or without threshold.
#' The default threshold is set to 1.
#'
#' a.head is for applying an arrow or not
#' -- code=0 : the link has no head - no arrow
#' -- code=1 : an arrow is draw at (x0[i], y0[i]).
#' -- code=2 : an arrow is draw at (x1[j], y1[j])
#' -- code=3 : an arrow is draw at both nodes.
#' @importFrom graphics segments
#' @importFrom graphics arrows
#' @import sf
#' @examples
#' library(cartograflow)
#' library(sf)
#' data(flowdata)
#' map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))
#' flowmap(
#'   tab = flows, fij = "Fij", origin.f = "i", destination.f = "j",
#'   bkg = map, code = "EPT_NUM", nodes.X = "X", nodes.Y = "Y",
#'   filter = FALSE
#' )
#' @export

flowmap <- function(tab, fij, origin.f, destination.f, bkg = NULL,
                    x,nodes = NULL, code, nodes.X, nodes.Y,
                    filter, plota, threshold, taille,
                    a.head, a.length, a.angle, a.col) {
  tab <- tab %>% select(origin.f, destination.f, fij)


  if (!is.null(nodes)) {
    geo <- "pt"
    mgp_flow <- flowjointure(
      geom = geo, DF.flow = tab, origin = origin.f, destination = destination.f,
      DF.point = nodes, id = code, x = nodes.X, y = nodes.Y
    )

    nodes$code <- as.character(nodes[, code])
    nodes$nodes.X <- as.numeric(nodes[, nodes.X])
    nodes$nodes.Y <- as.numeric(nodes[, nodes.Y])
    
    crs = if (missing(x)) NA_crs_ else st_crs(x)
    nodes <- sf::st_as_sf(x = nodes, coords = c(nodes.X, nodes.Y), crs = crs)

    plot(nodes$geometry, col = "grey", lwd = 0.05)
    }

  if (!is.null(bkg)) {
    geo <- "area"
    mgp_flow <- flowjointure(
      geom = geo, bkg, DF.flow = tab, origin = origin.f, destination = destination.f,
      id = code, x = nodes.X, y = nodes.Y
    )

    plot(mgp_flow$geometry.X, col = "grey", lwd = 0.05)
    
    if (missing(plota)) {
      plota <- FALSE
    }
    else {
      plot(sf::st_geometry(bkg), add = TRUE)
    }
  }

  if (missing(filter)) {
    filter <- FALSE
  }
  else {
    filter
  }

  if (filter == FALSE) {
    trace <- arrows(mgp_flow$Xi, mgp_flow$Yi, mgp_flow$Xj, mgp_flow$Yj, code = 0, col = "black")
    message("All theorical links are plotted")
    
  }

  if (filter == TRUE) {
    if (missing(threshold)) {
      threshold <- 1
      message("you use the default threshold= 1")
    }
    else {
      threshold
    }

    mgp_flow <- mgp_flow[mgp_flow$ydata >= threshold, ]

    if (missing(taille)) {
      taille <- 1
    }
    else {
      taille
    }
    maxsize <- taille
    mgp_flow$size <- (mgp_flow$ydata / max(mgp_flow$ydata)) * maxsize

    if (missing(a.head)) {
      a.head <- 0
    }
    else {
      a.head
    }

    if (missing(a.length)) {
      a.length <- 0.1
    }
    else {
      a.length
    }

    if (missing(a.angle)) {
      a.angle <- 30
    }
    else {
      a.angle
    }

    if (missing(a.col)) {
      a.col <- "#000311"
    }
    else {
      a.col
    }

    trace <- arrows(mgp_flow$Xi, mgp_flow$Yi, mgp_flow$Xj, mgp_flow$Yj,
      length = a.length, angle = a.angle, code = a.head, col = a.col, lwd = mgp_flow$size
    )
  }
}
