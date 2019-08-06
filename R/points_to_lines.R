#' From points to lines
#'
#'@description \code{\link{points_to_lines}} Aggregate points by street
#'
#' @param sfpoints spatial points from \code{\link{clean}}
#' @param dist buffer distance to intersect pointts with streets.
#' IF lat lon, degree, if projected [m]
#' @param net Spatial roadnetwork
#' @importFrom  data.table as.data.table
#' @importFrom sf st_as_sf st_buffer st_intersection
#' @importFrom units set_units
#' @note net must have a column name id to each street.
#' sfpoints must include names "id", "veh" and "speed".
#' Use with UTM data (not lat lon)
#' @export
#' @seealso \code{\link{clean}}
#' @examples \dontrun{
#' a <- clean()
#' a
#' plot(a["type"], axes = T, pch = 16, cex = 0.5, col = "red")
#' d <- points_to_lines(a, 1/102/47, osm)
#' plot(d[c("MeanSpeed")], axes = T)
#' plot(d[c("MaxSpeed")], axes = T)
#' }
points_to_lines <- function(sfpoints,
                            dist,
                            net,
                            verbose = TRUE){
  sfpoints <- sf::st_as_sf(sfpoints)
  net <- sf::st_as_sf(net)
  if(class(sfpoints$speed) != "units") stop("sfpoints$speed must have units of speed")
  km <- h <- NULL
  sfpoints$speed <- units::set_units(sfpoints$speed, km/h)
  vej_b10m <- sf::st_buffer(x = sfpoints, dist = dist)
  vej <- sf::st_intersection(x = net, y = vej_b10m)
  vej <- data.table::as.data.table(vej)
  veiculo <- speed <- id <- type <- NULL
  fluxo <- vej[   , .(length(veh),
                      mean(speed, na.rm = T),
                      median(speed, na.rm = T),
                      quantile(speed, .75, na.rm = T),
                      quantile(speed, .85, na.rm = T),
                      quantile(speed, .95, na.rm = T),
                      max(speed, na.rm = T)),
                  by = .(id)]

  names(fluxo) <- c("id", "veh", "MeanSpeed", "MedianSpeed",
                    "Q75Speed", "Q85SPeed", "Q95Speed", "MaxSpeed" )
  fluxo <- merge(net, fluxo, by = "id", all.y = T)
  return(fluxo)
}
