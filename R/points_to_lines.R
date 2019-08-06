#' From points to lines
#'
#'@description \code{\link{points_to_lines}} Aggregate points by street
#'
#' @param sfpoints spatial points from \code{\link{clean}}
#' @param dbuffer buffer distance to intersect pointts with streets.
#' IF lat lon, degree, if projected [m]
#' @param net Spatial roadnetwork
#' @importFrom  data.table as.data.table
#' @importFrom sf st_as_sf st_buffer st_intersection
#' @note net must have a column name id to each street.
#' sfpoints must include names "id", "veh", "type" and "speed".
#' Use with UTM data (not lat lon)
#' @export
#' @seealso \code{\link{clean}}
#' @examples \dontrun{
#' a <- clean()
#' a
#' plot(a["type"], axes = T)
#' library(vein)
#' data(net)
#' net$id <- 1:nrow(net@data)
#' d <- points_to_lines(a, 1/102/47, net)
#' plot(d[c("MeanSpeed")], axes = T)
#' plot(d[c("MaxSpeed")], axes = T)
#' }
points_to_lines <- function(sfpoints,
                            dbuffer,
                            net,
                            verbose = TRUE){
  sfpoints <- sf::st_as_sf(sfpoints)
  net <- sf::st_as_sf(net)

  vej_b10m <- sf::st_buffer(x = sfpoints, dist = dbuffer)
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
                  by = .(id, type)]

  names(fluxo) <- c("id", "type","veh", "MeanSpeed", "MedianSpeed",
                    "Q75Speed", "Q85SPeed", "Q95Speed", "MaxSpeed" )
  net <- net["id"]
  fluxo <- merge(net, fluxo, by = "id", all.y = T)
  return(fluxo)
}
