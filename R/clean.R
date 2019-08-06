#' Reads, cleans  and filter GPS data-set
#'
#'@description \code{\link{clean}} Reads, cleans  and filter GPS data-set
#'
#' @param input_file Character with location of '.csv', or data.table or data.frame
#' with gps data. The following colnames are required: "veh", "type", "time",
#' "lon", "lat". "veh" id for each vehicle. "type" type of each vehicle,
#' "time" character with format "2014-10-10 08:06:40".
#' @param type filter type
#' @param output_file Character to store resulting data.table as .csv. If missing
#' returns data.table
#' @param timezone timezone
#' @param n Integer, minimum number of consecutive observations by veh
#' @param max_speed Numeric, Maximum speed default 110 km/h
#' @param max_acceleration Numeric, Maximum acceleration default 10 m/s^2
#' @param coords in case of point data: names or numbers of the numeric columns holding coordinates
#' @param crs coordinate reference system
#' @param verbose logical, to show more information
#' @importFrom  data.table fread as.data.table ":=" shift .N
#' @importFrom units as_units set_units
#' @importFrom geosphere distHaversine
#' @importFrom utils write.table
#' @export
#' @seealso \code{\link{clean}}
#' @examples \dontrun{
#' a <- clean()
#' }
clean <- function(input_file = "data-raw/dados/000000000000.csv",
                  type,
                  output_file,
                  timezone = "Etc/UTC", #America/Sao_Paulo
                  n = 5,
                  max_speed = units::as_units(110.0, "km/h"),
                  max_acceleration = units::as_units(10.0, "m/s^2"),
                  coords = c("lat", "lon"),
                  crs = 4326,
                  verbose = TRUE){
  # input
  if(class(input_file) == "character"){
    if(verbose) message("Reading... \n")
    dt_in_gps_data <- data.table::fread(input = input_file,
                                        header = TRUE,
                                        na.strings = "NA",
                                        stringsAsFactors = FALSE
    )
  } else {
    if(verbose) message("Transforming to data.table... \n")
    dt_in_gps_data <- data.table::as.data.table(input_file)
  }
  if(verbose) message("Renaming data.table... \n")
  names(dt_in_gps_data) <- c("veh", "type", "time", "lon", "lat")
  if(!missing(type)) {
    if(verbose) message("Filtering data.table... \n")
    dt_in_gps_data <- dt_in_gps_data[type == type]
  }
  if(verbose) message("Transforming character time in POSIXct... \n")
  dt_in_gps_data$time <- as.POSIXct(dt_in_gps_data$time,
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "Etc/UTC" )
  dt_in_gps_data$LT <- dt_in_gps_data$time
  # Timezone
  attr(dt_in_gps_data$LT, "tzone") <- timezone

  # lags
  time <- NULL
  lat <- NULL
  lon <- NULL
  if(verbose) message("Calculating lags... \n")
  dt_in_gps_data[,
                 "lag_time" := data.table::shift(x = time,
                                                 n = 1,
                                                 type = 'lag'),
                 by = "veh"]
  dt_in_gps_data[,
                 "lag_lat" := data.table::shift(x = lat,
                                                n = 1,
                                                type = 'lag'),
                 by = "veh"]
  dt_in_gps_data[,
                 "lag_lon" := data.table::shift(x = lon,
                                                n = 1,
                                                type = 'lag'),
                 by = "veh"]
  # delta time
  if(verbose) message("Calculating delta time... \n")
  dt_in_gps_data$delta_time <- units::as_units(as.numeric(difftime(dt_in_gps_data$time,
                                                                   dt_in_gps_data$lag_time,
                                                                   units="secs")),'s')

  # delta space
  if(verbose) message("Calculating delta space... \n")
  x <- 1
  f1 <- function(lat, lon, lag_lat, lag_lon) {
    function() {

      x <<- x + 1
      if (is.na(lag_lat)) {
        NA
      }
      else {
        geosphere::distHaversine(c(as.numeric(lag_lat), as.numeric(lag_lon)), c(as.numeric(lat),as.numeric(lon)))
      }
    }
  }

  dt_in_gps_data$delta_space <- apply(dt_in_gps_data,
                                      1,
                                      function(dt_in_gps_data) f1(dt_in_gps_data['lat'],
                                                                  dt_in_gps_data['lon'],
                                                                  dt_in_gps_data['lag_lat'],
                                                                  dt_in_gps_data['lag_lon'])())

  dt_in_gps_data$delta_space <- units::as_units(dt_in_gps_data$delta_space, "m")
  # speeds
  if(verbose) message("Calculating speeds... \n")
  dt_in_gps_data[, "speed"  := dt_in_gps_data$delta_space/dt_in_gps_data$delta_time]


  dt_in_gps_data[, "lag_speed" := data.table::shift(x = dt_in_gps_data$speed,
                                                    n = 1,
                                                    type = 'lag')]
  dt_in_gps_data[, "delta_speed" := dt_in_gps_data$speed - dt_in_gps_data$lag_speed]
  # acceleration
  if(verbose) message("Calculating acceleration... \n")
  dt_in_gps_data[, "acceleration"  := dt_in_gps_data$delta_speed/dt_in_gps_data$delta_time]

  # Filter
  if(verbose) message("Filtering by 'n' obervations... \n")
  dt_in_gps_data[,
                 "n" := .N,
                 by = "veh"]
  a <- nrow(dt_in_gps_data)
  dt_in_gps_data <- dt_in_gps_data[ dt_in_gps_data$n >= n]
  b <- nrow(dt_in_gps_data)
  if(verbose) message(paste0(round(100*(a - b)/a, 2),
                             " % dont have at least , ", n,
                             " obervations by 'veh' \n"))

  # filtering speed and acceleration
  a <- nrow(dt_in_gps_data)
  dt_in_gps_data <- dt_in_gps_data[abs(dt_in_gps_data$acceleration) <= max_acceleration &
                                     abs(dt_in_gps_data$speed) <= max_speed, ]

  # Original Date: 2019-05-31
  # Changed Date: 2019-08-05
  dt_in_gps_data$speed <- abs(dt_in_gps_data$speed)

  b <- nrow(dt_in_gps_data)
  if(verbose) message(paste0(round(100*(a - b)/a, 2),
                             " % dont have acceleration <= ", max_acceleration,
                             " and speed <= ", max_speed," \n"))

  # converting speed in m/s to km/h
  km <- h <- NULL
  dt_in_gps_data$speed <- units::set_units(dt_in_gps_data$speed, km/h)

  # Changed Date: 2019-08-05
  dt_in_gps_data <- as.data.frame(dt_in_gps_data)
  # sf
  sdf <- sf::st_as_sf(dt_in_gps_data, coords = coords, crs = 4326)
  sdf[[coords[1]]] <- dt_in_gps_data[[coords[1]]]
  sdf[[coords[2]]] <- dt_in_gps_data[[coords[2]]]
  dt_in_gps_data <- sdf
  # output
  if(missing(output_file)){
    return(dt_in_gps_data)
  } else {
    utils::write.table(
      dt_in_gps_data,
      file = output_file,
      quote = FALSE,
      sep = ',',
      na = '',
      col.names = TRUE,
      row.names = FALSE
    )

  }
}


