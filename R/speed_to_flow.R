#' From points to lines
#'
#'@description \code{\link{points_to_lines}} Aggregate points by street
#'
#' @param net Spatial roadnetwork
#' @param speed Charcter to identify column speed
#' @param highway Charcter to identify type of street, sometimes called as fclass in osm.
#' @param lanes Charcter to identify column lanes
#' @param method "methodsx" only in the meantime
#' @param df_street optional data.frame with the same dimensions as data(m1),
#' but values can change. This means that can user internal values or own values.
#' @export
#' @seealso \code{\link{clean}} \code{\link{points_to_streets}}
#' @examples \dontrun{
#' a <- clean()
#' a
#' plot(a["type"], axes = T, pch = 16, cex = 0.5, col = "red")
#' data(osm)
#' unique(osm$lanes)
#' osm$lanes <- fill_data(osm)
#' unique(osm$lanes)
#' d <- points_to_lines(a, 1/102/47, osm)
#' #same speed becasue few observations by street
#' #need more data
#' plot(d[c("MeanSpeed")], axes = T)
#' plot(d[c("MaxSpeed")], axes = T)
#' b <- speed_to_flow(d)
#' }
speed_to_flow <- function(net,
                          speed = "MaxSpeed",
                          highway = "highway",
                          lanes = "lanes",
                          method = "methodsx",
                          df_streets){
  if(!any(grepl(pattern = "highway", x = names(net)))) {
    stop("Add a column named `highway` in net")
  }
  if(!any(grepl(pattern = "speed", x = names(net)))) {
    stop("Add a column named `speed` in net")
  }
  # check data. User can add its own observations
  if(missing(df_streets)){
    cet <- sysdata
  } else {
    cet <- df_streets
  }

# Cars ####
  net$CarsCor <- 0
  # motorway
  m1 <- cet[cet$highway == "motorway", ]

  net$CarsCor <- ifelse(
    net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Cars_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Cars_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Cars_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Cars_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Cars_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Cars_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Cars_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Cars_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Cars_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Cars_Speed*net[[speed]],
                      net$CarsCor
                    ))))))))))

  # trunk
  m1 <- cet[cet$highway == "trunk", ]

  net$CarsCor <- ifelse(
    net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Cars_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Cars_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Cars_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Cars_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Cars_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Cars_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Cars_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Cars_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Cars_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Cars_Speed*net[[speed]],
                      net$CarsCor
                    ))))))))))

  # primary
  m1 <- cet[cet$highway == "primary", ]

  net$CarsCor <- ifelse(
    net[[highway]] == "primary" | net[[highway]] == "primary_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Cars_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "primary" | net[[highway]] == "primary_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Cars_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "primary" | net[[highway]] == "primary_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Cars_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "primary" | net[[highway]] == "primary_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Cars_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "primary" | net[[highway]] == "primary_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Cars_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Cars_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Cars_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Cars_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Cars_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Cars_Speed*net[[speed]],
                      net$CarsCor
                    ))))))))))

  # secondary
  m1 <- cet[cet$highway == "secondary", ]

  net$CarsCor <- ifelse(
    net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Cars_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Cars_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Cars_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Cars_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Cars_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Cars_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Cars_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Cars_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Cars_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Cars_Speed*net[[speed]],
                      net$CarsCor
                    ))))))))))

  # tertiary
  m1 <- cet[cet$highway == "tertiary", ]

  net$CarsCor <- ifelse(
    net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Cars_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Cars_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Cars_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Cars_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Cars_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Cars_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Cars_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Cars_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Cars_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Cars_Speed*net[[speed]],
                      net$CarsCor
                    ))))))))))

  # Trucks ####
  net$TrucksCor <- 0
  # motorway
  m1 <- cet[cet$highway == "motorway", ]

  net$TrunksCor <- ifelse(
    net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Trucks_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Trucks_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Trucks_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Trucks_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Trucks_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Trucks_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Trucks_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Trucks_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Trucks_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "motorway" | net[[highway]] == "motorway_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Trucks_Speed*net[[speed]],
                      net$TrunksCor
                    ))))))))))

  # trunk
  m1 <- cet[cet$highway == "trunk", ]

  net$TrunksCor <- ifelse(
    net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Trucks_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Trucks_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Trucks_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Trucks_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Trucks_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Trucks_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Trucks_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Trucks_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Trucks_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "trunk" | net[[highway]] == "trunk_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Trucks_Speed*net[[speed]],
                      net$TrunksCor
                    ))))))))))

  # primary
  m1 <- cet[cet$highway == "primary", ]

  net$TrunksCor <- ifelse(
    net[[highway]] == "primary" | net[[highway]] == "primary_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Trucks_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "primary" | net[[highway]] == "primary_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Trucks_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "primary" | net[[highway]] == "primary_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Trucks_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "primary" | net[[highway]] == "primary_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Trucks_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "primary" | net[[highway]] == "primary_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Trucks_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Trucks_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Trucks_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Trucks_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Trucks_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "primary" | net[[highway]] == "primary_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Trucks_Speed*net[[speed]],
                      net$TrunksCor
                    ))))))))))

  # secondary
  m1 <- cet[cet$highway == "secondary", ]

  net$TrunksCor <- ifelse(
    net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Trucks_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Trucks_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Trucks_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Trucks_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Trucks_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Trucks_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Trucks_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Trucks_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Trucks_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "secondary" | net[[highway]] == "secondary_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Trucks_Speed*net[[speed]],
                      net$TrunksCor
                    ))))))))))

  # tertiary
  m1 <- cet[cet$highway == "tertiary", ]

  net$TrunksCor <- ifelse(
    net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
      net[[lanes]] >= 10,
    m1[m1[[lanes]] == 10, ]$Trucks_Speed*net[[speed]],
    ifelse(
      net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
        net[[lanes]] < 10 & net[[lanes]] >= 9,
      m1[m1[[lanes]] == 9, ]$Trucks_Speed*net[[speed]],
      ifelse(
        net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
          net[[lanes]] < 9 & net[[lanes]] >= 8,
        m1[m1[[lanes]] == 8, ]$Trucks_Speed*net[[speed]],
        ifelse(
          net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
            net[[lanes]] < 8 & net[[lanes]] >= 7,
          m1[m1[[lanes]] == 7, ]$Trucks_Speed*net[[speed]],
          ifelse(
            net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
              net[[lanes]] < 7 & net[[lanes]] >= 6,
            m1[m1[[lanes]] == 6, ]$Trucks_Speed*net[[speed]],
            ifelse(
              net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                net[[lanes]] < 6 & net[[lanes]] >= 5,
              m1[m1[[lanes]] == 5, ]$Trucks_Speed*net[[speed]],
              ifelse(
                net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                  net[[lanes]] < 5 & net[[lanes]] >= 4,
                m1[m1[[lanes]] == 4, ]$Trucks_Speed*net[[speed]],
                ifelse(
                  net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                    net[[lanes]] < 4 & net[[lanes]] >= 3,
                  m1[m1[[lanes]] == 3, ]$Trucks_Speed*net[[speed]],
                  ifelse(
                    net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                      net[[lanes]] < 3 & net[[lanes]] >= 2,
                    m1[m1[[lanes]] == 2, ]$Trucks_Speed*net[[speed]],
                    ifelse(
                      net[[highway]] == "tertiary" | net[[highway]] == "tertiary_link" &
                        net[[lanes]] < 2 & net[[lanes]] >= 0,
                      m1[m1[[lanes]] == 1, ]$Trucks_Speed*net[[speed]],
                      net$TrunksCor
                    ))))))))))

  return(net)
}
