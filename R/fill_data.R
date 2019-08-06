#' Fill missing data in osm
#'
#'@description \code{\link{fill_data}} Fill missing data in OSM columns
#'
#' @param df Spatial roadnetwork
#' @param what Charcter to identify what to aggregate, default "lanes"
#' @param by Charcter to identify aggregate by, default "highway". Sometimes can be "fclass".
#' It is the type of street according to OpenStreetMap covering:
#' "motorway", "motorway_link", "primary", "primary_link", "secondary",
#' "secondary_link", "tertiary", "trunk", "trunk_link"
#' @param FUN Character to apply function: "sum", "mean", etc.
#' @export
#' @seealso \code{\link{clean}} \code{\link{points_to_streets}} \code{\link{speed_to_flow}}
#' @examples \dontrun{
#' a <- clean()
#' data(osm)
#' tail(osm)
#' unique(osm$lanes)
#' osm$lanes <- fill_data(osm)
#' unique(osm$lanes)
#' tail(osm)
#' }
fill_data <- function(net,
                      what = "lanes",
                      by = "highway",
                      FUN = "mean"){
  net[[what]] <-as.numeric(as.character(net[[what]]))

  df <- aggregate(net[[what]],
                  by = list(net[[by]]),
                  FUN = eval(parse(text = FUN)),
                  na.rm = T)
  names(df) <- c(by, what)
  print(df)
  net[[what]] <- ifelse(
    is.na(net[[what]]) & net[[by]] == "motorway",
    df[df[[by]] == "motorway", ][[what]],
    ifelse(
      is.na(net[[what]]) & net[[by]] == "motorway_link",
      df[df[[by]] == "motorway_link", ][[what]],
      ifelse(
        is.na(net[[what]]) & net[[by]] == "trunk",
        df[df[[by]] == "trunk", ][[what]],
        ifelse(
          is.na(net[[what]]) & net[[by]] == "trunk_link",
          df[df[[by]] == "trunk_link", ][[what]],
          ifelse(
            is.na(net[[what]]) & net[[by]] == "primary",
            df[df[[by]] == "primary", ][[what]],
            ifelse(
              is.na(net[[what]]) & net[[by]] == "primary_link",
              df[df[[by]] == "primary_link", ][[what]],
              ifelse(
                is.na(net[[what]]) & net[[by]] == "secondary",
                df[df[[by]] == "secondary", ][[what]],
                ifelse(
                  is.na(net[[what]]) & net[[by]] == "secondary_link",
                  df[df[[by]] == "secondary_link", ][[what]],
                  ifelse(
                    is.na(net[[what]]) & net[[by]] == "tertiary",
                    df[df[[by]] == "tertiary", ][[what]],
                    ifelse(
                      is.na(net[[what]]) & net[[by]] == "tertiary_link",
                      df[df[[by]] == "tertiary_link", ][[what]],
                      net[[what]]
                    ))))))))))
  return(net[[what]])
}
