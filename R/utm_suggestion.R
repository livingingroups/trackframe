assert_latlon <- function(lat, lon, emsg = NULL) {
  if (any(abs(lon) > 180, na.rm = TRUE) || any(abs(lat) > 90, na.rm = TRUE)) {
    default_msg <- "coordinates do not seem to be lon/lat"
    stop(emsg %||% default_msg)
  }
}

crs_lat_lon_order <- function(crs, value = FALSE) {
  wtk <- unlist(strsplit(crs[["wkt"]], "\\s+"))
  idx <- c(
    grep("latitude", wtk, fixed = TRUE),
    grep("longitude", wtk, fixed = TRUE)
  )
  if (length(idx) != 2L) {
    return(NULL)
  }
  if (isTRUE(value)) {
    c("latitude", "longitude")[order(idx)]
  } else {
    order(idx)
  }
}


# A version that enforces the order
st_coordinates_lat_lon <- function(x) {
  coordinates <- st_coordinates(x)
  col_order <- crs_lat_lon_order(sf::st_crs(x))
  if (is.null(col_order) || all(col_order == c(1, 2))) {
    coordinates
  } else {
    coordinates[, col_order]
  }
}

#' Calculate which utm zones are appropreate for input data
#'
#' @param lat vector of latitudes or an sf object
#' @param lon vector of longitudes (empty in case x is an sf object)
#' @return vector of utm crs of the same length as input indicating
#' which zone the data fall into
#' @examples
# nolint start
# code for generating example lat-lon
# dput(round(
#   c(rep(47.6839936, 3), rep(38.5382322, 2)) +
#     rnorm(5, sd = 1e-6),
#   7
# ))
# dput(round(
#   c(rep(9.175044, 3), rep(-121.7642874, 2)) +
#     rnorm(5, sd = 1e-4),
#   6
# ))
# nolint end
#' trackframe::calculate_utm_zone_crs(
#'   lat = c(47.6839952, 47.6839941, 47.6839939, 38.5382329, 38.5382306),
#'   lon = c(9.175119, 9.17498, 9.175254, -121.764227, -121.764351)
#' )
#'
#' trackframe::calculate_utm_zone_crs(
#'   trackframe::move2_mini
#' )
#' @rdname calculate_utm
#' @export
calculate_utm_zone_crs <- function(lat, lon = NULL) {
  UseMethod("calculate_utm_zone_crs")
}

#' @rdname calculate_utm
#' @export
calculate_utm_zone_crs.sf <- function(lat, lon = NULL) {
  coords <- st_coordinates_lat_lon(lat)
  calculate_utm_zone_crs.numeric(coords[, 1], coords[, 2])
}

#' @rdname calculate_utm
#' @export
calculate_utm_zone_crs.numeric <- function(lat, lon = NULL) {
  assert_latlon(lat, lon)

  zone_number <- (floor((lon + 180) / 6) %% 60) + 1

  # Special zones for Norway
  cond_32 <- lat >= 56.0 & lat < 64.0 & lon >= 3.0 & lon < 12.0
  zone_number[cond_32] <- 32

  # Special zones for Svalbard
  cond_lat <- lat >= 72.0 & lat < 84.0

  cond_31 <- cond_lat & lon >= 0.0 & lon < 9.0
  zone_number[cond_31] <- 31

  cond_33 <- cond_lat & lon >= 9.0 & lon < 21.0
  zone_number[cond_33] <- 33

  cond_35 <- cond_lat & lon >= 21.0 & lon < 33.0
  zone_number[cond_35] <- 35

  cond_37 <- cond_lat & lon >= 33.0 & lon < 42.0
  zone_number[cond_37] <- 37

  # EPSG code
  utm <- zone_number[!is.na(zone_number)]
  lat <- lat[!is.na(zone_number)]
  utm[lat > 0] <- utm[lat > 0] + 32600
  utm[lat <= 0] <- utm[lat <= 0] + 32700
  utm
}

#' Suggest a utm crs
#'
#' Uses [calculate_utm_zone_crs] to determine the UTM zone
#' for each datapoint. Returns the most common zone in the dataset.
#'
#' @param lat vector of latitudes or an sf object
#' @param lon vector of longitudes (empty in case x is an sf object)
#' @return crs corresponding to the utm zone that the most data points fall into
#' @examples
#' suggest_utm_zone_crs(
#'   lat = c(47.6839952, 47.6839941, 47.6839939, 38.5382329, 38.5382306),
#'   lon = c(9.175119, 9.17498, 9.175254, -121.764227, -121.764351)
#' )
#'
#' suggest_utm_zone_crs(
#'   trackframe::move2_mini
#' )
#'
#' @details No weighting or averaging is done.
#' Simply the zone that the most of points fall into.
#' Arbitrary in case of a tie. Future versions may use
#' a different (better) methodology to chose a zone
#' when points fall into multiple zones.
#' @rdname suggest_utm
#' @export
suggest_utm_zone_crs <- function(lat, lon = NULL) {
  UseMethod("suggest_utm_zone_crs")
}

#' @rdname suggest_utm
#' @export
suggest_utm_zone_crs.sf <- function(lat, lon = NULL) {
  coords <- st_coordinates_lat_lon(lat)
  suggest_utm_zone_crs.numeric(coords[, 1], coords[, 2])
}

#' @rdname suggest_utm
#' @export
suggest_utm_zone_crs.numeric <- function(lat, lon = NULL) {
  assert_latlon(lat, lon)
  as.integer(tail(names(sort(table(calculate_utm_zone_crs(lat, lon)))), 1))
}
