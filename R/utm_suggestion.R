assert_latlon <- function(lat, lon, emsg = NULL) {
  if (any(abs(lon) > 180, na.rm = TRUE) || any(abs(lat) > 90, na.rm = TRUE)) {
    default_msg <- "coordinates do not seem to be lon/lat"
    stop(emsg %||% default_msg)
  }
}

crs_lat_lon_order <- function(crs, value = FALSE) {
  wtk <- unlist(strsplit(crs[["wkt"]], "\\s+"))
  idx <- c(grep("latitude", wtk, fixed = TRUE), grep("longitude", wtk, fixed = TRUE))
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
#'
#' @rdname calculate_utm_zone
#' @export
calculate_utm_zone_crs <- function(lat, lon = NULL) {
  UseMethod("calculate_utm_zone_crs")
}

#' @rdname calculate_utm_zone
#' @export
calculate_utm_zone_crs.sf <- function(lat, lon = NULL) {
  coords <- st_coordinates_lat_lon(lat)
  calculate_utm_zone_crs.numeric(coords[, 1], coords[, 2])
}

#' @rdname calculate_utm_zone
#' @export
calculate_utm_zone_crs.numeric <- function(lat, lon = NULL) {
  assert_latlon(lat, lon)

  zone_number <- (floor((lon + 180) / 6) %% 60) + 1

  # Special zones for Norway
  cond_32 <- lat >= 56.0 & lat < 64.0 & lon >= 3.0 & lon < 12.0
  zone_number[cond_32] <- 32

  # Special zones for Svalbard
  cond_lat <- lat >= 72.0 & lat < 84.0

  cond_31 <- cond_lat & lon >= 0.0 & lon <  9.0
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
#' @param lat vector of latitudes or an sf object
#' @param lon vector of longitudes (empty in case x is an sf object)
#' @return crs corresponding to the utm zone that the most data points fall into
#'
#' @details arbitrary in case of a tie
#' @rdname suggest_utm
#' @export
suggest_utm_crs <- function(lat, lon = NULL) {
  UseMethod("suggest_utm_crs")
}

#' @rdname suggest_utm
#' @export
suggest_utm_crs.sf <- function(lat, lon = NULL) {
  coords <- st_coordinates_lat_lon(lat)
  suggest_utm_crs.numeric(coords[, 1], coords[, 2])
}

#' @rdname suggest_utm
#' @export
suggest_utm_crs.numeric <- function(lat, lon = NULL) {
  assert_latlon(lat, lon)
  as.integer(tail(names(sort(table(calculate_utm_zone_crs(lat, lon)))), 1))
}
