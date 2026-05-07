#' Options for col guessing in trackframe
#'
#' @param option a character string name of option.
#'  "time_col", "easting_col", "northing_col", or "id_col"
#' @param value  vector of characters with possible candidates
#'
#' @return a vector of candidates for col guessing
#' @export
#' @examples
#' tf_options("time_col", c("t", "timestamp", "time", "time_index",
#' "time_col", "time_column", "tindex"))
#' tf_options("easting_col", c("easting", "east", "utm.easting", "easting_col",
#' "easting_column", "lon", "long", "longitude", "x"))
#' tf_options("northing_col", c("northing", "north", "utm.northing",
#' "northing_col", "northing_column", "lat", "latitude", "y"))
#' tf_options("id_col", c("track_id", "track_id", "trackid", "trackid_col",
#' "trackid_column", "id"))
tf_options <- local({
  options <- list()
  function(option, value) {
    if (missing(option)) {
      return(options)
    }
    if (missing(value)) {
      options[[option]]
    } else {
      options[[option]] <<- value
    }
  }
})

log_debug <- function(
  ...,
  namespace = NA_character_,
  .logcall = sys.call(),
  .topcall = sys.call(-1),
  .topenv = parent.frame(),
  .timestamp = Sys.time()
) {
  logger::log_debug(..., "trackframe", .logcall, .topcall, .topenv, .timestamp)
}

log_info <- function(
  ...,
  namespace = NA_character_,
  .logcall = sys.call(),
  .topcall = sys.call(-1),
  .topenv = parent.frame(),
  .timestamp = Sys.time()
) {
  logger::log_info(..., "trackframe", .logcall, .topcall, .topenv, .timestamp)
}

.onLoad <- function(libname, pkgname) {
  tf_options(
    "time_col",
    c(
      "t",
      "timestamp",
      "time",
      "time_index",
      "time_col",
      "time_column",
      "tindex",
      "datetime"
    )
  )
  tf_options(
    "easting_col",
    c(
      "easting",
      "east",
      "utm.easting",
      "easting_col",
      "easting_column",
      "x",
      "utm.x"
    )
  )
  tf_options(
    "northing_col",
    c(
      "northing",
      "north",
      "utm.northing",
      "northing_col",
      "northing_column",
      "y",
      "utm.y"
    )
  )
  tf_options(
    "id_col",
    c(
      "track_id",
      "animal_id",
      "trackid",
      "trackid_col",
      "trackid_column",
      "id",
      "individual_local_identifier"
    )
  )
  tf_options(
    "sf_easting_col",
    "easting"
  )
  tf_options(
    "sf_northing_col",
    "northing"
  )
  logger::log_formatter(logger::formatter_sprintf, namespace = "trackframe")
}

key_cols <- c("easting", "northing", "time", "id")
