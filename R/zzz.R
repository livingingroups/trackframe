#FIXME: documentation

#' Options for trackframe
#'
#' @param option name of option
#' @param value  value
#'
#' @export
#'
#' @examples
#' tf_options("time_col", c("t", "timestamp", "time", "time_index",
#' "time_col", "time_column", "tindex"))
#' tf_options("easting_col", c("easting", "east", "utm.easting", "easting_col",
#' "easting_column", "lon", "long", "longitude", "x"))
#' tf_options("northing_col", c("northing", "north", "utm.northing",
#' "northing_col", "northing_column", "lat", "latitude", "y"))
#' tf_options("id_col", c("animal_id", "track_id", "trackid", "trackid_col",
#' "trackid_column", "id"))
tf_options <- local({
  options <- list()
  function(option, value) {
    if (missing(option)) return(options)
    if (missing(value))
      options[[option]]
    else
      options[[option]] <<- value
  }
})


.onLoad <- function( libname, pkgname ) {
  tf_options("time_col", c("t", "timestamp", "time", "time_index", "time_col", "time_column", "tindex"))
  tf_options("easting_col", c("easting", "east", "utm.easting", "easting_col", "easting_column", "lon", "long", "longitude", "x"))
  tf_options("northing_col", c("northing", "north", "utm.northing", "northing_col", "northing_column", "lat", "latitude", "y"))
  tf_options("id_col", c("animal_id", "track_id", "trackid", "trackid_col", "trackid_column", "id"))
}

