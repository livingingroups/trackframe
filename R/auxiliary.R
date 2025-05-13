#accessor for cols

# #' @export
"index<-track_frame" <- function(x, value)
{
  stop("TODO")
}

#' Extract Index from a Track Frame
#'
#' This function retrieves the index values from a `track_frame` object.
#' The `track_frame` should be a data frame-like structure with an attribute
#' specifying the column containing index values.
#'
#' @param x A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the index column (`index`).
#' @param ... Additional arguments to be passed to `index`.
#' @return A vector of index values extracted from the `track_frame`.
#' @examples
#' # Assuming `tf` is a valid track_frame with an index column attribute:
#' tf <- sim_travel_path(100, format = "track_frame")
#' index_values <- index(tf)
#' print(index_values)
#' @export
index.track_frame <- function(x, ...){
  x[[attr(x, "index")]]
}

#longitude

#' Extract Longitude from a Track Frame
#'
#' This function retrieves the longitude values from a `track_frame` object.
#' The `track_frame` should be a data frame-like structure with an attribute
#' specifying the column containing longitude values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the longitude column (`lon_col`).
#' @return A vector of longitude values extracted from the `track_frame`.
#' @examples
#' # Assuming `tf` is a valid track_frame with a longitude column attribute:
#' tf <- sim_travel_path(100, format = "track_frame")
#' longitude_values <- longitude(tf)
#' print(longitude_values)
#' @export
longitude <- function(tf){
  assert_class(tf, "track_frame")
  tf[[attr(tf, "lon_col")]]
}

#' Extract Latitude from a Track Frame
#'
#' This function retrieves the latitude values from a `track_frame` object.
#' The `track_frame` should be a data frame-like structure with an attribute
#' specifying the column containing latitude values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the latitude column (`lat_col`).
#' @return A vector of latitude values extracted from the `track_frame`.
#' @examples
#' # Assuming `tf` is a valid track_frame with a latitude column attribute:
#' tf <- sim_travel_path(100, format = "track_frame")
#' latitude_values <- latitude(tf)
#' print(latitude_values)
#' @export
latitude <- function(tf){
  assert_class(tf, "track_frame")
  tf[[attr(tf, "lat_col")]]
}

#' @export
unique_ids <- function(tf) {
  assert_class(tf, "track_frame")
  ids <- unique(tf[, attr(tf, "id_cols")])
  if (is.null(dim(ids))) {
    ids <- data.frame(
      ids
    )
    names(ids) <- attr(tf, "id_cols")
  }
  ids
}


#select id of trackframe
# id can also be a dataframe
#' @export 
select_id <- function(tf, id) {
  assert_class(tf, "track_frame")
  # tf <- FFT_tf
  # id <- "Abby"
  # id <- c("Abby", "4652")
  
  if(length(id) > 1) {
    tf <- tf[do.call(paste0, tf[, attr(tf, "id_cols")]) %in% paste0(id, collapse = ""), ]
  } else { #TODO we need more sophisticated check here
  tf <- tf[tf[, attr(tf, "id_cols")] == id, ]
  }
  return(tf)
}


# coredata.track.frame <- function(tf){
#   #TODO check what we want to do in coredata
#   ctf <- tf[, c(attr(tf, "index"), attr(tf, "lon_col"), attr(tf, "lat_col"))]
#   return(ctf)
# }

#' Convert a Track Frame to Simple Features (sf) Object
#'
#' This function converts a `track_frame` object into a Simple Features (sf) object,
#' enabling spatial analysis and visualization. The longitude and latitude columns
#' are used as coordinates for the sf object.
#'
#' @param tf A `track_frame` object containing the tracking data. Must have
#'           attributes specifying the longitude and latitude columns (`lon_col` and `lat_col`).
#' @param crs The coordinate reference system (CRS) to be used for the sf object.
#'            Defaults to EPSG code 4326 (WGS84).
#' @param ... Additional arguments to be passed to `st_as_sf`.
#' @return An sf object representing the spatial data contained in the `track_frame`.
#' @examples
#' # Assuming `tf` is a valid track_frame with longitude and latitude attributes:
#' tf <- sim_travel_path(100, format = "track_frame")
#' sf_object <- tf_to_sf(tf, crs = 4326)
#' plot(sf_object)
#' @export
tf_to_sf <- function(tf, crs = 4326, ...) {
  assert_class(tf , "track_frame")
  coords <- c(attr(tf, "lon_col"), attr(tf, "lat_col"))
  st_as_sf(x = tf, crs = 4326, coords = coords, ...)
}
