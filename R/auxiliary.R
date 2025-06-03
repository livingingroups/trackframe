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
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the index column (`index`).
#' @return A vector of index values extracted from the `track_frame`.
#' @examples
#' tf <- sim_travel_path(100, format = "track_frame")
#' index_values <- index(tf)
#' print(index_values)
#' @export
time_index <- function(tf){
  tf[[attr(tf, "time_index")]]
}


#' Extract Track ID from a Track Frame
#'
#' This function retrieves the track ID values from a `track_frame` object.
#' The `track_frame` should be a data frame-like structure with an attribute
#' specifying the column containing track ID values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`track_id`).
#' @return A vector of track ID values extracted from the `track_frame`.
#' @examples
#' tf <- sim_travel_paths(3, c(2, 4, 5))
#' track_id_values <- track_id(tf)
#' print(track_id_values)
#' @export
track_id <- function(tf){
  assert_class(tf, "track_frame")
  tf[[attr(tf, "track_id")]]
}


#easting

#' Extract Easting from a Track Frame
#'
#' This function retrieves the easting values from a `track_frame` object.
#' The `track_frame` should be a data frame-like structure with an attribute
#' specifying the column containing easting values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the easting column (`easting_col`).
#' @return A vector of easting values extracted from the `track_frame`.
#' @examples
#' tf <- sim_travel_path(100, format = "track_frame")
#' easting_values <- easting(tf)
#' print(easting_values)
#' @export
easting <- function(tf){
  assert_class(tf, "track_frame")
  tf[[attr(tf, "easting")]]
}

#' Extract Northing from a Track Frame
#'
#' This function retrieves the northing values from a `track_frame` object.
#' The `track_frame` should be a data frame-like structure with an attribute
#' specifying the column containing northing values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the northing column (`northing_col`).
#' @return A vector of northing values extracted from the `track_frame`.
#' @examples
#' tf <- sim_travel_path(100, format = "track_frame")
#' northing_values <- northing(tf)
#' print(northing_values)
#' @export
northing <- function(tf){
  assert_class(tf, "track_frame")
  tf[[attr(tf, "northing")]]
}


#' Extract Unique IDs from a Track Frame
#'
#' This function retrieves the unique track IDs from a `track_frame` object.
#' The `track_frame` should be a data frame-like structure with an attribute
#' specifying the column containing track IDs.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`track_id`).
#' @return A vector of unique track IDs extracted from the `track_frame`.
#' @examples
#' tf <- sim_travel_path(100, format = "track_frame")
#' unique_ids <- unique_ids(tf)
#' print(unique_ids)
#' @export
unique_ids <- function(tf) {
  assert_class(tf, "track_frame")
  ids <- unique(tf[, attr(tf, "track_id")])
  if (is.null(dim(ids))) {
    ids <- data.frame(
      ids
    )
    names(ids) <- attr(tf, "track_id")
  }
  ids
}


#select id of trackframe
# id can also be a dataframe


#' Select Tracks by ID from a Track Frame
#'
#' This function filters a track_frame object to include only tracks with the specified ID(s).
#' It supports selecting a single ID or multiple IDs simultaneously.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#' @param id A character or vector of characters representing the track ID(s) to select.
#'
#' @return A filtered `track_frame` containing only the specified track(s).
#'
#' @examples
#' tf <- sim_travel_path(100, format = "track_frame")
#' single_track <- select_id(tf, "track1")
#' multiple_tracks <- select_id(tf, c("track1", "track2"))
#' @export 
select_id <- function(tf, id) {
  assert_class(tf, "track_frame")
  # tf <- FFT_tf
  # id <- "Abby"
  # id <- c("Abby", "4652")
  
  if(length(id) > 1) {
    tf <- tf[do.call(paste0, tf[, attr(tf, "track_id")]) %in% paste0(id, collapse = ""), ]
  } else { #TODO we need more sophisticated check here
  tf <- tf[tf[, attr(tf, "track_id")] == id, ]
  }
  return(tf)
}



#' Convert a Track Frame to XYT Format
#'
#' This function extracts the core spatial-temporal data from a track_frame object,
#' returning a simplified data frame with just the easting (X), northing (Y), 
#' timestamp (T), and optionally the track ID columns.
#'
#' @param x A `track_frame` object containing the tracking data.
#' @return A data frame with the easting, northing, time index, and optionally track ID columns.
#' @export 
tf_to_xyt <- function(x){ #coredata.track_frame
  #TODO check what we want to do in coredata
  if(!is.null(attr(x, "track_id"))){
    ctf <- x[, c(attr(x, "easting"), attr(x, "northing"), attr(x, "time_index"), attr(x, "track_id"))]
  } else {
    ctf <- x[, c(attr(x, "easting"), attr(x, "northing"), attr(x, "time_index"))]
  }
  class(ctf) <- "data.frame"
  return(ctf)
}

#' Convert a Track Frame to Simple Features (sf) Object
#'
#' This function converts a `track_frame` object into a Simple Features (sf) object,
#' enabling spatial analysis and visualization. The easting and northing columns
#' are used as coordinates for the sf object.
#'
#' @param tf A `track_frame` object containing the tracking data. Must have
#'           attributes specifying the easting and northing columns (`easting_col` and `northing_col`).
#' @param crs The coordinate reference system (CRS) to be used for the sf object.
#'            Defaults to EPSG code 4326 (WGS84).
#' @param ... Additional arguments to be passed to `st_as_sf`.
#' @return An sf object representing the spatial data contained in the `track_frame`.
#' @examples
#' tf <- sim_travel_path(100, format = "track_frame")
#' sf_object <- tf_to_sf(tf, crs = 4326)
#' plot(sf_object)
#' @export
tf_to_sf <- function(tf, crs = 4326, ...) {
  assert_class(tf , "track_frame")
  coords <- c(attr(tf, "easting"), attr(tf, "northing"))
  st_as_sf(x = tf, crs = 4326, coords = coords, ...)
}
