#accessor for cols

easting_col <- function(tf) {
  attr(tf, "easting")
}

`easting_col<-` <- function(tf, value) {
  attr(tf, "easting") <- value
  tf
}

northing_col <- function(tf) {
  attr(tf, "northing")
}

time_col <- function(tf) {
  attr(tf, "time")
}


id_col <- function(tf) {
  attr(tf, "id")
}

utm_epsg <- function(tf) {
  attr(tf, "utm_epsg")
}


#' Extract Index from a Track Frame
#'
#' This function retrieves the time index values from a `trackframe` object.
#' The `trackframe` has a data frame-like structure with an attribute
#' specifying the column containing time index values.
#'
#' @param x A `trackframe` object containing the tracking data.
#'           Must have an attribute indicating the time column (`time`).
#' @param ... ...
#' @return A vector of time index values extracted from the `trackframe`.
#' 
#' @examples
#' tf <- travelpaths::sim_travel_path(100, format = "trackframe")
#' time(tf)
#' 
#' 
#' @export
#' @rdname tf_accessor
time.trackframe <- function(x, ...) {
  x[[attr(x, "time")]]
}


# # TODO: Do we need this?
# "time<-.trackframe" <- function(tf, value) {
#   assert_class(tf, "trackframe")
#   tf[[attr(tf, "time")]] <- value
#   tf
# }


#' Extract Track ID from a Track Frame
#'
#' This function retrieves the track ID values from a `trackframe` object.
#' The `trackframe` has a data frame-like structure with an attribute
#' specifying the column containing track ID values.
#'
#' @param tf A `trackframe` object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of track ID values extracted from the `trackframe`.
#' 
#' @examples
#' tf <- travelpaths::sim_travel_paths(3, c(2, 4, 5))
#' id(tf)
#' 
#' @export
#' @rdname tf_accessor
id <- function(tf) {
  assert_class(tf, "trackframe")
  id_col <- attr(tf, "id")
  if (is.null(id_col)) {
    return(NULL)
  } else {
    tf[[id_col]]
  }
}


# TODO: Do we need this?
"id<-" <- function(tf, value) {
  assert_class(tf, "trackframe")
  id_col <- attr(tf, "id")
  if (is.null(id_col)) {
    attr(tf, "id") <- id_col <- "id"
  }
  tf[[id_col]] <- value
  tf
}


#' Extract Easting from a Track Frame
#'
#' This function retrieves the easting values from a `trackframe` object.
#' The `trackframe` has a data frame-like structure with an attribute
#' specifying the column containing easting values.
#'
#' @param tf A `trackframe` object containing the tracking data.
#'           Must have an attribute indicating the easting column (`easting`).
#' @return A vector of easting values extracted from the `trackframe`.
#' 
#' @examples
#' tf <- travelpaths::sim_travel_path(10, format = "trackframe")
#' easting(tf)
#' 
#' @export
#' @rdname tf_accessor
easting <- function(tf){
  assert_class(tf, "trackframe")
  tf[[attr(tf, "easting")]]
}


# TODO: Do we need this?
"easting<-" <- function(tf, value) {
  assert_class(tf, "trackframe")
  tf[[attr(tf, "easting")]] <- value
  tf
}


#' Extract Northing from a Track Frame
#'
#' This function retrieves the northing values from a `trackframe` object.
#' The `trackframe` has a data frame-like structure with an attribute
#' specifying the column containing northing values.
#'
#' @param tf A `trackframe` object containing the tracking data.
#'           Must have an attribute indicating the northing column (`northing`).
#' @return A vector of northing values extracted from the `trackframe`.
#'
#' @examples
#' tf <- travelpaths::sim_travel_path(10, format = "trackframe")
#' northing(tf)
#' 
#' @export
#' @rdname tf_accessor
northing <- function(tf){
  assert_class(tf, "trackframe")
  tf[[attr(tf, "northing")]]
}


#' Extract Unique IDs from a Track Frame
#'
#' This function retrieves the unique track IDs from a `trackframe` object.
#' The `trackframe` should be a data frame-like structure with an attribute
#' specifying the column containing track IDs.
#'
#' @param tf A `trackframe` object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of unique track IDs extracted from the `trackframe`.
#'
#' @examples
#' tf <- travelpaths::sim_travel_paths(4, 2:5)
#' unique_ids(tf)
#' 
#' @export
unique_ids <- function(tf) {
  unique(id(tf))
}


#' Select Tracks by ID from a Track Frame
#'
#' This function filters a trackframe object to include only tracks with the specified ID(s).
#' It supports selecting a single ID or multiple IDs simultaneously.
#'
#' @param tf A `trackframe` object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#' @param id A character or vector of characters representing the track ID(s) to select.
#'
#' @return A filtered `trackframe` containing only the specified track(s).
#'
#' @examples
#' tf <- travelpaths::sim_travel_paths(3, 2:4)
#' single_track <- select_id(tf, "track_1")
#' single_track
#' multiple_tracks <- select_id(tf, c("track_2", "track_3"))
#' multiple_tracks
#' @export 
select_id <- function(tf, id) {
  checkmate::assert_class(tf, "trackframe")
  if (is.null(attr(tf, "id"))) {
    stop("trackframe does not store an id column")
  }
  idx <- tf[[attr(tf, "id")]] %in% id
  return(tf[idx, , drop = FALSE, with = FALSE])
}

#' Split trackframe by ID
#'
#' This function splits a trackframe into a list of trackframes by the id.
#'
#' @param tf A `trackframe` object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#'
#' @return an object of class `list_of_trackframes` containing a `trackframe` in each list element.
#'
#' @examples
#' tf <- travelpaths::sim_travel_paths(3, 3)
#' tf_split <- split_by_id(tf)
#' class(tf_split)
#' @export 
split_by_id <- function(tf) {
  if(is.null(id(tf))) stop("No id specified for trackframe.")
  tf_split <- split(tf, id(tf))
  class(tf_split) <- "list_of_trackframes"
  return(tf_split)
}