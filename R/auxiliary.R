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
#' This function retrieves the time index values from a `track_frame` object.
#' The `track_frame` has a data frame-like structure with an attribute
#' specifying the column containing time index values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the time column (`time`).
#' @return A vector of time index values extracted from the `track_frame`.
#' 
#' @examples
#' tf <- sim_travel_path(100, format = "track_frame")
#' time(tf)
#' 
#' 
#' @export
time <- function(tf) {
  UseMethod("time")
}

#' @noRd
#' @export
time.track_frame <- function(tf) {
  tf[[attr(tf, "time")]]
}


# # TODO: Do we need this?
# "time<-.track_frame" <- function(tf, value) {
#   assert_class(tf, "track_frame")
#   tf[[attr(tf, "time")]] <- value
#   tf
# }


#' Extract Track ID from a Track Frame
#'
#' This function retrieves the track ID values from a `track_frame` object.
#' The `track_frame` has a data frame-like structure with an attribute
#' specifying the column containing track ID values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of track ID values extracted from the `track_frame`.
#' 
#' @examples
#' tf <- sim_travel_paths(3, c(2, 4, 5))
#' id(tf)
#' 
#' @export
id <- function(tf) {
  assert_class(tf, "track_frame")
  id_col <- attr(tf, "id")
  if (is.null(id_col)) {
    return(NULL)
  } else {
    tf[[id_col]]
  }
}


# TODO: Do we need this?
"id<-" <- function(tf, value) {
  assert_class(tf, "track_frame")
  id_col <- attr(tf, "id")
  if (is.null(id_col)) {
    attr(tf, "id") <- id_col <- "id"
  }
  tf[[id_col]] <- value
  tf
}


#' Extract Easting from a Track Frame
#'
#' This function retrieves the easting values from a `track_frame` object.
#' The `track_frame` has a data frame-like structure with an attribute
#' specifying the column containing easting values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the easting column (`easting`).
#' @return A vector of easting values extracted from the `track_frame`.
#' 
#' @examples
#' tf <- sim_travel_path(10, format = "track_frame")
#' easting(tf)
#' 
#' @export
easting <- function(tf){
  assert_class(tf, "track_frame")
  tf[[attr(tf, "easting")]]
}


# TODO: Do we need this?
"easting<-" <- function(tf, value) {
  assert_class(tf, "track_frame")
  tf[[attr(tf, "easting")]] <- value
  tf
}


#' Extract Northing from a Track Frame
#'
#' This function retrieves the northing values from a `track_frame` object.
#' The `track_frame` has a data frame-like structure with an attribute
#' specifying the column containing northing values.
#'
#' @param tf A `track_frame` object containing the tracking data.
#'           Must have an attribute indicating the northing column (`northing`).
#' @return A vector of northing values extracted from the `track_frame`.
#'
#' @examples
#' tf <- sim_travel_path(10, format = "track_frame")
#' northing(tf)
#' 
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
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of unique track IDs extracted from the `track_frame`.
#'
#' @examples
#' tf <- sim_travel_paths(4, 2:5)
#' unique_ids(tf)
#' 
#' @export
unique_ids <- function(tf) {
  unique(id(tf))
}


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
#' tf <- sim_travel_paths(3, 2:4)
#' single_track <- select_id(tf, "track_1")
#' single_track
#' multiple_tracks <- select_id(tf, c("track_2", "track_3"))
#' multiple_tracks
#' @export 
select_id <- function(tf, id) {
  checkmate::assert_class(tf, "track_frame")
  if (is.null(attr(tf, "id"))) {
    stop("track_frame does not store an id column")
  }
  idx <- tf[[attr(tf, "id")]] %in% id
  return(tf[idx, , drop = FALSE, with = FALSE])
}


#' Convert a Track Frame to XYT Format
#'
#' This function extracts the core spatial-temporal data from a track_frame object,
#' returning a simplified data frame with just the easting (x), northing (y), 
#' time (t), and optionally the track ID columns.
#'
#' @param tf A `track_frame` object containing the tracking data.
#' @return A data frame with the easting, northing, time index, and optionally track ID columns.
#' tf <- sim_travel_paths(3, c(2, 4, 5))
#' tf_as_xyt(tf)
#' @export 
tf_as_xyt <- function(tf) { #coredata.track_frame
  #TODO check what we want to do in coredata
  assert_class(tf, "track_frame")
  if (is.null(attr(tf, "id"))) {
    cols <- c(attr(tf, "easting"), attr(tf, "northing"), attr(tf, "time"))
  } else {
    cols <- c(attr(tf, "easting"), attr(tf, "northing"), attr(tf, "time"), attr(tf, "id"))
  }
  x <- tf[, cols, with = FALSE]
  class(x) <- setdiff(class(x), "track_frame")
  return(x)
}


#' Convert a Track Frame to Simple Features (sf) Object
#'
#' This function converts a `track_frame` object into a Simple Features (sf) object,
#' enabling spatial analysis and visualization. The easting and northing columns
#' are used as coordinates for the sf object.
#'
#' @param tf A `track_frame` object containing the tracking data. Must have
#'           attributes specifying the easting and northing columns (`easting` and `northing`).
#' @param tf_crs The coordinate reference system (CRS) used in the `track_frame`.
#' @param crs_new The coordinate reference system (CRS) to be used for the sf object.
#' @param ... Additional arguments to be passed to `st_as_sf`.
#' @return An sf object representing the spatial data contained in the `track_frame`.
#' 
#' @examples
#' tf <- sim_travel_paths(4, 2:5)
#' sf_object <- tf_as_sf(tf, tf_crs = 32610, crs_new = 4326)
#' plot(sf_object)
#' @export
#' @rdname tf_as
tf_as_sf <- function(tf, tf_crs = NULL, crs_new = NULL, ...) {
  # NOTE: tf_as_sf to be consistent with the sf package.
  assert_class(tf, "track_frame")
  if(is.null(tf_crs)) {
    tf_crs <- utm_epsg(tf)
    if(is.null(tf_crs)) stop("no utm_epsg provided in track_frame. Please provide argument tf_crs")
  }
  coords <- c(attr(tf, "easting"), attr(tf, "northing"))
  new_sf <- sf::st_as_sf(x = tf, crs = tf_crs, coords = coords, ...)
  if(!is.null(crs_new)) {
    new_sf <- sf::st_transform(new_sf, crs_new)
}
  return(new_sf)
}


#' @export
#' @rdname tf_as
tf_as_sftrack <- function(tf, tf_crs = NULL, crs_new = NULL, ...) {
  # tf_crs <- 32610
  assert_class(tf, "track_frame")
  as_sftrack <- try(getNamespace("sftrack")$as_sftrack, silent = TRUE)
  if (inherits(as_sftrack, "try-error")) {
    stop("package 'sftrack' is required for this function. Please install it.")
  }
  # TODO: Should we drop NA?
  tf <- tf[!is.na(easting(tf)) & !is.na(northing(tf)),]
  sf_df <- tf_as_sf(tf = tf, tf_crs = tf_crs, crs_new = crs_new)
  # sf_df["sft_group"] <- lapply(id(tf), function(text) eval(parse(text = text)))
  # FIXME: Split does not work if not multiple groups are present.
  sft_group <- as.list(do.call(rbind.data.frame, lapply(id(tf), function(text) eval(parse(text = text)))))
  # FIXME: We want to create an sftrack object without importing it.
  new_sftrack <- as_sftrack(sf_df, group = sft_group, time = attr(tf, "time"), overwrite_names = TRUE)
  # new_sftrack[[attr(new_sftrack, "group_col")]]
  return(new_sftrack)
}


#' @export
#' @rdname tf_as
tf_as_move2 <- function(tf, tf_crs = NULL, crs_new = NULL, ...) {
  # tf_crs <- 32610
  assert_class(tf, "track_frame")
  mt_as_move2 <- try(getNamespace("move2")$mt_as_move2, silent = TRUE)
  if (inherits(mt_as_move2, "try-error")) {
    stop("package 'move2' is required for this function. Please install it.")
  }
  # TODO: Should we drop NA?
  tf <- tf[!is.na(easting(tf)) & !is.na(northing(tf)),]
  sf_df <- tf_as_sf(tf = tf, tf_crs = tf_crs, crs_new = crs_new)
  # FIXME: We want to create an move2 object without importing it.
  mt_as_move2(sf_df,
              time_column = attr(tf, "time"),
              track_id_column = attr(tf, "id"))
}


