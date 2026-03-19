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
#' @seealso [as.trackframe()]
#'
#' @examples
#' unique_ids(tf_mini)
#'
#' @export
unique_ids <- function(tf) {
  unique(id(tf))
}


#' Select Tracks by ID from a Track Frame
#'
#' This function filters a \code{trackframe} object to include only tracks with the specified ID(s).
#' It supports selecting a single ID or multiple IDs simultaneously.
#'
#' @param tf A `trackframe` object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#' @param id A character or vector of characters representing the track ID(s) to select.
#'
#' @return A filtered `trackframe` containing only the specified track(s).
#'
#' @examples
#' single_track <- select_id(tf_mini, "track_1")
#' single_track
#' multiple_tracks <- select_id(tf_mini, c("track_2", "track_3"))
#' multiple_tracks
#' @export
select_id <- function(tf, id) {
  checkmate::assert_class(tf, "trackframe")
  tf[id(tf) %in% id, , drop = FALSE, with = FALSE]
}

#' Split trackframe by ID
#'
#' This function splits a \code{trackframe} object into a list of trackframes by the id.
#'
#' @param tf A `trackframe` object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#'
#' @return an object of class `list_of_trackframes` containing a `trackframe`
#' in each list element.
#'
#' @examples
#' tf_split <- split_by_id(tf_mini)
#' class(tf_split)
#' @export
split_by_id <- function(tf) {
  tf_split <- split(tf, id(tf))
  class(tf_split) <- "list_of_trackframes"
  tf_split
}

make_unique_id <- function(id_col) {
  unique_id <- sapply(id_col, paste, collapse = "<;>")
  attr(unique_id, "group_names") <- attr(id_col, "active_group")
  unique_id
}

backtransform_id <- function(unique_id, group_names) {
  id_list <- lapply(
    str_split(unique_id, pattern = "<;>"),
    function(x) {
      id_i <- setNames(as.list(x), group_names)
      class(id_i) <- "s_group"
      id_i
    }
  )
  class(id_list) <- "c_grouping"
  attr(id_list, "active_group") <- group_names
  attr(id_list, "sort_index") <- as.factor(sapply(
    id_list,
    paste,
    collapse = "_"
  ))
  id_list
}

if (getRversion() <= "4.4.0") {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

id_hash <- function(
  data,
  time_col = attr(data, "time"),
  id_col = attr(data, "id")
) {
  if (is.null(id_col)) {
    cols <- time_col
  } else {
    cols <- c(time_col, id_col)
  }
  apply(data[, cols, with = FALSE], 1, digest)
}


#' Sorting of Trackframes
#'
#' Sort a trackframe into ascending or descending order, by track ID and time.
#'If no id column is available only sorted by time.
#'
#' @param x an object of class trackframe
#' @param decreasing logical indicating if sort should be increasing or decreasing.
#' @param ... additional arguments passed to order
#'
#' @return an object of class trackframe
#' @export
#' @examples
#' sort(
#'   as.trackframe(
#'     data.frame(
#'       x = 1:6,
#'       y = 1:6,
#'       t = as.POSIXct(c(2, 2, 2, 1, 1, 1)),
#'       id = c("Asa", "Betty", "Charlie", "Asa", "Betty", "Charlie")
#'     ),
#'     crs = NA
#'   )
#' )

sort.trackframe <- function(x, decreasing = FALSE, ...) {
  x[order(id(x), time(x), decreasing = decreasing, ...), ]
}


#' Obtain starting points
#'
#' This function obtains starting points for all tracks in a trackframe object.
#'
#' @param tf an object of class trackframe
#'
#' @return a trackframe providing the x and y coordinates of the starting points all
#' different tracks sorted by id (if available) and time.
#'
#' @examples
#' library(trackframe)
#' get_starting_points(tf_mini)
#'
#' @export
get_starting_points <- function(tf) {
  assert_class(tf, "trackframe")
  tf <- sort(tf)
  tf <- tf[
    !duplicated(tf[, c(
      easting_col(tf),
      northing_col(tf),
      id_col(tf)
    )]),
  ]
  starting_points <- tf[!duplicated(id(tf)), , drop = FALSE]
  rownames(starting_points) <- id(starting_points)
  return(starting_points)
}

#' Obtain direction points
#'
#' This function obtains direction points (representing the second data point) for all tracks of
#' objects of class trackframe.
#'
#' @param tf an object of class trackframe
#'
#' @return a trackframe providing the x and y coordinates of the direction points
#' of all different tracks sorted by id (if available) and time.
#'
#' @examples
#' library(trackframe)
#' get_direction_points(tf_mini)
#'
#' @export
get_direction_points <- function(tf) {
  assert_class(tf, "trackframe")
  tf <- sort(tf)
  tf <- tf[
    !duplicated(tf[, c(easting_col(tf), northing_col(tf), id_col(tf))]),
  ]
  tf <- tf[duplicated(tf[[id_col(tf)]]), ]
  direction_points <- tf[!duplicated(tf[[id_col(tf)]]), , drop = FALSE]
  rownames(direction_points) <- id(direction_points)
  return(direction_points)
}
