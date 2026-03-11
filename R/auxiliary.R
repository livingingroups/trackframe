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
  if (is.null(attr(tf, "id"))) {
    stop("trackframe does not store an id column")
  }
  idx <- tf[[attr(tf, "id")]] %in% id
  tf[idx, , drop = FALSE, with = FALSE]
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
  if (is.null(id(tf))) {
    stop("No id specified for trackframe.")
  }
  tf_split <- split(tf, id(tf))
  class(tf_split) <- "list_of_trackframes"
  tf_split
}


#' Guesses columns
#'
#' @param col_names vector of column names of the input data
#' @param time_col_candidates a vector of candidates for time column.
#' Typically provided in tf_options("time_col").
#' @param easting_col_candidates a vector of candidates for easting column.
#' Typically provided in tf_options("easting_col").
#' @param northing_col_candidates a vector of candidates for northing column.
#' Typically provided in tf_options("northing_col").
#' @param id_col_candidates a vector of candidates for id column.
#' Typically provided in tf_options("id_col").
#'
#' @return a list of guesses
#' @export
#'
#' @examples
#' data("path_trackframe")
#' path_trackframe$time_col <- path_trackframe$time
#' guess_all_cols(colnames(path_trackframe))
guess_all_cols <- function(
  col_names,
  time_col_candidates = tf_options("time_col"),
  easting_col_candidates = tf_options("easting_col"),
  northing_col_candidates = tf_options("northing_col"),
  id_col_candidates = tf_options("id_col")
) {
  time_guess <- guess_a_col(col_names, time_col_candidates, id = "time")
  easting_guess <- guess_a_col(
    col_names,
    easting_col_candidates,
    id = "easting"
  )
  northing_guess <- guess_a_col(
    col_names,
    northing_col_candidates,
    id = "northing"
  )
  id_guess <- id_col_candidates[id_col_candidates %in% col_names]
  if (length(id_guess) == 0) {
    id_guess <- NA
  }

  return(list(
    "time_col" = time_guess,
    "easting_col" = easting_guess,
    "northing_col" = northing_guess,
    "id_col" = id_guess
  ))
}

guess_a_col <- function(col_names, candidates, id) {
  ind <- candidates %in% col_names
  if (sum(ind) < 1) {
    stop(sprintf("%s needs to be specified. Guessing not successful.", id))
  }
  candidates[ind]
}

warn_if_guess_ambiguous <- function(data, guesses) {
  for (guessable_col in names(guesses)) {
    guesses_col <- guesses[[guessable_col]]
    chosen_guess <- guesses_col[1]
    if (length(guesses_col) > 1) {
      if (
        !all(duplicated(t(data[,
          colnames(data) %in% guesses_col, # nolint https://github.com/r-lib/lintr/issues/2960
          with = FALSE
        ]))[-1])
      ) {
        warning(sprintf(
          "multiple possible columns found. %s chosen as %s",
          chosen_guess,
          guessable_col
        ))
      }
    }
  }
}

subset_guesses <- function(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col")
) {
  guesses <- guess_all_cols(
    col_names = colnames(data),
    time_col_candidates = time_col,
    easting_col_candidates = easting_col,
    northing_col_candidates = northing_col,
    id_col_candidates = id_col
  )
  warn_if_guess_ambiguous(data, guesses)
  if (is.na(guesses[["id_col"]][1])) {
    data[, c(
      guesses[["time_col"]][1],
      guesses[["easting_col"]][1],
      guesses[["northing_col"]][1]
    )]
  } else {
    data[, c(
      guesses[["time_col"]][1],
      guesses[["easting_col"]][1],
      guesses[["northing_col"]][1],
      guesses[["id_col"]][1]
    )]
  }
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
  if (is.null(attr(x, "id"))) {
    x <- x[order(time(x), decreasing = decreasing, ...), ]
  } else {
    x <- x[order(id(x), time(x), decreasing = decreasing, ...), ]
  }
  return(x)
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
  starting_points <- tf[!duplicated(id(tf)), ]
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
  x <- attr(tf, "easting")
  y <- attr(tf, "northing")
  id <- attr(tf, "id")
  tf <- sort(tf)
  tf <- tf[!duplicated(tf[, c(x, y, id)]), ]
  tf <- tf[duplicated(tf[[id]]), ]
  direction_points <- tf[!duplicated(tf[[id]]), ]
  rownames(direction_points) <- direction_points[[id]]
  return(direction_points)
}
