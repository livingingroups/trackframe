# accessor for cols

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

crs <- function(tf) {
  attr(tf, "crs")
}

crs_type <- function(tf) {
  attr(tf, "crs_type")
}


#' Extract Index from a Track Frame
#'
#' This function retrieves the time index values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing time index values.
#'
#' @param x A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the time column (`time`).
#' @param ... ...
#' @return A vector of time index values extracted from the \code{trackframe}.
#'
#' @examples
#' time(tf_mini)
#'
#' @export
#' @rdname tf_accessor
time.trackframe <- function(x, ...) {
  x[[attr(x, "time")]]
}


# # FIXME: Do we need this?
# "time<-.trackframe" <- function(tf, value) {
#   assert_class(tf, "trackframe")
#   tf[[attr(tf, "time")]] <- value
#   tf
# }


#' Extract Track ID from a Track Frame
#'
#' This function retrieves the track ID values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing track ID values.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of track ID values extracted from the \code{trackframe}.
#'
#' @examples
#' id(tf_mini)
#'
#' @export
#' @rdname tf_accessor
id <- function(tf) {
  assert_class(tf, "trackframe")
  id_col <- attr(tf, "id")
  if (is.null(id_col)) NULL else tf[[id_col]]
}


# FIXME: Do we need this?
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
#' This function retrieves the easting values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing easting values.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the easting column (`easting`).
#' @return A vector of easting values extracted from the \code{trackframe}.
#'
#' @examples
#' easting(tf_mini)
#'
#' @export
#' @rdname tf_accessor
easting <- function(tf) {
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
#' This function retrieves the northing values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing northing values.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the northing column (`northing`).
#' @return A vector of northing values extracted from the \code{trackframe}.
#'
#' @examples
#' northing(tf_mini)
#'
#' @export
#' @rdname tf_accessor
northing <- function(tf) {
  assert_class(tf, "trackframe")
  tf[[attr(tf, "northing")]]
}

#' Extract Coordinate Reference System from a Track Frame
#'
#' This function retrieves the crs value from a \code{trackframe} object.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#' @return A representation of crs extracted from the \code{trackframe}.
#'  If crs_type is geographic or projected, then crs is a valid input to sf::st_crs.
#'  If crs_type is nongeoreferenced, then crs can be any non-null value including NA.
#'
#' @examples
#' crs(tf_mini)
#'
#' @export
#' @rdname tf_accessor
crs <- function(tf) {
  assert_class(tf, "trackframe")
  attr(tf, "crs")
}

#' Extract Coordinate Reference System Type from a Track Frame
#'
#' This function retrieves the crs value from a \code{trackframe} object.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#' @return One of:
#'  \code{geographic} (longlat)
#'  \code{projected} e.g. utm
#'  \code{nongeoreferenced} designed for use in captive or simulated scenarios
#'
#' @examples
#' crs_type(tf_mini)
#'
#' @export
#' @rdname tf_accessor
crs_type <- function(tf) {
  assert_class(tf, "trackframe")
  attr(tf, "crs_type")
}


#' Extract Unique IDs from a Track Frame
#'
#' This function retrieves the unique track IDs from a \code{trackframe} object.
#' The \code{trackframe} should be a data frame-like structure with an attribute
#' specifying the column containing track IDs.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of unique track IDs extracted from the \code{trackframe}.
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
#' This function filters a trackframe object to include only tracks with the specified ID(s).
#' It supports selecting a single ID or multiple IDs simultaneously.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#' @param id A character or vector of characters representing the track ID(s) to select.
#'
#' @return A filtered \code{trackframe} containing only the specified track(s).
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
#' This function splits a trackframe into a list of trackframes by the id.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#'
#' @return an object of class `list_of_trackframes` containing a \code{trackframe}
#' in each list element.
#'
#' @examples
#' tf_split <- split_by_id(tf_mini)
#' class(tf_split)
#' @export
split_by_id <- function(tf) {
  if (is.null(id(tf))) stop("No id specified for trackframe.")
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
  easting_guess <- guess_a_col(col_names, easting_col_candidates, id = "easting")
  northing_guess <- guess_a_col(col_names, northing_col_candidates, id = "northing")
  id_guess <- id_col_candidates[id_col_candidates %in% col_names]
  if (length(id_guess) == 0) id_guess <- NA


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
      if (!all(duplicated(t(data[, colnames(data) %in% guesses_col, with = FALSE]))[-1])) {
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
    })
  class(id_list) <- "c_grouping"
  attr(id_list, "active_group") <- group_names
  attr(id_list, "sort_index") <- as.factor(sapply(id_list, paste, collapse = "_"))
  id_list
}
