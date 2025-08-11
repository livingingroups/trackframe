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
#' tf <- travelpaths::sim_travel_path(100, format = "trackframe")
#' time(tf)
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
#' This function retrieves the track ID values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing track ID values.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of track ID values extracted from the \code{trackframe}.
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
#' This function retrieves the easting values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing easting values.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the easting column (`easting`).
#' @return A vector of easting values extracted from the \code{trackframe}.
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
#' This function retrieves the northing values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing northing values.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the northing column (`northing`).
#' @return A vector of northing values extracted from the \code{trackframe}.
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
#' This function retrieves the unique track IDs from a \code{trackframe} object.
#' The \code{trackframe} should be a data frame-like structure with an attribute
#' specifying the column containing track IDs.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of unique track IDs extracted from the \code{trackframe}.
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
#' @param tf A \code{trackframe} object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#' @param id A character or vector of characters representing the track ID(s) to select.
#'
#' @return A filtered \code{trackframe} containing only the specified track(s).
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
#' @param tf A \code{trackframe} object containing the tracking data.
#'   Must have an attribute indicating the track ID column.
#'
#' @return an object of class `list_of_trackframes` containing a \code{trackframe} in each list element.
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


#' Guesses columns
#'
#' @param col_names vector of column names of the input data
#' @param time_col_candidates a vector of candidates for time column. Typically provided in tf_options("time_col").
#' @param easting_col_candidates a vector of candidates for easting column. Typically provided in tf_options("easting_col").
#' @param northing_col_candidates a vector of candidates for northing column. Typically provided in tf_options("northing_col").
#' @param id_col_candidates a vector of candidates for id column. Typically provided in tf_options("id_col").
#'
#' @return a list of guesses
#' @export
#'
#' @examples
col_guessing <- function(col_names,
                         time_col_candidates = tf_options("time_col"),
                         easting_col_candidates = tf_options("easting_col"),
                         northing_col_candidates = tf_options("northing_col"),
                         id_col_candidates = tf_options("id_col")) {
  time_guess <- guessing(col_names, time_col_candidates, id = "time")
  easting_guess <- guessing(col_names, easting_col_candidates, id = "easting")
  northing_guess <- guessing(col_names, northing_col_candidates, id = "northing")
  id_guess <- id_col_candidates[id_col_candidates %in% col_names]
  if(length(id_guess) == 0) id_guess <- NA
  
  
  return(list("time_col" = time_guess,
              "easting_col" = easting_guess,
              "northing_col" = northing_guess,
              "id_col" = id_guess))
  
}

guessing <- function(col_names, candidates, id) {
  ind <- candidates %in% col_names
  if(sum(ind) < 1) stop(
    sprintf("%s needs to be specified. Guessing not successful.", id)
  )
  # chosen_guess <- candidates[ind][1]
  # if (sum(ind) > 1) {
  #   warning(sprintf("multiple possible %s columns found, %s chosen", id, chosen_guess))
  # }
  return(candidates[ind])
}

validate_guesses <- function(data, guesses){
  for(guessable_col in names(guesses)) {
    guesses_col <- guesses[[guessable_col]]
    chosen_guess <- guesses_col[1]
    if(length(guesses_col) > 1) {
      if(!all(duplicated(t(data[, colnames(data) %in% guesses_col]))[-1])) {
        warning(sprintf("multiple possible columns found. %s chosen as %s", chosen_guess, guessable_col))
      }
    }
  }
}

subset_guesses <- function(data,
                           time_col = tf_options("time_col"),
                           easting_col = tf_options("easting_col"),
                           northing_col = tf_options("northing_col"),
                           id_col = tf_options("id_col")) {
  guesses <- col_guessing(col_names = colnames(data),
                          time_col_candidates = time_col,
                          easting_col_candidates = easting_col,
                          northing_col_candidates = northing_col,
                          id_col_candidates = id_col)
  validate_guesses(data, guesses)
  if (is.na(guesses[["id_col"]][1])) {
    return(data[, c(guesses[["time_col"]][1],
                    guesses[["easting_col"]][1],
                    guesses[["northing_col"]][1])])
  } else {
    return(data[, c(guesses[["time_col"]][1],
                    guesses[["easting_col"]][1],
                    guesses[["northing_col"]][1],
                    guesses[["id_col"]][1])])
  }
}