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
