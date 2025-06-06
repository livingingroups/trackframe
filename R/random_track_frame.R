#' Generate Random Travel Path
#'
#' This function generates a random travel path with easting, northing, and time values.
#' The path can include stationary periods and movements with configurable parameters.
#'
#' @param size An integer giving the number of points to generate in the path.
#' @param max_step A numeric giving the maximum step size in degrees for each movement. Default is 0.001.
#' @param time_increment A numeric giving the time between consecutive points in seconds. Default is 60 (1 minute).
#' @param start_location A numeric vector giving the starting location as c(northing, easting). Default is Vienna (48.2083537, 16.3725042).
#' @param start_time A POSIXct giving the starting time for the path. Default is current time.
#' @param stay_prob A numeric giving the probability of staying at the same location (0-1). Default is 0.2.
#' @param format A character string, either "data.frame" or "matrix". Default is "data.frame".
#'
#' @return Depending on the format argument either a \code{"track\_frame"} or
#'  \code{"data.frame"} or \code{"matrix"}.
#'
#' @examples
#' data <- sim_travel_path(100, format = "matrix")
#' @export
sim_travel_path <- function(size,
                         max_step = 0.001,
                         time_increment = 60, # in seconds
                         start_location = c(48.2083537, 16.3725042),
                         start_time = Sys.time(),
                         stay_prob = 0.2,
                         format = c("track_frame", "data.frame", "matrix")) {
  checkmate::assert_integerish(size, lower = 2, any.missing = FALSE)
  checkmate::assert_numeric(max_step, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(start_location, len = 2, any.missing = FALSE)
  checkmate::assert_posixct(start_time, any.missing = FALSE)
  checkmate::assert_numeric(stay_prob, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  format <- match.arg(format)
  
  path <- data.frame(
    northing = numeric(size),
    easting = numeric(size),
    time = as.POSIXct(rep(NA, size))
  )
  
  path$northing[1] <- start_location[1]
  path$easting[1] <- start_location[2]
  path$time[1] <- start_time
  
  for (i in 2:size) {
    if (runif(1) < stay_prob) {
      path$northing[i] <- path$northing[i-1]
      path$easting[i] <- path$easting[i-1]
      path$time[i] <- path$time[i-1] + round(runif(1, 1, 30)) *  time_increment
    } else {
      dnorthing <- runif(1, -max_step, max_step)
      deasting <- runif(1, -max_step, max_step)
      
      new_northing <- path$northing[i-1] + dnorthing
      new_easting <- path$easting[i-1] + deasting
      
      path$northing[i] <- new_northing
      path$easting[i] <- new_easting
      path$time[i] <- path$time[i-1] + time_increment
    }    
  }
  
  if (format == "matrix") {
    path$time <- as.integer(path$time)
    return(as.matrix(path))
  } else if(format == "track_frame") {
    return(as.track_frame(data = path,
                          time_col = "time",
                          easting_col = "easting",
                          northing_col = "northing"))
    } else {
    return(path)
  }
}


#' Generate Multiple Random Travel Paths
#'
#' This function creates multiple random travel paths and combines them into a single
#' \code{track\_frame} object. Each path is assigned a unique track ID.
#'
#' @param ntracks An integer specifying the number of tracks to generate.
#' @param sizes An integer vector specifying the number of points for each track. If a single value
#'   is provided, it will be repeated for all tracks.
#' @param max_step A numeric value specifying the maximum step size for random movements.
#'   Default is 0.001 (approximately 100m at the equator).
#' @param time_increment A numeric giving the time between consecutive points in seconds.
#'   Default is 60 (1 minute).
#' @param start_location A numeric vector giving the starting location as northing, easting.
#' @param start_time A POSIXct giving the starting time for the paths. Default is current time.
#' @param stay_prob A numeric between 0 and 1 giving the probability of staying at the same location.
#'   Default is 0.2.
#' @param track_prefix A character string used as a prefix for track IDs. Default is "track".
#'   Track IDs will be formatted as \code{prefix\_number}.
#'
#' @return A \code{track\_frame} object.
#'
#' @examples
#' # Generate 3 tracks with different sizes
#' ntracks <- 3
#' sizes <- c(2, 4, 5)
#' multi_track <- sim_travel_paths(ntracks, sizes)
#'
#' # Generate 5 tracks all with the same size
#' uniform_tracks <- sim_travel_paths(5, 10)
#'
#' # Extract a specific track
#' track2 <- select_id(multi_track, "track_2")
#' @export
sim_travel_paths <- function(ntracks,
                             sizes,
                             max_step = 0.001,
                             time_increment = 60, # in seconds
                             start_location = c(48.2083537, 16.3725042),
                             start_time = Sys.time(),
                             stay_prob = 0.2,
                             track_prefix = "track") {
  checkmate::assert_integerish(ntracks, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(sizes, lower = 1, any.missing = FALSE)
  if (length(sizes) == 1L) {
    sizes <- rep.int(sizes, ntracks)
  }
  checkmate::assert_integerish(sizes, lower = 1, any.missing = FALSE, len = ntracks)
  checkmate::assert_numeric(max_step, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(start_location, len = 2, any.missing = FALSE)
  checkmate::assert_posixct(start_time, any.missing = FALSE)
  checkmate::assert_numeric(stay_prob, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  
  total_size <- sum(sizes)
  tf <- data.frame(
    id = character(total_size),
    easting = numeric(total_size),
    northing = numeric(total_size),
    time = as.POSIXct(rep(NA, total_size))
  )

  start <- 1L
  for (i in seq_len(ntracks)) {
    end <- start + sizes[i] - 1L
    idx <- seq.int(start, end)
    x <- sim_travel_path(size = sizes[i], max_step = max_step, time_increment = time_increment,
                         start_location = start_location, start_time = start_time,
                         stay_prob = stay_prob, format = "data.frame")
    for (col in colnames(x)) {
      tf[idx, col] <- x[[col]]
    }
    tf[idx, "id"] <- sprintf("%s_%i", track_prefix, i)
    start <- end + 1L
  }

  return(as.track_frame(tf,
                        id_col = "id",
                        time_col = "time",
                        easting_col = "easting",
                        northing_col = "northing"))
}
