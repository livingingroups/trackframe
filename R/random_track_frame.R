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
#' @param format A character string, either "track_frame", "data.frame", "matrix", "sftrack" or "move2".
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
                         format = c("track_frame", "data.frame", "matrix", "sftrack", "move2")) {
  checkmate::assert_integerish(size, lower = 2, any.missing = FALSE)
  checkmate::assert_numeric(max_step, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(start_location, len = 2, any.missing = FALSE)
  checkmate::assert_posixct(start_time, any.missing = FALSE)
  checkmate::assert_numeric(stay_prob, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  format <- match.arg(format)
  
  path <- data.frame(
    time = as.POSIXct(rep(NA, size)),
    latitude = numeric(size),
    longitude = numeric(size)
  )
  
  path$time[1] <- start_time
  path$latitude[1] <- start_location[2]
  path$longitude[1] <- start_location[1]

  
  for (i in 2:size) {
    if (runif(1) < stay_prob) {
      path$longitude[i] <- path$longitude[i-1]
      path$latitude[i] <- path$latitude[i-1]
      path$time[i] <- path$time[i-1] + round(runif(1, 1, 30)) *  time_increment
    } else {
      dlongitude <- runif(1, -max_step, max_step)
      dlatitude <- runif(1, -max_step, max_step)
      
      new_longitude <- path$longitude[i-1] + dlongitude
      new_latitude <- path$latitude[i-1] + dlatitude
      
      path$longitude[i] <- new_longitude
      path$latitude[i] <- new_latitude
      path$time[i] <- path$time[i-1] + time_increment
    }    
  }
  
  if (format == "matrix") {
    path$id <- 1
    path$time <- as.integer(path$time)
    return(as.matrix(path))
  } else if (format == "sftrack") {
    as_sftrack <- try(getNamespace("sftrack")$as_sftrack, silent = TRUE)
    if (inherits(as_sftrack, "try-error")) {
      stop("package 'sftrack' is required for this function. Please install it.")
    }
    data <- cbind(path, "id" = "track_1")
    return(as_sftrack(data,
                      coords = c("latitude", "longitude"),
                      crs = 4326))
  } else if (format == "move2") {
    mt_as_move2 <- try(getNamespace("move2")$mt_as_move2, silent = TRUE)
    if (inherits(mt_as_move2, "try-error")) {
      stop("package 'move2' is required for this function. Please install it.")
    }
    data <- cbind(path, "id" = "track_1")
    return(mt_as_move2(data, coords = c("latitude", "longitude"), time_column = "time", track_id_column = "id", crs = 4326))
    
  } else if(format == "track_frame") {
    data <- cbind(path, "id" = "track_1")
    as_sftrack <- try(getNamespace("sftrack")$as_sftrack, silent = TRUE)
    if (inherits(as_sftrack, "try-error")) {
      stop("package 'sftrack' is required for this function. Please install it.")
    }
    data <- as_sftrack(data,
                      coords = c("latitude", "longitude"),
                      crs = 4326)
    data_tf <- as.track_frame(data = data,
                   time_col = "time",
                   easting_col = "easting",
                   northing_col = "northing")
    
    data_tf[["sft_group"]] <- NULL
    data_tf[["id"]] <- "track_1"
    data_tf[["geometry"]] <- NULL
    return(data_tf)
  } else {
    data <- cbind(path, "id" = "track_1")
    return(data)
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
#' @param format A character string, either "track_frame", "data.frame", "matrix", "sftrack" or "move2".
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
                             track_prefix = "track",
                             format = c("track_frame", "data.frame", "matrix", "sftrack", "move2")) {
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
  format <- match.arg(format)
  
  total_size <- sum(sizes)
  data <- data.frame(
    time = as.POSIXct(rep(NA, total_size)),
    latitude = numeric(total_size),
    longitude = numeric(total_size),
    id = character(total_size)
  )

  start <- 1L
  for (i in seq_len(ntracks)) {
    end <- start + sizes[i] - 1L
    idx <- seq.int(start, end)
    x <- sim_travel_path(size = sizes[i], max_step = max_step, time_increment = time_increment,
                         start_location = start_location, start_time = start_time,
                         stay_prob = stay_prob, format = "data.frame")
    for (col in colnames(x)) {
      data[idx, col] <- x[[col]]
    }
    data[idx, "id"] <- sprintf("%s_%i", track_prefix, i)
    start <- end + 1L
  }

  # return(as.track_frame(tf,
  #                       id_col = "id",
  #                       time_col = "time",
  #                       easting_col = "easting",
  #                       northing_col = "northing"))
  
  if (format == "matrix") {
    data[["id"]] <- as.integer(as.factor(data[["id"]]))
    data[["time"]] <- as.integer(data[["time"]])
    return(as.matrix(data))
  } else if (format == "sftrack") {
    as_sftrack <- try(getNamespace("sftrack")$as_sftrack, silent = TRUE)
    if (inherits(as_sftrack, "try-error")) {
      stop("package 'sftrack' is required for this function. Please install it.")
    }
    
    return(as_sftrack(data,
                      coords = c("latitude", "longitude"),
                      crs = 4326))
  } else if (format == "move2") {
    mt_as_move2 <- try(getNamespace("move2")$mt_as_move2, silent = TRUE)
    if (inherits(mt_as_move2, "try-error")) {
      stop("package 'move2' is required for this function. Please install it.")
    }
    return(mt_as_move2(data, coords = c("latitude", "longitude"), time_column = "time", track_id_column = "id", crs = 4326))
    
  } else if(format == "track_frame") {
    as_sftrack <- try(getNamespace("sftrack")$as_sftrack, silent = TRUE)
    if (inherits(as_sftrack, "try-error")) {
      stop("package 'sftrack' is required for this function. Please install it.")
    }
    data <- as_sftrack(data,
                       coords = c("latitude", "longitude"),
                       crs = 4326)
    
    data_tf <- as.track_frame(data = data,
                   time_col = "time",
                   easting_col = "easting",
                   northing_col = "northing")
    
    data_tf[["sft_group"]] <- NULL
    data_tf[["id"]]  <- data[["id"]] 
    data_tf[["geometry"]] <- NULL
    
    return(data_tf)
  } else {
    return(data)
  }
  
  
}
