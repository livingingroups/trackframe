
# #' Create a new Track Frame
# #'
# #' @param data A data.frame or tibble or data.table.
# #' @param index Column name of the index column
# #' @param easting Column name of the easting column
# #' @param northing Column name of the northing column
# #' @param alt Column name of the altitude column
# #' @param id Column name of the id column
# #' @return A track_frame object
# #' @export
# track_frame <- function(data,
#                         index,
#                         lon,
#                         lat,
#                         alt,
#                         id,
#                         ...) {
# 
#     stop("TODO")
#     cols <- c("id", "index", "lon", "lat", "alt")
#     cols <- c(intersect(cols, colnames(data)), setdiff(colnames(data), cols))
#     data <- data[, cols]
# 
#     data
# }


#' Convert an object to a Track Frame
#' 
#' @param data the object to be converted.
#' @param ... additional arguments passed to the method.
#' @return A track_frame object
#' @export
as.track_frame <- function(data, ...) {
    UseMethod("as.track_frame")
}


#' Convert a Data Frame to a Track Frame Object
#'
#' This function converts a `data.frame` into a `track_frame` object,
#' ensuring required columns exist and have valid data types.
#'
#' @param data a `data.frame` containing the tracking data.
#' @param time_col a character string specifying the column name of the time column.
#' @param easting_col A character string specifying the column name of the easting column.
#' @param northing_col A character string specifying the column name of the northing column.
#' @param id_col Optional character vector specifying identifier column names.
#' @param ... Additional arguments (unused).
#'
#' @return A `track_frame` object with appropriate attributes set.
#' @examples
#' df <- data.frame(
#'   time_col = as.POSIXct(Sys.time() + 1:5),
#'   easting_col = runif(5, 0, 10),
#'   northing_col = runif(5, 0, 10),
#'   id = 1:5
#' )
#' tf <- as.track_frame(df, time_col = "time_col", easting_col = "easting_col",
#'                         northing_col = "northing_col", id_col = "id")
#' attributes(tf)
#' @export
as.track_frame.data.frame <- function(data,
                                      time_col = NULL,
                                      easting_col = NULL,
                                      northing_col = NULL,
                                      id_col = NULL,
                                      ...) {
    assert_choice(time_col, colnames(data), null.ok = TRUE)
    if(is.null(time_col)) {
      time_col_guesses <- c("t", "timestamp", "time", "time_index", "tindex")
      ind <- time_col_guesses %in% colnames(data)
      stopifnot("time_col needs to be specified. Guessing not succesful." = sum(ind) >= 1)
      time_col <- time_col_guesses[time_col_guesses %in% colnames(data)][1]
    }
    assert_choice(easting_col, colnames(data),  null.ok = TRUE)
    if(is.null(easting_col)) {
      easting_col_guesses <- c("easting", "east", "lon", "long", "longitude", "x", "utm.easting")
      ind <- easting_col_guesses %in% colnames(data)
      stopifnot("easting_col needs to be specified. Guessing not succesful." = sum(ind) >= 1)
      easting_col <- easting_col_guesses[easting_col_guesses %in% colnames(data)][1]
    }
    assert_choice(northing_col, colnames(data),  null.ok = TRUE)
    if(is.null(northing_col)) {
      northing_col_guesses <- c("northing", "north", "lat", "latitude", "y", "utm.northing")
      ind <- northing_col_guesses %in% colnames(data)
      stopifnot("northing_col needs to be specified. Guessing not succesful." = sum(ind) >= 1)
      northing_col <- northing_col_guesses[northing_col_guesses %in% colnames(data)][1]
    }
    assert_choice(id_col, colnames(data),  null.ok = TRUE)
    assert_character(id_col, len = 1, null.ok = TRUE)
    assert_numeric(data[[easting_col]])
    assert_numeric(data[[northing_col]])
    assert_posixct(data[[time_col]])
    attr(data, "time") <- time_col
    attr(data, "easting") <- easting_col
    attr(data, "northing") <- northing_col
    attr(data, "id") <- id_col
    class(data) <- union("track_frame", class(data))
    return(data)
}


#' @export
as.track_frame.matrix <- function(data,
                                  time_col = NULL,
                                  easting_col = NULL,
                                  northing_col = NULL,
                                  id_col = NULL,
                                  ...) {
  assert_choice(time_col, colnames(data), null.ok = TRUE)
  if(is.null(time_col)) {
    time_col_guesses <- c("t", "timestamp", "time", "time_index", "tindex")
    ind <- time_col_guesses %in% colnames(data)
    stopifnot("time_col needs to be specified. Guessing not succesful." = sum(ind) >= 1)
    time_col <- time_col_guesses[time_col_guesses %in% colnames(data)][1]
  }
  assert_choice(easting_col, colnames(data),  null.ok = TRUE)
  if(is.null(easting_col)) {
    easting_col_guesses <- c("easting", "east", "lon", "long", "longitude", "x", "utm.easting")
    ind <- easting_col_guesses %in% colnames(data)
    stopifnot("easting_col needs to be specified. Guessing not succesful." = sum(ind) >= 1)
    easting_col <- easting_col_guesses[easting_col_guesses %in% colnames(data)][1]
  }
  assert_choice(northing_col, colnames(data),  null.ok = TRUE)
  if(is.null(northing_col)) {
    northing_col_guesses <- c("northing", "north", "lat", "latitude", "y", "utm.northing")
    ind <- northing_col_guesses %in% colnames(data)
    stopifnot("northing_col needs to be specified. Guessing not succesful." = sum(ind) >= 1)
    northing_col <- northing_col_guesses[northing_col_guesses %in% colnames(data)][1]
  }
  assert_choice(id_col, colnames(data),  null.ok = TRUE)
  assert_character(id_col, len = 1, null.ok = TRUE)
  assert_numeric(data[, easting_col])
  assert_numeric(data[, northing_col])
  # assert_posixct(data[, time_col])
  
  # data <- as.data.frame(data)
  attr(data, "time") <- time_col
  attr(data, "easting") <- easting_col
  attr(data, "northing") <- northing_col
  attr(data, "id") <- id_col
  # class(data) <- c("track_frame", "data.frame")
  class(data) <- union("track_frame", class(data))
  return(data)
}

# unique_id <- function(data) {
#   # FIXME: make id col unique
#   if(length(id_cols) > 1) {
#     id_col <- paste(id_cols, collapse = "__" )
#     data[[id_col]] <- apply(data[, id_cols] , 1 , paste , collapse = "__")
#   } else {
#     id_col <- id_cols
#   }
#   attr(data, "id") <- id_col
#   return(data)
# }

#' @export
as.track_frame.move2 <- function(data, ...) {
    data_attr <- attributes(data)
    x_y <- st_coordinates(data[[attr(data, "sf_column")]])  #FIXME transformation to cartesian coordinates
    time_index <- attr(data, "time_column")
    id_col <- attr(data, "track_id_column") #move2: The `track_id_column` attribute should be a <character> of length 1
    cols <- setdiff(colnames(data), attr(data, "sf_column"))
    class(data) <- "list"
    data <- data[cols]
    data[["easting"]] <- x_y[, 1]
    data[["northing"]] <- x_y[, 2]
    # class(data) <- c("tbl_df", "tbl", "data.frame") #FIXME classes?
    class(data) <- c("data.frame")
    attr(data, "row.names") <- data_attr[["row.names"]]
    as.track_frame(data, time_col = time_index, easting_col = "easting",
                   northing_col = "northing", id_col = id_col)
}

#' @export
as.track_frame.sftrack <- function(data, ...) {
  data_attr <- attributes(data)
  x_y <- st_coordinates(data[[attr(data, "sf_column")]]) #FIXME transformation to cartesian coordinates
  time_index <- attr(data, "time_col")
  id_col <- attr(data, "group_col")
  cols <- setdiff(colnames(data), attr(data, "sf_column"))
  # class(data) <- "list"
  data <- data[,cols]
  data[["track_id"]] <- sapply(data[[id_col]], deparse)
  # lapply(uid, function(text) eval(parse(text = text))) # reverse transformation
  data[["easting"]] <- x_y[, 1]
  data[["northing"]] <- x_y[, 2]
  class(data) <- c("data.frame")
  attr(data, "row.names") <- data_attr[["row.names"]]
  as.track_frame(data, time_col = time_index, easting_col = "easting",
                 northing_col = "northing", id_col = "track_id")
}

#' @export
as.track_frame.track_frame <- function(data,
                                       time_col = NULL,
                                       easting_col = NULL,
                                       northing_col = NULL,
                                       id_col = NULL,
                                       ...) {
  assert_choice(time_col, colnames(data),  null.ok = TRUE)
  assert_choice(easting_col, colnames(data),  null.ok = TRUE)
  assert_choice(northing_col, colnames(data),  null.ok = TRUE)
  assert_choice(id_col, colnames(data),  null.ok = TRUE)
  assert_character(id_col, len = 1, null.ok = TRUE)
  if(is.null(time_col)) time_col <- attr(data, "time")
  if(is.null(easting_col)) easting_col <- attr(data, "easting")
  if(is.null(northing_col)) northing_col <- attr(data, "northing")
  if(is.null(id_col)) id_col <- attr(data, "id")
  assert_numeric(data[[easting_col]])
  assert_numeric(data[[northing_col]])
  assert_posixct(data[[time_col]])
  attr(data, "time") <- time_col
  attr(data, "easting") <- easting_col
  attr(data, "northing") <- northing_col
  attr(data, "id") <- id_col
  class(data) <- union("track_frame", class(data)) #FIXME: only data.frame?
  return(data)
}


#' Converts a cocomo object to a track_frame
#'
#' @param xs matrix of x coordinates (UTM eastings) of all individuals in a group or population (rows) at every time point (columns) xs[i,t] gives the x / easting position of individual i at time point t
#' @param ys matrix of y coordinates (UTM northings) of all individuals in a group or population (rows) at every time point (columns) ys[i,t] gives the y / northing position of individual i at time point t
#' @param timestamps vector of timestamps in posixct corresponding to the columns of xs and ys matrices. Timestamps must be uniformly sampled, though it is possible to have gaps (e.g. between different days of recording)
#' @param ids  data frame giving information about the tracked individuals, with rows correpsonding to the rows of the xs and ys matrices. There must be one column called id_code which contains a unique individual identifier for each animal (e.g. for meerkats: 'VCVM001', for hyenas: 'WRTH', for coatis: 'Luna') The other columns contained are flexible, and can include information on age, sex, dominance, etc
#'
#' @return an object of class track_frame
#' @export
#'
#' @examples
#' #test data
#' padding <- 25
#' n_times <- 20 + 3 * padding
#' N <- 2
#' 
#' xs <- matrix(
#'   c(
#'     rep(-1, padding),
#'     c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1),
#'     rep(0, padding),
#'     c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
#'     rep(1, padding),
#'     rep(0, n_times)
#'   ),
#'   N,n_times,byrow=TRUE
#' )
#' ys <- matrix(
#'   rep(0, N*n_times),
#'   N,n_times,byrow=TRUE
#' )
#' timestamps <- as.POSIXct(1:n_times)
#' 
#' ids <- rbind.data.frame(list("id_code" = 'VCVM001', "age" = 10, "sex" = "m"),
#'                         list("id_code" = 'WRTH', "age" = 5, "sex" = "f"))
#'                         
#' tf <- as.track_frame_from_cocomo(xs, ys, timestamps, ids)
#' class(tf)
#' head(tf)
as.track_frame_from_cocomo <- function(xs, ys, timestamps, ids) {
  assert_matrix(xs)
  assert_matrix(ys)
  assert_posixct(timestamps)
  assert_data_frame(ids)
  assert_choice("id_code", colnames(ids))
  data <- data.frame("time" = timestamps,
                     "easting" = as.vector(t(xs)),
                     "northing" = as.vector(t(ys)),
                     "track_id" = rep(ids$id_code, each = NCOL(xs)))
  cols <- setdiff(colnames(ids), "id_code")
  add_cols <- do.call("cbind.data.frame", lapply(ids[, cols], function(x) rep(x, each = NCOL(xs))))
  data <- cbind(data, add_cols)
  as.track_frame(data, time_col = "time", easting_col = "easting",
                 northing_col = "northing", id_col = "track_id")
}