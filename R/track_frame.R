
# #' @param index Column name of the index column
# #' @param easting Column name of the easting column
# #' @param northing Column name of the northing column
# #' @param alt Column name of the altitude column
# #' @param id Column name of the id column
# #' @return A track_frame object
# #' @export
track_frame <- function(...,
                        time_col = NULL,
                        easting_col = NULL,
                        northing_col = NULL,
                        id_col = NULL) {
  df <- data.frame(...)
  as.track_frame(df, time_col = time_col, easting_col = easting_col, 
                 northing_col = northing_col, id_col = id_col)
}
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
#' @rdname as_track_frame
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
#' @rdname as_track_frame
as.track_frame.data.frame <- function(data,
                                      time_col = NULL,
                                      easting_col = NULL,
                                      northing_col = NULL,
                                      id_col = NULL,
                                      ...) {
    assert_choice(time_col, colnames(data), null.ok = TRUE)
    if(is.null(time_col)) {
      time_col_guesses <- c("t", "timestamp", "time", "time_index", "tindex") #FIXME: avoid duplication and move to function in auxiliary
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
  as.track_frame(as.data.frame(data), time_col = time_col,
                 easting_col = easting_col, northing_col = northing_col,
                 id_col = id_col, ...)
}


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
  cols <- setdiff(colnames(data), attr(data, "sf_column"))
  data <- data[,cols]
  #FIXME: Can attr(data, "group_col") be null if only 1 track exists?
  data[["id"]] <- sapply(data[[attr(data, "group_col")]], deparse)
  data[["easting"]] <- x_y[, 1]
  data[["northing"]] <- x_y[, 2]
  # FIXME: as.data.frame?
  class(data) <- c("data.frame")
  attr(data, "row.names") <- data_attr[["row.names"]]
  as.track_frame(data, time_col = time_index, easting_col = "easting",
                 northing_col = "northing", id_col = "id")
}


#' @export
as.track_frame.track_frame <- function(data,
                                       time_col = NULL,
                                       easting_col = NULL,
                                       northing_col = NULL,
                                       id_col = NULL,
                                       ...) {
  if (!is.null(time_col)) {
    checkmate::assert_string(time_col)
    checkmate::assert_choice(time_col, colnames(data))
    attr(data, "time") <- time_col
  }
  if (!is.null(easting_col)) {
    checkmate::assert_choice(easting_col, colnames(data))
    checkmate::assert_string(easting_col)
    attr(data, "easting") <- easting_col
  }
  if (!is.null(northing_col)) {
    checkmate::assert_choice(northing_col, colnames(data))
    checkmate::assert_string(northing_col)
    attr(data, "northing") <- northing_col
  }
  if (!is.null(id_col)) {
    checkmate::assert_choice(id_col, colnames(data))
    checkmate::assert_string(id_col)
    attr(data, "id_col") <- id_col
  }
  data
}


#' Converts a cocomo object to a track_frame
#'
#' @param x matrix of x coordinates (UTM eastings) of all individuals in a group or population (rows) at every time point (columns) x[i,t] gives the x / easting position of individual i at time point t
#' @param y matrix of y coordinates (UTM northings) of all individuals in a group or population (rows) at every time point (columns) y[i,t] gives the y / northing position of individual i at time point t
#' @param t vector of timestamps in posixct corresponding to the columns of x and y matrices. Timestamps must be uniformly sampled, though it is possible to have gaps (e.g. between different days of recording)
#' @param ids  data frame giving information about the tracked individuals, with rows correpsonding to the rows of the x and y matrices. There must be one column called id_code which contains a unique individual identifier for each animal (e.g. for meerkats: 'VCVM001', for hyenas: 'WRTH', for coatis: 'Luna') The other columns contained are flexible, and can include information on age, sex, dominance, etc
#'
#' @return an object of class track_frame
#' @export
#'
#' @examples
#' x <- tf_as_cocomo(sim_travel_paths(3, 3))
#' cocomo_as_tf(x$x, x$y, x$t, x$ids)
cocomo_as_tf <- function(x, y, t, ids) {
  assert_matrix(x)
  assert_matrix(y)
  assert_true(NCOL(x) == NCOL(y))
  assert_true(NROW(x) == NROW(y))
  assert_numeric(t, len = NCOL(x), any.missing = FALSE)
  assert_data_frame(ids)
  assert_choice("id_code", colnames(ids))
  data <- data.frame("time" = t,
                     "easting" = as.vector(base::t(x)),
                     "northing" = as.vector(base::t(y)),
                     "id" = rep(ids$id_code, each = NCOL(x)))
  if (NCOL(ids) > 1L) {
    for (col in setdiff(colnames(ids), "id_code")) {
      if (col %in% colnames(data)) {
        new_name <- tail(make.unique(c(colnames(data), col), "_"), 1L)
        data[[new_name]] <- ids[[col]]
      } else {
        data[[col]] <- ids[[col]]
      }
    }
  }
  as.track_frame(data, time_col = "time", easting_col = "easting",
                 northing_col = "northing", id_col = "id")
}


#' Convert a \code{track\_frame} into the \code{cocomo} format
#'
#' This function converts a \code{track\_frame} object into the cocomo format.
#'
#' @param tf an object inheriting from \code{track\_frame}.
#'
#' @return A list with three components:
#'   \item{x}{A matrix of x-coordinates (easting values). If tf has no id attribute,
#'     this is a single-column matrix. If tf has ids, rows represent different tracks
#'     and columns represent time points.}
#'   \item{y}{A matrix of y-coordinates (northing values). Same structure as x matrix.}
#'   \item{t}{A vector of time values, sorted in ascending order.}
#'
#' @examples
#' tf <- sim_travel_paths(3, 3)
#' tf_as_cocomo(tf)
#'
#' @export
tf_as_cocomo <- function(tf) {
  if (is.null(attr(tf, "id"))) {
    x <- matrix(tf[[attr(tf, "easting")]])
    y <- matrix(tf[[attr(tf, "northing")]])
    time <- tf[[attr(tf, "time")]]
  } else {
    time <- sort(unique(tf[[attr(tf, "time")]]))
    ids <- unique_ids(tf)
    NA_val <- as(NULL, mode(tf[[attr(tf, "easting")]]))
    x <- y <- matrix(NA_val, nrow = length(ids), ncol = length(time))
    for (i in seq_along(ids)) {
        id <- ids[i]
        idx <- which(tf[[attr(tf, "id")]] == id)
        m <- match(time, tf[[attr(tf, "time")]][idx])
        x[i, ] <- tf[[attr(tf, "easting")]][idx][m]
        y[i, ] <- tf[[attr(tf, "northing")]][idx][m]
    }
    rownames(x) <- rownames(y) <- ids
  }
  list(x = x, y = y, t = time, ids = data.frame(id_code = ids))
}


is.track_frame <- function(x) {
  inherits(x, "track_frame")
}

