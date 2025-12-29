#' Extract or Assign from a Track Frame Coordinate Reference System
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
#' crs(tf_mini) <- NA
#' crs(tf_mini)
#'
#' @export
#' @rdname crs
crs <- function(tf) {
  assert_trackframe(tf)
  attr(tf, "crs")
}

#' @export
#' @param value a valid projected crs or NA
#' @returns tf with new crs
#' @rdname crs
`crs<-` <- function(tf, value) {
  # arg checking
  assert_trackframe(tf)
  # derive_crs_type error if invalid.
  # Must be run before any assignment
  crs_type <- derive_crs_type(value)

  # assignment
  attr(tf, "crs") <- value
  attr(tf, "crs_type") <- crs_type
  tf
}

#' Extract Coordinate Reference System Type from a Track Frame
#'
#' This function retrieves the crs value from a \code{trackframe} object.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#' @return One of:
#'  \code{projected} e.g. utm
#'  \code{nongeoreferenced} designed for use in captive or simulated scenarios
#'
#' @examples
#' crs_type(tf_mini)
#'
#' @export
crs_type <- function(tf) {
  attr(tf, "crs_type")
}


#' Extract or Assign Trackframe Time Index
#'
#' This function retrieves or updates
#' the time index values from a \code{trackframe} object.
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
#' time(tf_mini) <- seq_along(nrow(tf_mini))
#'
#' @export
#' @rdname tf_time
time.trackframe <- function(x, ...) {
  x[[attr(x, "time")]]
}

#' @export
#' @param value new value
#' @rdname tf_time
"time<-" <- function(x, value) {
  UseMethod("time<-")
}

#' @export
#' @param value vector of POSIX timestamps with length nrow(x)
#' @rdname tf_time
"time<-.trackframe" <- function(x, value) {
  assert_class(x, "trackframe")
  x[[attr(x, "time")]] <- value
  x
}

#' @rdname tf_colnames
#' @examples
#' time_col(tf_mini)
#' @export
time_col <- function(tf) {
  tf_colnames(tf)[["time"]]
}

#' @rdname tf_colnames
#' @examples
#' time_col(tf_mini) <- as.POSIXct(seq_along(nrow(tf_mini)))
#' colnames(tf_mini)
#' @export
`time_col<-` <- function(tf, value) {
  tf_colnames(tf)[["time"]] <- value
  tf
}

#' Extract or Assign Trackframe Track ID
#'
#' This function retrieves the track ID values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing track ID values.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the track ID column (`id`).
#' @return A vector of track ID values extracted from the \code{trackframe}.
#'         NULL if no id column is configured.
#'
#' @examples
#' id(tf_mini)
#' id(tf_mini) <- rep("new_trackname", nrow(tf_mini))
#' @export
#' @rdname tf_id
id <- function(tf) {
  assert_class(tf, "trackframe")
  id_col <- attr(tf, "id")
  if (is.null(id_col)) NULL else tf[[id_col]]
}


#' @rdname tf_id
#' @param value new values for id column
#' @export
"id<-" <- function(tf, value) {
  assert_class(tf, "trackframe")
  id_col <- attr(tf, "id")
  if (is.null(id_col)) {
    attr(tf, "id") <- id_col <- "id"
  }
  tf[[id_col]] <- value
  tf
}

#' @rdname tf_colnames
#' @examples
#' id_col(tf_mini)
#' @export
id_col <- function(tf) {
  if ('id' %in% names(tf_colnames(tf))) tf_colnames(tf)[["id"]] else NULL
}

#' @rdname tf_colnames
#' @examples
#' id_col(tf_mini) <- "track_id"
#' colnames(tf_mini)
#' @export
`id_col<-` <- function(tf, value) {
  tf_colnames(tf)[["id"]] <- value
  tf
}


#' Extract or Assign Trackframe coordinates
#'
#' This function retrieves the easting values from a \code{trackframe} object.
#' The \code{trackframe} has a data frame-like structure with an attribute
#' specifying the column containing easting values.
#'
#' @param tf A \code{trackframe} object containing the tracking data.
#'           Must have an attribute indicating the easting column (`easting`).
#' @return A vector of easting or northing values extracted from the \code{trackframe}.
#'
#' @examples
#' easting(tf_mini)
#'
#' @export
#' @rdname tf_coords
easting <- function(tf) {
  assert_class(tf, "trackframe")
  tf[[easting_col(tf)]]
}

#' @export
#' @param value numeric vector of new coordinate values
#' @examples
#' easting(tf_mini) <- -easting(tf_mini)
#' @rdname tf_coords
"easting<-" <- function(tf, value) {
  assert_class(tf, "trackframe")
  assert_numeric(value)
  tf[[easting_col(tf)]] <- value
  tf
}

#' @export
#' @examples
#' easting_col(tf_mini)
#' @rdname tf_colnames
easting_col <- function(tf) {
  tf_colnames(tf)[["easting"]]
}

#' @export
#' @examples
#' easting_col(tf_mini) <- "x"
#' @rdname tf_colnames
"easting_col<-" <- function(tf, value) {
  tf_colnames(tf)[["easting"]] <- value
  tf
}


#' @examples
#' northing(tf_mini)
#'
#' @export
#' @rdname tf_coords
northing <- function(tf) {
  assert_class(tf, "trackframe")
  tf[[northing_col(tf)]]
}

#' @examples
#' northing(tf_mini) <- northing(tf_mini)*5
#'
#' @export
#' @rdname tf_coords
"northing<-" <- function(tf, value) {
  assert_class(tf, "trackframe")
  assert_numeric(value)
  tf[[attr(tf, "northing")]] <- value
  tf
}

#' @export
#' @examples
#' northing_col(tf_mini)
#' @rdname tf_colnames
northing_col <- function(tf) {
  tf_colnames(tf)[["northing"]]
}

#' @export
#' @examples
#' northing_col(tf_mini) <- "y"
#' @rdname tf_colnames
`northing_col<-` <- function(tf, value) {
  tf_colnames(tf)[["northing"]] <- value
  tf
}
