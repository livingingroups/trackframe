#' Backtransform
#'
#' @param tf an object of class `trackframe`
#'
#' @return an object which has been coerced to `trackframe`
#' @export
#'
#' @examples
#' library(trackframe)
#' # for move2
#' tf <- as.trackframe(data = move2_mini)
#' tfb <- tf_backtransform(tf)
#' tfb
#' move2_mini
#'
#' # for sftrack
#' tf <- as.trackframe(data = sftrack_mini)
#' tfb <- tf_backtransform(tf)
#' tfb
#' sftrack_mini
#'
#' # for data.frame
#' tf <- as.trackframe(data = df_mini)
#' tfb <- tf_backtransform(tf)
#' tfb
#' df_mini
tf_backtransform <- function(tf) {
  assert_class(tf, "trackframe")
  transformation_info <- attr(tf, "transformation_info")
  if (is.null(transformation_info)) {
    stop("no transformation info stored to trackframe")
  }
  class_old <- transformation_info$class[1]
  if (class_old == "move2") {
    tf_as_move2(tf, tf_crs = attr(tf, "utm_epsg"), crs_new = transformation_info$crs_code)
  } else if ("sftrack" %in% transformation_info$class) {
    tf_as_sftrack(tf, tf_crs = attr(tf, "utm_epsg"), crs_new = transformation_info$crs_code)
  } else if (class_old %in%  c("data.frame", "data.table", "tbl_df", "tbl")) {
    if (attr(tf, "easting") != transformation_info$coord_names[1]) {
      tf[, attr(tf, "easting")] <- NULL
    }
    if (attr(tf, "northing") != transformation_info$coord_names[2]) {
      tf[, attr(tf, "northing")] <- NULL
    }
    if (class_old ==  "data.frame") {
      data <- as.data.frame(tf)
    } else if (class_old ==  "data.table") {
      data <- as.data.table(tf)
    } else if (class_old %in% c("tbl_df", "tbl")) {
      data <- as_tibble(tf)
    }
    attributes(data) <- transformation_info$attributes
    data
  } else if (class_old ==  "matrix") {
    stop("backtransformation not supported for class matrix. Use ?coredata instead.")
  }
}


#' Convert a Track Frame to XYT Format
#'
#' This function extracts the core spatial-temporal data from a trackframe object,
#' returning a simplified data frame with just the easting (x), northing (y),
#' time (t), and optionally the track ID columns.
#'
#' @param x A `trackframe` object containing the tracking data.
#' @param ... other arguments passed to coredata
#' @return A data frame with the easting, northing, time index, and optionally track ID columns.
#' @examples
#' tf_as_xyt(tf_mini)
#' @export
tf_as_xyt <- function(x, ...) { #coredata.trackframe
  #FIXME: check what we want to do in coredata
  assert_class(x, "trackframe")
  if (is.null(attr(x, "id"))) {
    cols <- c(attr(x, "easting"), attr(x, "northing"), attr(x, "time"))
  } else {
    cols <- c(attr(x, "easting"), attr(x, "northing"), attr(x, "time"), attr(x, "id"))
  }
  x <- x[, cols, with = FALSE]
  class(x) <- setdiff(class(x), "trackframe")
  x
}


#' @export
#' @rdname coredata
coredata.trackframe <- tf_as_xyt

#' Convert a Track Frame to Simple Features (sf) Object
#'
#' This function converts a `trackframe` object into a Simple Features (sf) object,
#' enabling spatial analysis and visualization. The easting and northing columns
#' are used as coordinates for the sf object.
#'
#' @param tf A `trackframe` object containing the tracking data. Must have
#'           attributes specifying the easting and northing columns (`easting` and `northing`).
#' @param tf_crs The coordinate reference system (CRS) used in the `trackframe`.
#' @param crs_new The coordinate reference system (CRS) to be used for the sf object.
#' @param ... Additional arguments to be passed to `st_as_sf`.
#' @return An sf object representing the spatial data contained in the `trackframe`.
#'
#' @examples
#' sf_object <- tf_as_sf(tf_mini, tf_crs = 32610, crs_new = 4326)
#' print(sf_object)
#' @export
#' @rdname tf_as
tf_as_sf <- function(tf, tf_crs = NULL, crs_new = NULL, ...) {
  # NOTE: tf_as_sf to be consistent with the sf package.
  assert_class(tf, "trackframe")
  if (is.null(tf_crs)) {
    tf_crs <- utm_epsg(tf)
    if (is.null(tf_crs)) stop("no utm_epsg provided in trackframe. Please provide argument tf_crs")
  }
  coords <- c(attr(tf, "easting"), attr(tf, "northing"))
  new_sf <- sf::st_as_sf(x = tf, crs = tf_crs, coords = coords, na.fail = FALSE, ...)
  if (!is.null(crs_new)) {
    new_sf <- sf::st_transform(new_sf, crs_new)
  }
  class(new_sf) <- setdiff(class(new_sf), "trackframe")
  return(new_sf)
}


#' @export
#' @rdname tf_as
tf_as_sftrack <- function(tf, tf_crs = NULL, crs_new = NULL, ...) {
  assert_class(tf, "trackframe")
  transformation_info <- attr(tf, "transformation_info")
  if (is.null(tf_crs)) {
    tf_crs <- utm_epsg(tf)
    if (is.null(tf_crs)) stop("no utm_epsg provided in trackframe. Please provide argument tf_crs")
  }
  # We want to create an sftrack object without importing it.
  as_sftrack <- try(getNamespace("sftrack")$as_sftrack, silent = TRUE)
  if (inherits(as_sftrack, "try-error")) {
    stop("package 'sftrack' is required for this function. Please install it.")
  }
  # # FIXME: Should we drop NA?
  # tf <- tf[!is.na(easting(tf)) & !is.na(northing(tf)),]

  sft_group <- as.list(do.call(
    rbind.data.frame,
    backtransform_id(id(tf), group_names = transformation_info[["group_names"]])
  ))

  new_sftrack <- as_sftrack(
    as.data.frame(tf),
    coords = c(attr(tf, "easting"), attr(tf, "northing")),
    group = sft_group,
    time = attr(tf, "time"),
    crs = tf_crs,
    group_name = transformation_info$group_col,
    error = transformation_info$error_col,
    overwrite_names = TRUE
  )

  if (length(new_sftrack$sft_group[[1]]) == 1) {
    attr_agr <- attr(new_sftrack, "agr")
    new_sftrack[[attr(tf, "id")]] <- unlist(new_sftrack[["sft_group"]])
    attr(new_sftrack, "agr") <- attr_agr
  } else {
    new_sftrack[[attr(tf, "id")]] <- NULL
  }

  if (!is.null(crs_new)) {
    new_sftrack <- sf::st_transform(new_sftrack, crs_new)
  }

  for (colname in c(attr(tf, "easting"), attr(tf, "northing"))) {
    if (!colname %in% transformation_info$names) new_sftrack[[colname]] <- NULL
  }
  class(new_sftrack) <- setdiff(class(new_sftrack), "trackframe")

  return(new_sftrack)
}


#' @export
#' @rdname tf_as
tf_as_move2 <- function(tf, tf_crs = NULL, crs_new = NULL, ...) {
  # tf_crs <- 32610
  assert_class(tf, "trackframe")
  mt_as_move2 <- try(getNamespace("move2")$mt_as_move2, silent = TRUE)
  if (inherits(mt_as_move2, "try-error")) {
    stop("package 'move2' is required for this function. Please install it.")
  }
  # TODO: Should we drop NA?
  tf <- tf[!is.na(easting(tf)) & !is.na(northing(tf)), ]
  sf_df <- tf_as_sf(tf = tf, tf_crs = tf_crs, crs_new = crs_new)
  # FIXME: We want to create an move2 object without importing it.
  mt_as_move2(
    sf_df,
    time_column = attr(tf, "time"),
    track_id_column = attr(tf, "id"))
}
