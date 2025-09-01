#' @examples
#' library(trackframe)
#' df <- data.frame(
#'   time_col = as.POSIXct(Sys.time() + 1:5),
#'   easting_col = runif(5, 0, 10),
#'   northing_col = runif(5, 0, 10),
#'   id = 1:5
#' )
#' tf <- trackframe(df, time_col = "time_col", easting_col = "easting_col",
#'                         northing_col = "northing_col", id_col = "id")
#' attributes(tf)
#' @export
#' @rdname as_trackframe
trackframe <- function(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  verbose = FALSE,
  crs_input = NULL,
  utm_epsg = NULL,
  ...
) {
  df <- data.frame(data) #FIXME: does not work for tibble + data.table
  as.trackframe(
    df, time_col = time_col, easting_col = easting_col,
    northing_col = northing_col, id_col = id_col,
    crs_input = crs_input, utm_epsg = utm_epsg, sort = sort,
    coerce_to = coerce_to, ...
  )
}


#' Convert an object to a \code{trackframe}
#'
#' This function converts an object into a \code{trackframe} object,
#' checks that required columns exist and have valid data types.
#' Coordinates must be provided in easting/northing.
#' Coordinates for
#'  \code{data.frame}, \code{sftrack} and \code{move2} objects
#'  are transformed to easting/northing if possible.
#'
#' @param data
#'  a \code{data.frame}, a \code{matrix}, an \code{sftrack} object or a \code{move2} object
#'  containing the tracking data.
#' @param time_col
#'  a character string or vector specifying the column name of the time column.
#' @param easting_col a character string or vector specifying the column name of the easting column.
#'   If a vector is provided, the first element matching a column name of `data`` is used.
#' @param northing_col a character string or vector specifying the column name of
#'  the northing column.
#'  If a vector is provided, the first element matching a column name of `data`` is used.
#' @param id_col optional character vector specifying identifier column names.
#'  If no column is specified,
#'  If a vector is provided, the first element matching a column name of `data`` is used.
#' @param sort logical, if data should be sorted by id_col and time_col.
#' @param coerce_to the type of dataframe that trackframe is coerced to.
#'  `base`, `data.table` and `tibble` are supported.
#'  Default is `base` and coerces to a `data.frame` without `data.table` or `tbl` classes.
#'  If NULL, the returned `trackframe` object takes the same dataframe type as `data` argument
#' @param verbose logical, default value is \code{TRUE}
#' @param ... Additional arguments (unused).
#'
#' @return A \code{trackframe} object with appropriate attributes set.
#' @examples
#' df <- data.frame(
#'   time_col = as.POSIXct(Sys.time() + 1:5),
#'   easting_col = runif(5, 0, 10),
#'   northing_col = runif(5, 0, 10),
#'   id = 1:5
#' )
#' tf <- as.trackframe(df, time_col = "time_col", easting_col = "easting_col",
#'                         northing_col = "northing_col", id_col = "id")
#' class(tf)
#' attributes(tf)
#' easting(tf)
#' northing(tf)
#' time(tf)
#' id(tf)
#'
#' @export
#' @rdname as_trackframe
as.trackframe <- function(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  verbose = FALSE,
  ...
) {
  UseMethod("as.trackframe")
}


#' @param crs_input crs code for input of coordinates
#' @param utm_epsg crs value for utm zone of the \code{trackframe} output
#' @examples
#' as.trackframe(df_mini, crs_input = 4326)
#'
#' set.seed(2025)
#' df <- data.frame(
#'   x = rnorm(10),
#'     y = rnorm(10),
#'       t = 1:10,
#'         animal_id = c(rep('a', 5), rep('b', 5))
#'         )
#'  as.trackframe(df,
#'                time_col = "t",
#'                easting_col = "x",
#'                northing_col = "y",
#'                id_col = "animal_id")
#'  # with col guessing
#'  as.trackframe(df, coerce_to = "base")
#'
#'  tf_df <- as.trackframe(df, coerce_to = "base")
#'  tf_dt <- as.trackframe(df, coerce_to = "data.table")
#'  tf_tib <- as.trackframe(df, coerce_to = "tibble")
#'
#'  tf_backtransform(tf_df)
#'  tf_backtransform(tf_dt)
#'  tf_backtransform(tf_tib)
#'
#' @export
#' @rdname as_trackframe
as.trackframe.data.frame <- function(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base", #FIXME: or "data.frame"?
  verbose = FALSE,
  crs_input = NULL,
  utm_epsg = NULL,
  ...
) {
  assert_logical(verbose, len = 1L)
  cn_input <- colnames(data)
  attributes_input <- attributes(data)

  # coerce_to
  assert_choice(coerce_to, choices = c("base", "data.table", "tibble"), null.ok = TRUE)
  if (is.null(coerce_to)) {
  } else if (coerce_to == "base") {
    if (inherits(data, "data.table") || inherits(data, "tbl") || inherits(data, "tbl_df")) {
      if (verbose) writeLines("- data coerced by as.data.frame(data)")
      data <- as.data.frame(data)
    }

  } else if (coerce_to == "data.table") {
    if (!inherits(data, "data.table")) {
      if (verbose) writeLines("- data coerced by as.data.table(data)")
      data <- as.data.table(data)
    }
  } else if (coerce_to == "tibble") {
    if (!inherits(data, "tbl") || !inherits(data, "tbl_df")) {
      if (verbose) writeLines("- data coerced by as_tibble(data)")
      if (!is.null(data$sft_group)) {#data$sft_group <- NULL #FIXME: same transformation as for id
        data$sft_group <- make_unique_id(data$sft_group)
      }
      data <- as_tibble(data)
    }
  }

  #columns guessing
  guesses <- guess_all_cols(
    col_names = colnames(data),
    time_col_candidates = time_col,
    easting_col_candidates = easting_col,
    northing_col_candidates = northing_col,
    id_col_candidates = id_col
  )

  warn_if_guess_ambiguous(data, guesses)

  time_col <- guesses[["time_col"]][1]
  assert_choice(time_col, colnames(data), null.ok = FALSE)
  assert_character(time_col, len = 1, null.ok = FALSE)
  assert_numeric(data[[time_col]])

  easting_col <- guesses[["easting_col"]][1]
  assert_choice(easting_col, colnames(data),  null.ok = FALSE)
  assert_character(easting_col, len = 1, null.ok = FALSE)

  northing_col <- guesses[["northing_col"]][1]
  assert_choice(northing_col, colnames(data),  null.ok = FALSE)
  assert_character(northing_col, len = 1, null.ok = FALSE)

  id_col <- guesses[["id_col"]][1]
  if (is.na(id_col)) id_col <- NULL
  assert_choice(id_col, colnames(data),  null.ok = TRUE)
  assert_character(id_col, len = 1, null.ok = TRUE)

  assert_numeric(data[[easting_col]])
  assert_numeric(data[[northing_col]])
  assert_numeric(data[[time_col]])
  assert_numeric(utm_epsg, lower = 32600, upper = 32760, null.ok = TRUE)

  attr(data, "time") <- time_col
  attr(data, "easting") <- easting_col
  attr(data, "northing") <- northing_col
  attr(data, "id") <- id_col
  class(data) <- union("trackframe", class(data))

  # transform if easting %in% c("lon", "long", "longitude")
  lon_names <- c("lon", "long", "longitude") #FIXME: move to options?
  lat_names <- c("lat", "latitude") #FIXME: move to options?
  if (easting_col %in% lat_names) {
    warning(sprintf("%s specified as easting_col, but seems to correspond to latitude. 
                      easting should correspond to longitude", easting_col))
  }
  if (northing_col %in% lon_names) {
    warning(sprintf("%s specified as northing_col, but seems to correspond to longitude 
                      northing should correspond to latitude", northing_col))
  }
  if (easting_col %in% lon_names || northing_col %in% lat_names) {
    # check data consistency
    if (easting_col %in% lon_names) {
      #check if longitude between -180 and 180
      if (any(data[[easting_col]] < -180 | data[[easting_col]] > 180)) {
        warning(sprintf(
          "Longitude values provided in %s are not between -180 and 180.",
          easting_col
        ))
      }
    }
    if (northing_col %in% lat_names) {
      #check if latitude between -90 and 90
      if (any(data[[northing_col]] < -90 | data[[northing_col]] > 90)) {
        warning(sprintf("Latitude values provided in %s are not between -90 and 90.", northing_col))
      }
    }

    if (!all(c(easting_col %in% lon_names, northing_col %in% lat_names))) {
      stop(sprintf(c(
        "coordinate system of %s easting_col and northing_col %s do not match.",
        "If guessing was not successfull please provide arguments explicitly."
      ), easting_col, northing_col))
    }
    #transform
    if (is.null(crs_input)) {
      warning("crs of input data not known. Assume crs is 4326")
      crs_input <- 4326
    }

    coords <- c(attr(data, "easting"), attr(data, "northing"))
    data_sf <- sf::st_as_sf(x = data, crs = crs_input, coords = coords, ...)
    utm_epsg <- sf_to_utm_epsg(data_sf)
    assert_numeric(utm_epsg, lower = 32600, upper = 32760, null.ok = TRUE)
    new_data_sf <- sf::st_transform(data_sf, utm_epsg)
    x_y <- st_coordinates(new_data_sf[[attr(new_data_sf, "sf_column")]])
    x_y[is.nan(x_y)] <- NA


    data[["easting"]] <- x_y[, 1]
    data[["northing"]] <- x_y[, 2]
    attr(data, "easting") <- "easting"
    attr(data, "northing") <- "northing"
  }

  attr(data, "utm_epsg") <- utm_epsg
  if (verbose) {
    writeLines(sprintf("- %s set as time_col", attr(data, "time")))
    writeLines(sprintf("- %s set as easting_col", attr(data, "easting")))
    writeLines(sprintf("- %s set as northing_col", attr(data, "northing")))
    writeLines(sprintf("- %s set as id_col", attr(data, "id")))
    writeLines(sprintf("- %i set as utm_epsg", attr(data, "utm_epsg")))
  }

  if (is.null(attr(data, "transformation_info"))) {
    transformation_info <- list()
    transformation_info$attributes <- attributes_input
    transformation_info$class <- attributes_input$class
    transformation_info$crs_code <- crs_input
    transformation_info$names <- cn_input
    transformation_info$coord_names <- c(easting_col, northing_col)
    attr(data, "transformation_info") <- transformation_info
  }

  # sort data by id and time
  if (isTRUE(sort)) {
    if (is.null(attr(data, "id"))) {
      data <- data[order(time(data)), ]
    } else {
      data <- data[order(id(data), time(data)), ]
    }
  }
  # # set units
  # units(data[[attr(data, "easting")]]) <- "m"
  # units(data[[attr(data, "northing")]]) <- "m"
  return(data)
}


#' @export
#' @rdname as_trackframe
as.trackframe.matrix <- function(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  verbose = FALSE,
  crs_input = NULL,
  utm_epsg = NULL,
  ...
) {
  as.trackframe(
    as.data.frame(data), time_col = time_col,
    easting_col = easting_col, northing_col = northing_col,
    id_col = id_col, crs_input = crs_input,
    utm_epsg = utm_epsg, sort = sort,
    coerce_to = coerce_to, verbose = verbose, ...)
}


#' @examples
#' # example for move2 objects
#' library(move2)
#' library(trackframe)
#' tf <- as.trackframe(data = move2_mini)
#'
#' move2_dat2 <- tf_backtransform(tf)
#' all.equal(move2_dat2, move2_mini)
#'
#' @export
#' @rdname as_trackframe
as.trackframe.move2 <- function(
  data,
  time_col = NULL,
  easting_col = NULL,
  northing_col = NULL,
  id_col = NULL,
  sort = TRUE,
  coerce_to = "base",
  verbose = FALSE,
  ...
) {
  #FIXME: transform to sftrack and call as.trackframe.sftrack
  if (is.null(time_col)) {
    time_index <- attr(data, "time_column")
  } else {
    time_index <- time_col
  }
  if (is.null(id_col)) {
    # move2: The `track_id_column` attribute should be a <character> of length 1
    id_col <- attr(data, "track_id_column")
  }
  transformation_info <- attributes(data)
  transformation_info$crs_code <- sf::st_crs(data)$input
  # transformation to cartesian coordinates
  utm_epsg <- sf_to_utm_epsg(data)
  data <- st_transform(data, utm_epsg)
  data_attr <- attributes(data)
  cols <- setdiff(colnames(data), attr(data, "sf_column"))
  data <- data[, cols]

  x_y <- st_coordinates(data[[attr(data, "sf_column")]])
  x_y[is.nan(x_y)] <- NA
  if (is.null(easting_col)) {
    easting_col <- "easting"
    data[["easting"]] <- x_y[, 1]
  } else {
    if (length(easting_col) == 1) {
      lon_names <- c("lon", "long", "longitude") #FIXME: move to options?
      if (easting_col %in% lon_names) easting_col <- "easting"
      data[[easting_col]] <- x_y[, 1] #FIXME: how to check if transformation makes sense?
    } else {
      stop("easting col not identified. Please provide further information on easting_col.")
    }
  }

  if (is.null(northing_col)) {
    northing_col <- "northing"
    data[["northing"]] <- x_y[, 2]
  } else {
    if (length(northing_col) == 1) {
      lat_names <- c("lat", "latitude") #FIXME: move to options?
      if (northing_col %in% lat_names) northing_col <- "northing"
      data[[northing_col]] <- x_y[, 2] #FIXME: how to check if transformation makes sense?
    } else {
      stop("northing col not identified. Please provide further information on northing_col")
    }
  }

  class(data) <- c("data.frame")
  attr(data, "row.names") <- data_attr[["row.names"]]
  attr(data, "transformation_info") <- transformation_info
  as.trackframe(
    data, time_col = time_index, easting_col = easting_col,
    northing_col = northing_col, id_col = id_col, utm_epsg = utm_epsg,
    sort = sort, coerce_to = coerce_to, verbose = verbose, ...
  )
}


#' @examples
#' # example for sftrack objects
#' library(sftrack)
#' set.seed(2025)
#' tf <- as.trackframe(data = sftrack_mini)
#' sftrack_dat2 <- tf_backtransform(tf)
#' sftrack_dat2$id <- unlist(sftrack_dat2$sft_group)
#' all.equal(sftrack_dat2, sftrack_mini)
#'
#' @export
#' @rdname as_trackframe
as.trackframe.sftrack <- function(
  data,
  time_col = NULL,
  easting_col = NULL,
  northing_col = NULL,
  id_col = NULL,
  sort = TRUE,
  coerce_to = "base",
  verbose = FALSE,
  ...
) {
  if (is.null(time_col)) {
    time_index <- attr(data, "time_col")
  } else {
    time_index <- time_col
  }
  if (is.null(id_col)) {
    id_col <- "id"
    if (inherits(data[[attr(data, "group_col")]], "c_grouping"))
      data[["id"]] <- make_unique_id(data[[attr(data, "group_col")]])
    attr(data, "group_names") <- attr(data[[attr(data, "group_col")]], "active_group")
  }
  transformation_info <- attributes(data)
  transformation_info$crs_code <- sf::st_crs(data)$input
  # transformation to cartesian coordinates
  utm_epsg <- sf_to_utm_epsg(data)
  data <- st_transform(data, utm_epsg)
  data_attr <- attributes(data)
  cols <- setdiff(colnames(data), attr(data, "sf_column"))
  data <- data[, cols]

  x_y <- st_coordinates(data[[attr(data, "sf_column")]])
  x_y[is.nan(x_y)] <- NA
  if (is.null(easting_col)) {
    easting_col <- "easting"
    data[["easting"]] <- x_y[, 1]
  } else {
    if (length(easting_col) == 1) {
      lon_names <- c("lon", "long", "longitude") #FIXME: move to options?
      if (easting_col %in% lon_names) easting_col <- "easting"
      data[[easting_col]] <- x_y[, 1] #FIXME: how to check if transformation makes sense?
    } else {
      stop("easting col not identified. Please provide further information on easting_col.")
    }
  }

  if (is.null(northing_col)) {
    northing_col <- "northing"
    data[["northing"]] <- x_y[, 2]
  } else {
    if (length(northing_col) == 1) {
      lat_names <- c("lat", "latitude") #FIXME: move to options?
      if (northing_col %in% lat_names) northing_col <- "northing"
      data[[northing_col]] <- x_y[, 2] #FIXME: how to check if transformation makes sense?
    } else {
      stop("northing col not identified. Please provide further information on northing_col")
    }
  }

  # FIXME: as.data.frame?
  class(data) <- c("data.frame")
  attr(data, "row.names") <- data_attr[["row.names"]]
  attr(data, "transformation_info") <- transformation_info
  as.trackframe(
    data, time_col = time_index, easting_col = easting_col,
    northing_col = northing_col, id_col = id_col, utm_epsg = utm_epsg,
    sort = sort, coerce_to = coerce_to, verbose = verbose, ...
  )
}


#' @export
#' @rdname as_trackframe
as.trackframe.trackframe <- function(data, ...) {
  # time_col = NULL,
  # easting_col = NULL,
  # northing_col = NULL,
  # id_col = NULL,
  # sort = TRUE,
  # coerce_to = "base",
  # verbose = FALSE,
  argg <- list(...)
  if (length(argg) > 0) warning("... arguments are ignored in as.trackframe.trackframe()")
  # FIXME: what do we expect here?
  # FIXME: add transformation_info
  # if (!is.null(time_col)) {
  #   checkmate::assert_string(time_col)
  #   checkmate::assert_choice(time_col, colnames(data))
  #   attr(data, "time") <- time_col
  # }
  # if (!is.null(easting_col)) {
  #   checkmate::assert_choice(easting_col, colnames(data))
  #   checkmate::assert_string(easting_col)
  #   attr(data, "easting") <- easting_col
  # }
  # if (!is.null(northing_col)) {
  #   checkmate::assert_choice(northing_col, colnames(data))
  #   checkmate::assert_string(northing_col)
  #   attr(data, "northing") <- northing_col
  # }
  # if (!is.null(id_col)) {
  #   checkmate::assert_choice(id_col, colnames(data))
  #   checkmate::assert_string(id_col)
  #   attr(data, "id_col") <- id_col
  # }
  #
  # # #coerce_to
  # # if (is.null(coerce_to)) {
  # # } else if (coerce_to == "base") {
  # #   data <- as.data.frame(data)
  # # } else if (coerce_to == "data.table") {
  # #   data <- as.data.table(data)
  # # } else if (coerce_to == "tibble") {
  # #   data <- as_tibble(data)
  # # }
  #
  # if (isTRUE(sort)) {
  #   if (is.null(attr(data, "id"))) {
  #     data <- data[order(time(data)), ]
  #   } else {
  #     data <- data[order(id(data), time(data)), ]
  #   }
  # }
  data
}




is.trackframe <- function(x) {
  inherits(x, "trackframe")
}
