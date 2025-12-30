derive_crs_type <- function(crs) {
  if (is.null(crs)) {
    stop(
      "Missing argument: crs\n",
      "Coordinate reference system must be explicit. ",
      "For unspecified, non-georeferenced, cartesian coordinate  set crs = NA"
    )
  }
  is_longlat <- sf::st_is_longlat(crs)
  crs_type <- if (is.na(is_longlat)) {
    "nongeoreferenced"
  } else if (is_longlat) {
    "geographic"
  } else {
    "projected"
  }
  if (crs_type == "geographic") {
    stop(
      "Expected projected coordinates, got geographic coordinates. ",
      "Please project into an appropriate crs."
    )
  }
  if (is.na(crs)) {
    log_debug(
      "crs provided for non georeferenced data.",
      "Appropriate for custom (non epsg) cartesian coordinate system."
    )
  }
  crs_type
}

#' @examples
#' library(trackframe)
#' df <- data.frame(
#'   time_col = as.POSIXct(Sys.time() + 1:5),
#'   easting_col = runif(5, 0, 10),
#'   northing_col = runif(5, 0, 10),
#'   id = 1:5
#' )
#' tf <- trackframe(df, time_col = "time_col", easting_col = "easting_col",
#'   northing_col = "northing_col", id_col = "id", crs = NA)
#'
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
  crs = NULL,
  ...
) {
  if (!"data.frame" %in% class(data)) {
    data <- data.frame(data)
  }
  as.trackframe(
    data,
    time_col = time_col,
    easting_col = easting_col,
    northing_col = northing_col,
    id_col = id_col,
    crs = crs,
    sort = sort,
    coerce_to = coerce_to,
    ...
  )
}


#' Convert an object to a \code{trackframe}
#'
#' This function converts an object into a \code{trackframe} object,
#' checks that required columns exist and have valid data types.
#' Coordinates must be provided in easting/northing.
#' Coordinate system must be cartesian: either a projected coordinate system
#' for georeferenced data or another cartesian coordinate system for non-
#' georeferenced data (e.g. for captive or simulated data).
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
#'                         northing_col = "northing_col", id_col = "id", crs = NA)
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
  crs = NULL,
  ...
) {
  UseMethod("as.trackframe")
}


#' @param crs coordinate reference system code for of coordinates. Required for non-sf input.
#'  Use `NA` to denote non georeferenced cartesian coordinates.
#' @examples
#' as.trackframe(df_mini, crs = NA)
#'
#' set.seed(2025)
#' df <- data.frame(
#'   x = rnorm(10),
#'   y = rnorm(10),
#'   t = 1:10,
#'   animal_id = c(rep('a', 5), rep('b', 5))
#' )
#'
#'  # crs = NA indicates non georeferenced coordinates
#'  as.trackframe(
#'   df,
#'   time_col = "t",
#'   easting_col = "x",
#'   northing_col = "y",
#'   id_col = "animal_id",
#'   crs = NA
#' )
#'  # with col guessing
#'  as.trackframe(df, coerce_to = "base", crs = NA)
#'
#'  tf_df <- as.trackframe(df, coerce_to = "base", crs = NA)
#'  tf_dt <- as.trackframe(df, coerce_to = "data.table", crs = NA)
#'  tf_tib <- as.trackframe(df, coerce_to = "tibble", crs = NA)
#'
#'  tf_backtransform(tf_df)
#'  tf_backtransform(tf_dt)
#'  tf_backtransform(tf_tib)
#'
#   # Projected CRS, acceptable
#'  tf_df <- as.trackframe(df, coerce_to = "base", crs = "EPSG:32632")
#'  # Non cartesian/lat long crs e.g. "EPSG:4326" is not acceptable.
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
  coerce_to = "base",
  crs = NULL,
  ...
) {
  cn_input <- colnames(data)
  attributes_input <- attributes(data)

  # coerce_to
  assert_choice(
    coerce_to,
    choices = c("base", "data.table", "tibble"),
    null.ok = TRUE
  )
  if (is.null(coerce_to)) {} else if (coerce_to == "base") {
    if (
      inherits(data, "data.table") ||
        inherits(data, "tbl") ||
        inherits(data, "tbl_df")
    ) {
      log_debug("- data coerced by as.data.frame(data)")
      data <- as.data.frame(data)
    }
  } else if (coerce_to == "data.table") {
    if (!inherits(data, "data.table")) {
      log_debug("- data coerced by as.data.table(data)")
      data <- as.data.table(data)
    }
  } else if (coerce_to == "tibble") {
    if (!inherits(data, "tbl") || !inherits(data, "tbl_df")) {
      log_debug("- data coerced by as_tibble(data)")
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
  assert_choice(easting_col, colnames(data), null.ok = FALSE)
  assert_character(easting_col, len = 1, null.ok = FALSE)

  northing_col <- guesses[["northing_col"]][1]
  assert_choice(northing_col, colnames(data), null.ok = FALSE)
  assert_character(northing_col, len = 1, null.ok = FALSE)

  id_col <- guesses[["id_col"]][1]
  if (is.na(id_col)) {
    id_col <- NULL
  }
  assert_choice(id_col, colnames(data), null.ok = TRUE)
  assert_character(id_col, len = 1, null.ok = TRUE)

  assert_numeric(data[[easting_col]])
  assert_numeric(data[[northing_col]])
  assert_numeric(data[[time_col]])

  crs_type <- derive_crs_type(crs)

  attr(data, "time") <- time_col
  attr(data, "easting") <- easting_col
  attr(data, "northing") <- northing_col
  attr(data, "id") <- id_col
  class(data) <- union("trackframe", class(data))

  attr(data, "crs") <- crs
  attr(data, "crs_type") <- crs_type
  log_debug("- %s set as time_col", attr(data, "time"))
  log_debug("- %s set as easting_col", attr(data, "easting"))
  log_debug("- %s set as northing_col", attr(data, "northing"))
  log_debug("- %s set as id_col", attr(data, "id"))
  log_debug("- %i set as crs", attr(data, "crs"))
  log_debug("- %i set as crs_type", attr(data, "crs_type"))

  if (is.null(attr(data, "transformation_info"))) {
    transformation_info <- list()
    transformation_info$attributes <- attributes_input
    transformation_info$class <- attributes_input$class
    transformation_info$names <- cn_input
    transformation_info$coord_names <- c(easting_col, northing_col)
    attr(data, "transformation_info") <- transformation_info
  }

  # sort data by id and time
  if (isTRUE(sort)) {
    if (is.null(attr(data, "id"))) {
      idx <- order(time(data))
    } else {
      idx <- order(id(data), time(data))
    }
    id_hash_unsort <- id_hash(data)
    data <- data[idx, ]
    transformation_info <- attr(data, "transformation_info")
    transformation_info$id_hash_orig <- id_hash_unsort
    transformation_info$id_hash_ordered <- id_hash(data)
    attr(data, "transformation_info") <- transformation_info
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
  crs = NULL,
  ...
) {
  as.trackframe(
    as.data.frame(data),
    time_col = time_col,
    easting_col = easting_col,
    northing_col = northing_col,
    id_col = id_col,
    sort = sort,
    crs = crs,
    coerce_to = coerce_to,
    ...
  )
}


#' @examples
#' # example for move2 objects
#' library(move2)
#' library(trackframe)
#'
#' # column mapping and crs are implicit so do not need to be specified in args.
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
  ...
) {
  time_index <- attr(data, "time_column")

  # FIXME: add similar messages about ignoring time_col, easting_col, northing_col, etc.
  if (!is.null(time_col)) {
    log_debug(
      paste(
        "move2 input so using implicitly configured time column %s",
        "rather that time_col argument %s"
      ),
      time_index,
      dput(time_col)
    )
  } else {
    log_debug(
      paste(
        "move2 input so using implicitly configured time column %s",
      ),
      time_index
    )
  }
  log_debug("Use move2::mt_set_time_column to adjust time column.")

  # move2: The `track_id_column` attribute should be a <character> of length 1
  id_col <- attr(data, "track_id_column")
  transformation_info <- attributes(data)
  # todo": better message
  if ('crs' %in% names(list(...))) {
    stop("crs provided as arg for sf arg. this val will be ignored")
  }
  crs <- sf::st_crs(data)$input
  transformation_info$crs_code <- crs

  data_attr <- attributes(data)
  cols <- setdiff(colnames(data), attr(data, "sf_column"))
  data <- data[, cols]

  x_y <- sf::st_coordinates(data[[attr(data, "sf_column")]])
  x_y[is.nan(x_y)] <- NA
  data[["easting"]] <- x_y[, 1]
  data[["northing"]] <- x_y[, 2]

  class(data) <- c("data.frame")
  attr(data, "row.names") <- data_attr[["row.names"]]
  attr(data, "transformation_info") <- transformation_info
  as.trackframe(
    data,
    time_col = time_index,
    easting_col = "easting",
    northing_col = "northing",
    id_col = id_col,
    sort = sort,
    coerce_to = coerce_to,
    crs = crs,
    ...
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
  ...
) {
  if (is.null(time_col)) {
    time_index <- attr(data, "time_col")
  } else {
    time_index <- time_col
  }
  if (is.null(id_col)) {
    id_col <- "id"
    if (inherits(data[[attr(data, "group_col")]], "c_grouping")) {
      data[["id"]] <- make_unique_id(data[[attr(data, "group_col")]])
    }
    attr(data, "group_names") <- attr(
      data[[attr(data, "group_col")]],
      "active_group"
    )
  }
  transformation_info <- attributes(data)
  crs <- sf::st_crs(data)$input
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
      if (easting_col %in% lon_names) {
        easting_col <- "easting"
      }
      data[[easting_col]] <- x_y[, 1] #FIXME: how to check if transformation makes sense?
    } else {
      stop(
        "easting col not identified. Please provide further information on easting_col."
      )
    }
  }

  if (is.null(northing_col)) {
    northing_col <- "northing"
    data[["northing"]] <- x_y[, 2]
  } else {
    if (length(northing_col) == 1) {
      lat_names <- c("lat", "latitude") #FIXME: move to options?
      if (northing_col %in% lat_names) {
        northing_col <- "northing"
      }
      data[[northing_col]] <- x_y[, 2] #FIXME: how to check if transformation makes sense?
    } else {
      stop(
        "northing col not identified. Please provide further information on northing_col"
      )
    }
  }

  # FIXME: as.data.frame?
  class(data) <- c("data.frame")
  if (!is.null(data$sft_group) && coerce_to %||% "" == "tibble") {
    data$sft_group <- make_unique_id(data$sft_group)
  }

  attr(data, "row.names") <- data_attr[["row.names"]]
  attr(data, "transformation_info") <- transformation_info
  as.trackframe(
    data,
    time_col = time_index,
    easting_col = easting_col,
    northing_col = northing_col,
    id_col = id_col,
    sort = sort,
    coerce_to = coerce_to,
    crs = crs,
    ...
  )
}


#' @export
#' @rdname as_trackframe
as.trackframe.trackframe <- function(
  data,
  time_col = NULL,
  easting_col = NULL,
  northing_col = NULL,
  id_col = NULL,
  sort = TRUE,
  coerce_to = NULL,
  crs = NULL,
  ...
) {
  args_in <- list(
    easting_col = easting_col,
    northing_col = northing_col,
    time_col = time_col,
    id_col = id_col,
    sort = sort,
    coerce_to = coerce_to,
    crs = crs
  )
  if (
    (!is.null(easting_col) || !is.null(easting_col)) &&
      is.null(crs)
  ) {
    warning(
      "trackframe coordinates are being updated without updating crs.",
      "Potential mismatch between coordinates and reference system."
    )
  }
  data_state_in <- list(
    easting_col = attr(data, "easting"),
    northing_col = attr(data, "northing"),
    time_col = attr(data, "time"),
    id_col = attr(data, "id"),

    # tf_state[["sort"]] setting doesn't do anything
    # default is to sort unless user specifically
    # puts sort = FALSE.
    sort = NULL,
    crs = attr(data, "crs")
  )
  args_out <- lapply(names(args_in), \(key) {
    # the user has set an arg away from null,
    # pass that forward to update the trackframe
    # otherwise, pass in current value so no change occurs
    args_in[[key]] %||% data_state_in[[key]]
  })
  names(args_out) <- names(args_in)
  do.call(
    as.trackframe.data.frame,
    c(
      list(data),
      args_out
    )
  )
}


is.trackframe <- function(x) {
  inherits(x, "trackframe")
}
