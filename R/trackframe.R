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
# TITLE
#' @title
#' Convert an object to a `trackframe`
#'
# DESCRIPTION
#' @description
#' This function converts an object into a `trackframe` object, and
#' checks that the key columns exist and have valid data types.
#' The coordinate system must be Cartesian: either a projected coordinate system
#' for georeferenced data or another coordinate system in Euclidean space for non-georeferenced data
#' (e.g., for captive or simulated data).
#'
# ARGUMENTS
#' @param data
#'   a `data.frame`, `matrix`, `sftrack` object or `move2` object
#'   containing timestamped coordinates of one or multiple animals' locations.
#'   The object should have one row per location, and must contain at least a column for each of
#'   time, easting, and northing.
#'
#' @param time_col
#'   a character string or vector of strings specifying the name, or possible names, of the time
#'   column in `data`.
#'   If a vector is provided, the first element matching a column name of `data` is used.
#'   The data in the time column must be of class `POSIXct` or coercible to `POSIXct`.
#'
#' @param easting_col
#'   a character string or vector of strings specifying the name, or possible names, of the easting
#'   column in `data`.
#'   If a vector is provided, the first element matching a column name of `data` is used.
#'   The data in the easting column must be numeric.
#'
#' @param northing_col
#'   a character string or vector of strings specifying the name, or possible names, of the northing
#'   column in `data`.
#'   If a vector is provided, the first element matching a column name of `data` is used.
#'   The data in the northing column must be numeric.
#'
#' @param id_col
#'   an optional character string or vector of strings specifying the name, or possible names,
#'   of the id column (the column containing the individual identifier(s) of each track) in `data`.
#'   If a vector is provided, the first element matching a column name of `data` is used.
#'   The data in the id column can be numeric, character, or factor.
#'
#' @param sort
#'   logical, if true (default), data will be sorted by id_col and then by time_col.
#'
#' @param coerce_to
#'   the type of dataframe that trackframe is coerced to.
#'  `base`, `data.table` and `tibble` are supported.
#'  The default is `base`, which coerces to a `data.frame` without `data.table` or `tbl` classes.
#'  If NULL, the returned `trackframe` object takes the same dataframe type as the input `data`
#'  object.
#'
#' @param crs
#'    the numeric [EPSG coordinate reference system code](https://epsg.io/) for the coordinates.
#'    This argument is required for non-sf input.
#'    Use `NA` to denote non-georeferenced coordinates.
#'    The coordinate reference system must be Cartesian.
#'
#' @param ... Additional arguments (unused).
#'
# VALUE
#' @return A `trackframe` object with appropriate attributes set.
#'
# DETAILS
#' @details
#' When creating a `trackframe` object from another representation of animal track data,
#' `as.trackframe` identifies the columns representing
#' time (`time_col`), coordinates (`northing_col` & `easting_col`), and track id (`id_col`).
#' It obtains this information from 3 potential sources: 1) information inherited from the input
#' object (see below), 2) arguments supplied directly to `as.trackframe`, and/or 3) package-level
#' options(see [tf_options]).
#'
#' If the input object is a `data.frame`, `data.table`, or `tibble`,
#' no information about the (`time_col`), coordinates (`northing_col` & `easting_col`),
#' and track id (`id_col`) columns is inherited,
#' all are identified from other sources
#' (i.e., arguments supplied directly to the function, or package-level options).
#' If the input object is an `sf` object (but not `sftrack` or `move2`),
#' the `easting_col` and `northing_col` are identified from the input object's `geometry` column,
#' and the `time_col` and `id_col` are identified from other sources.
#' If the input object is an `sftrack` or `move2` object,
#' then all 4 of the key columns are identified from the input object's attributes.

#' See vignette `vignette("identifying-columns")` for details and examples.
#'
#' Trackframe only supports Cartesian coordinates, i.e., coordinates that define locations on a
#' flat surface. Geographic coordinate systems, i.e., those that
#' define locations on a three-dimensional spherical model of the Earth using latitude and
#' longitude, are not supported by trackframe or by (most) functions using trackframe.
#' If you try to pass a non-Cartesian georeferenced coordinate reference system to
#' the `crs` argument, you will get an error. Data
#' containing latitude-longitude coordinates must first be projected before the data is
#' converted to a `trackframe` object.
#' If you set `crs = NA` for non-georeferenced coordinates, the function will assume that these are
#' Cartesian coordinates, though it has no way of checking or warning if not.
#'
# SEE ALSO
#' @seealso
#' [tf_options], [tf_backtransform]
#'
# EXAMPLES
#' @examples
#' library(trackframe)
#'
#' # Create a dataframe
#' set.seed(2025)
#' df <- data.frame(
#'   x = rnorm(10),
#'   y = rnorm(10),
#'   t = 1:10,
#'   animal_id = c(rep('a', 5), rep('b', 5))
#' )
#'
#' # Convert dataframe to trackframe, identify key columns by setting arguments
#' tf <- trackframe(df,
#'                  time_col = "t",
#'                  easting_col = "x",
#'                  northing_col = "y",
#'                  id_col = "animal_id",
#'                  crs = "EPSG:32632")
#'
#' class(tf)
#' attributes(tf)
#'
#' tf <- as.trackframe(df,
#'                     time_col = "t",
#'                     easting_col = "x",
#'                     northing_col = "y",
#'                     id_col = "animal_id",
#'                     crs = "EPSG:32632")
#'
#' # Convert dataframe to trackframe, identify key columns by having as.trackframe match column
#' # names to tf_options
#' tf <- as.trackframe(df_mini, crs = NA)
#'
#' # Convert dataframe to trackframe, identify key columns by having as.trackframe match column
#' # names to multiple options given in arguments
#' tf <- as.trackframe(df_mini,
#'                     time_col = c("t", "time", "timestamp"),
#'                     easting_col = c("x", "easting", "horiz"),
#'                     northing_col = c("y", "northing", "vert"),
#'                     id_col = c("animal", "id"),
#'                     crs = NA)
#'
#' # Convert dataframe to trackframe, identify key columns by setting arguments, also set
#' # crs = NA to indicate non georeferenced coordinates
#' tf <-  as.trackframe(df,
#'                     time_col = "t",
#'                     easting_col = "x",
#'                     northing_col = "y",
#'                     id_col = "animal_id",
#'                     crs = NA)
#'
#' # Convert dataframe to trackframe, identify key columns by having as.trackframe match column
#' # names to tf_options, set crs = NA to indicate non georeferenced coordinates, coerce output
#' # to be:
#' # ... a plain (base R) data.frame
#' tf_df <- as.trackframe(df, coerce_to = "base", crs = NA)
#' attributes(tf_df)$class
#' # ... a data.table
#' tf_dt <- as.trackframe(df, coerce_to = "data.table", crs = NA)
#' attributes(tf_dt)$class
#' # ... a tibble.
#' tf_tib <- as.trackframe(df, coerce_to = "tibble", crs = NA)
#' attributes(tf_tib)$class
#'
#' # Projected CRS, acceptable
#' tf_df <- as.trackframe(df, coerce_to = "base", crs = "EPSG:32632")
#' # Non Cartesian crs, e.g. "EPSG:4326", is not acceptable
#'
#' # Example for move2 objects
#'
#' # Key columns and crs are identified from the attributes of the move2 object
#' tf <- as.trackframe(data = move2_mini)
#'
#'
#' # Example for sftrack objects
#'
#' # Key columns and crs are identified from the attributes of the sftrack object
#' tf <- as.trackframe(data = sftrack_mini)
#'
#'
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


update_warn_if_conflicting <- function(arg_name, arg_value, sf_value, tf_value) {
  warning(sprintf(
    c("Conflicting %s info provided: %s provided as an arg to as.trackframe, but %s implicit in
      sf type object. Using %s."
    ),
    arg_name,
    arg_value,
    sf_value,
    tf_value
  ))
}

update_sf_col_arg <- function(data, arg_name, arg_value, sf_value) {
  if (is.null(arg_value)) {
    arg_value <- sf_value
  } else {
    if (arg_value != sf_value) {
      update_warn_if_conflicting(arg_name, arg_value, sf_value, arg_value)
    }
    arg_value <- arg_value[arg_value %in% colnames(data)][1]
    if (is.na(arg_value)) {
      stop(sprintf("%s argument(s): %s are not available in data.", arg_name, arg_value))
    }
  }
  return(arg_value)
}


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
  time_col <- update_sf_col_arg(data, arg_name = "time_col", arg_value = time_col,
    sf_value = attr(data, "time_column"))

  id_col <- update_sf_col_arg(data, arg_name = "id_col", arg_value = id_col,
    sf_value = attr(data, "track_id_column"))

  as.trackframe.sf(
    data,
    time_col = time_col,
    easting_col = easting_col,
    northing_col = northing_col,
    id_col = id_col,
    sort = sort,
    coerce_to = coerce_to,
    ...
  )
}

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
  time_col <- update_sf_col_arg(data, arg_name = "time_col", arg_value = time_col,
    sf_value = attr(data, "time_col"))

  if (is.null(id_col)) {
    id_col <- "id"
    if (inherits(data[[attr(data, "group_col")]], "c_grouping")) {
      data[["id"]] <- make_unique_id(data[[attr(data, "group_col")]])
    }
    attr(data, "group_names") <- attr(
      data[[attr(data, "group_col")]],
      "active_group"
    )
  } else {
    update_warn_if_conflicting("id_col", id_col, attr(data, "group_col"), id_col)
    if (!id_col %in% colnames(data)) {
      stop(sprintf("id_col %s not available in data.", id_col))
    }
  }

  as.trackframe.sf(
    data,
    time_col = time_col,
    easting_col = easting_col,
    northing_col = northing_col,
    id_col = id_col,
    sort = sort,
    coerce_to = coerce_to,
    ...
  )
}


#' @export
#' @rdname as_trackframe
as.trackframe.sf <- function(
  data,
  time_col = tf_options("time_col"),
  easting_col = NULL,
  northing_col = NULL,
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  ...
) {
  assert_character(time_col)
  assert_character(easting_col, null.ok = TRUE)
  assert_character(northing_col, null.ok = TRUE)
  assert_character(id_col)
  assert_logical(sort)
  assert_choice(
    coerce_to,
    choices = c("base", "data.table", "tibble"),
    null.ok = TRUE
  )

  if ('crs' %in% names(list(...))) {
    update_warn_if_conflicting("crs", list(...)[["crs"]], st_crs(data)[[1]], st_crs(data)[[1]])
  }

  transformation_info <- attributes(data)
  crs <- list(...)[["crs"]] %||% sf::st_crs(data)$input
  transformation_info$crs_code <- crs
  data_attr <- attributes(data)

  x_y <- st_coordinates(data[[attr(data, "sf_column")]])
  x_y[is.nan(x_y)] <- NA
  # set colnames as defined in tf_options() #nolint 
  colnames(x_y) <- c(tf_options("sf_easting_col"), tf_options("sf_northing_col"))
  # error if colnames exist already in data
  if (tf_options("sf_easting_col") %in% colnames(data)) {
    stop(sprintf("Column %s set as sf_easting_col, but exists also in data.
      Remove column %s in data, or change sf_easting_col using tf_options()",
        tf_options("sf_easting_col"), tf_options("sf_easting_col")))
  }
  if (tf_options("sf_northing_col") %in% colnames(data)) {
    stop(sprintf("Column %s set as sf_northing_col, but exists also in data.
      Remove column %s in data, or change sf_northing_col using tf_options()",
        tf_options("sf_northing_col"), tf_options("sf_northing_col")))
  }

  if (is.null(easting_col)) {
    easting_col <- tf_options("sf_easting_col")
  } else if (tf_options("sf_easting_col") %in% easting_col) {
    easting_col <- tf_options("sf_easting_col")
  } else {
    easting_col_orig <- easting_col
    easting_col <- easting_col[easting_col %in% colnames(data)][1]
    if (is.na(easting_col)) {
      stop(sprintf("easting_col argument(s): %s are not available in data.", easting_col_orig))
    }
  }

  if (is.null(northing_col)) {
    northing_col <- tf_options("sf_northing_col")
  } else if (tf_options("sf_northing_col") %in% northing_col) {
    northing_col <- tf_options("sf_northing_col")
  } else {
    northing_col_orig <- northing_col
    northing_col <- northing_col[northing_col %in% colnames(data)][1]
    if (is.na(northing_col)) {
      stop(sprintf("northing_col argument(s): %s are not available in data.", northing_col_orig))
    }
  }

  data[[tf_options("sf_easting_col")]] <- x_y[, 1]
  data[[tf_options("sf_northing_col")]] <- x_y[, 2]
  data <- as.data.frame(data)

  if (!is.null(data$sft_group) && coerce_to %||% "" == "tibble") {
    data$sft_group <- make_unique_id(data$sft_group)
  }

  attr(data, "row.names") <- data_attr[["row.names"]]
  attr(data, "transformation_info") <- transformation_info
  as.trackframe(
    data,
    time_col = time_col,
    easting_col = easting_col,
    northing_col = northing_col,
    id_col = id_col,
    sort = sort,
    coerce_to = coerce_to,
    crs = crs,
    list(...)[!names(list(...)) %in% "crs"]
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
