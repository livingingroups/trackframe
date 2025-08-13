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
trackframe <- function(data,
                       time_col = tf_options("time_col"),
                       easting_col = tf_options("easting_col"),
                       northing_col = tf_options("northing_col"),
                       id_col = tf_options("id_col"),
                       sort = TRUE,
                       coerce_to = "base",
                       verbose = FALSE,
                       crs_input = NULL,
                       utm_epsg = NULL,
                       ...) {
  df <- data.frame(data) #FIXME: does not work for tibble + data.table
  as.trackframe(df, time_col = time_col, easting_col = easting_col, 
                 northing_col = northing_col, id_col = id_col,
                crs_input = crs_input, utm_epsg = utm_epsg, sort = sort,
                coerce_to = coerce_to, ...)
}


#' Convert an object to a \code{trackframe}
#'
#' This function converts an object into a \code{trackframe} object,
#' ensuring required columns exist and have valid data types. Coordinates must be provided in easting/northing.
#' Coordinates for  \code{data.frame}, \code{sftrack} and \code{move2} objects are transformed to easting/northing if possible.
#'
#' @param data a \code{data.frame}, a \code{matrix}, an \code{sftrack} object or a \code{move2} object containing the tracking data.
#' @param time_col a character string specifying the column name of the time column. If no column is specified, 
#' the `time_col` is tried to be matched by possible names provided in `tf_options("time_col")`. In case of multiple matches, the first match is chosen.
#' @param easting_col a character string specifying the column name of the easting column. If no column is specified, 
#' the `easting_col` is tried to be matched by possible names provided in `tf_options("easting_col")`. In case of multiple matches, the first match is chosen.
#' @param northing_col a character string specifying the column name of the northing column. If no column is specified, 
#' the `northing_col` is tried to be matched by possible names provided in `tf_options("northing_col")`. In case of multiple matches, the first match is chosen.
#' @param id_col optional character vector specifying identifier column names. If no column is specified, 
#' the `id_col` is tried to be matched by possible names provided in `tf_options("id_col")`. In case of multiple matches, the first match is chosen.
#' @param sort logical, if data should be sorted according to id_col and time_col
#' @param coerce_to the format trackframe is coerced to. `base`, `data.table` and `tibble` are supported. Default is `base` and coerces to a `data.frame`.
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
as.trackframe <- function(data,
                          time_col = tf_options("time_col"),
                          easting_col = tf_options("easting_col"),
                          northing_col = tf_options("northing_col"),
                          id_col = tf_options("id_col"),
                          sort = TRUE,
                          coerce_to = "base",
                          verbose = FALSE,
                           ...) {
  UseMethod("as.trackframe")
}

# tf_control <- function(time_col_guesses = c("t", "timestamp", "time", "time_index", "tindex"),
#                        easting_col_guesses = c("easting", "east", "utm.easting", "lon", "long", "longitude", "x"),
#                        northing_col_guesses = c("northing", "north", "utm.northing", "lat", "latitude", "y"),
#                        id_col_guesses = c("id", "animal_id", "track_id", "trackid")) {
#   structure(as.list(environment()), class = "tf_control")
# }


#' @param crs_input crs code for input of coordinates
#' @param utm_epsg crs value for utm zone of the \code{trackframe} output
#' @examples
#' set.seed(2025)
#' sim_df <- travelpaths::sim_travel_path(5, format = "data.frame")
#' as.trackframe(sim_df, crs_input = 4326)
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
as.trackframe.data.frame <- function(data,
                                     time_col = tf_options("time_col"),
                                     easting_col = tf_options("easting_col"),
                                     northing_col = tf_options("northing_col"),
                                     id_col = tf_options("id_col"),
                                     sort = TRUE,
                                     coerce_to = "base", #FIXME: "data.frame"
                                     verbose = FALSE,
                                     crs_input = NULL,
                                     utm_epsg = NULL,
                                     ...) {
    assert_logical(verbose, len = 1L)
    cn_input <- colnames(data)
    attributes_input <- attributes(data)

    # coerce_to
    assert_choice(coerce_to, choices = c("base", "data.table", "tibble"), null.ok = TRUE)
    if(is.null(coerce_to)) {
    } else if(coerce_to == "base") {
      if(inherits(data, "data.table") | inherits(data, "tbl") | inherits(data, "tbl_df")){
        if(verbose) writeLines("- data coerced by as.data.frame(data)")
        data <- as.data.frame(data)
      }

    } else if(coerce_to == "data.table") {
      if(!inherits(data, "data.table")) {
        if(verbose) writeLines("- data coerced by as.data.table(data)")
        data <- as.data.table(data)
      }
    } else if(coerce_to == "tibble") {
      if(!inherits(data, "tbl") | !inherits(data, "tbl_df")) {
        if(verbose) writeLines("- data coerced by as_tibble(data)")
        if(!is.null(data$sft_group)) data$sft_group <- NULL #FIXME: same transformation as for id
        data <- as_tibble(data)
      }
    }
    
    #columns guessing
    guesses <- guess_all_cols(col_names = colnames(data),
                            time_col_candidates = time_col,
                            easting_col_candidates = easting_col,
                            northing_col_candidates = northing_col,
                            id_col_candidates = id_col)
    
    warn_if_guess_ambiguous(data, guesses)
    
    # # guess time_col
    # if(length(time_col) > 1) {
    #   col_ind <- time_col %in% colnames(data)
    #   if(sum(col_ind) < 1) {
    #     stop("time_col needs to be specified. Guessing not successful.")
    #   } else {
    #     # if(sum(col_ind) >= 1) {
    #       time_cols <- time_col
    #       time_col <- time_col[col_ind][1]
    #       if(sum(col_ind) > 1) {
    #          if(!all(duplicated(t(data[, colnames(data) %in% time_cols]))[-1])) {
    #             warning(sprintf("multiple possible columns found. %s chosen as time_col", time_col))
    #         }
    #       }
    #     # }
    #   }
    # }
    time_col <- guesses[["time_col"]][1]
    assert_choice(time_col, colnames(data), null.ok = FALSE)
    assert_character(time_col, len = 1, null.ok = FALSE)
    assert_numeric(data[[time_col]])
    
    # guess easting_col
    # if(length(easting_col) > 1) {
    #   col_ind <- easting_col %in% colnames(data)
    #   if(sum(col_ind) < 1) {
    #     stop("easting_col needs to be specified. Guessing not successful.")
    #   } else {
    #     easting_cols <- easting_col
    #     easting_col <- easting_col[col_ind][1]
    #     if(sum(col_ind) >= 1) {
    #       if(!all(duplicated(t(data[, colnames(data) %in% easting_cols]))[-1])) {
    #         warning(sprintf("multiple possible columns found. %s chosen as easting_col", easting_col))
    #       }
    #     }
    #   }
    # }
    easting_col <- guesses[["easting_col"]][1]
    assert_choice(easting_col, colnames(data),  null.ok = FALSE)
    assert_character(easting_col, len = 1, null.ok = FALSE)
    
    # # guess northing_col
    # if(length(northing_col) > 1) {
    #   col_ind <- northing_col %in% colnames(data)
    #   if(sum(col_ind) < 1) {
    #     stop("northing_col needs to be specified. Guessing not successful.")
    #   } else {
    #     northing_cols <- northing_col
    #     northing_col <- northing_col[col_ind][1]
    #     if(sum(col_ind) >= 1) {
    #       if(!all(duplicated(t(data[, colnames(data) %in% northing_cols]))[-1])) {
    #         warning(sprintf("multiple possible columns found. %s chosen as northing_col", northing_col))
    #       }
    #     }
    #   }
    # }
    northing_col <- guesses[["northing_col"]][1]
    assert_choice(northing_col, colnames(data),  null.ok = FALSE)
    assert_character(northing_col, len = 1, null.ok = FALSE)

    # # guess id_col
    # if(length(id_col) > 1) {
    #   col_ind <- id_col %in% colnames(data)
    #   if(sum(col_ind) < 1) {
    #     # stop("id_col needs to be specified. Guessing not successful.")
    #     id_col <- NULL
    #   } else {
    #     id_cols <- id_col
    #     id_col <- id_col[col_ind][1]
    #     if(sum(col_ind) >= 1) {
    #       if(!all(duplicated(t(data[, colnames(data) %in% id_cols]))[-1])) {
    #         warning(sprintf("multiple possible columns found. %s chosen as id_col", id_col))
    #       }
    #     }
    #   }
    # } else {
    #   if(length(id_col) == 0) {
    #     id_col <- NULL
    #   }
    # }
    id_col <- guesses[["id_col"]][1]
    if(is.na(id_col)) id_col <- NULL
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
    lat_names <- c("lat","latitude") #FIXME: move to options?
    if(easting_col %in% lon_names | northing_col %in% lat_names) {
      if(!all(c(easting_col %in% lon_names, northing_col %in% lat_names))) {
        stop(sprintf("coordinate system of %s easting_col and northing_col %s do not match.", easting_col, northing_col))
      }
      #transform
      if(is.null(crs_input)) {
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
      
      
      data[["easting"]] <- x_y[,1]
      data[["northing"]] <- x_y[,2]
      attr(data, "easting") <- "easting"
      attr(data, "northing") <- "northing"
    }
    
    attr(data, "utm_epsg") <- utm_epsg
    if(verbose) {
      writeLines(sprintf("- %s set as time_col", attr(data, "time")))
      writeLines(sprintf("- %s set as easting_col", attr(data, "easting")))
      writeLines(sprintf("- %s set as northing_col", attr(data, "northing")))
      writeLines(sprintf("- %s set as id_col", attr(data, "id")))
      writeLines(sprintf("- %i set as utm_epsg", attr(data, "utm_epsg")))
    }
    
    if(is.null(attr(data, "transformation_info"))) {
      transformation_info <- list()
      transformation_info$attributes <- attributes_input
      transformation_info$class <- attributes_input$class
      transformation_info$crs_code <- crs_input
      transformation_info$names <- cn_input
      transformation_info$coord_names <- c(easting_col, northing_col)
      attr(data, "transformation_info") <- transformation_info
    }
    
    # sort data by id and time
    if(isTRUE(sort)) {
      if(is.null(attr(data, "id"))) {
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
as.trackframe.matrix <- function(data,
                                 time_col = tf_options("time_col"),
                                 easting_col = tf_options("easting_col"),
                                 northing_col = tf_options("northing_col"),
                                 id_col = tf_options("id_col"),
                                 sort = TRUE,
                                 coerce_to = "base",
                                 verbose = FALSE,
                                 crs_input = NULL,
                                 utm_epsg = NULL,
                                 ...) {
  as.trackframe(as.data.frame(data), time_col = time_col,
                easting_col = easting_col, northing_col = northing_col,
                id_col = id_col, crs_input = crs_input,
                utm_epsg = utm_epsg, sort = sort,
                coerce_to = coerce_to, verbose = verbose, ...)
}


#' @examples
#' # example for move2 objects
#' library(move2)
#' library(trackframe)
#' set.seed(2025)
#' move2_dat <- travelpaths::sim_travel_path(5, format = "move2")
#' tf <- as.trackframe(data = move2_dat)
#' 
#' move2_dat2 <- tf_backtransform(tf)
#' all.equal(move2_dat, move2_dat2)
#' 
#' @export
#' @rdname as_trackframe
as.trackframe.move2 <- function(data, time_col = NULL,
                                easting_col = NULL,
                                northing_col = NULL,
                                id_col = NULL,
                                sort = TRUE,
                                coerce_to = "base",
                                verbose = FALSE,
                                ...) {
    if(is.null(time_col)) {
      time_index <- attr(data, "time_column")
    } else {
      time_index <- time_col
    }
  if(is.null(id_col)) {
    id_col <- attr(data, "track_id_column") #move2: The `track_id_column` attribute should be a <character> of length 1
  }
    transformation_info <- attributes(data)
    transformation_info$crs_code <- sf::st_crs(data)$input
    # transformation to cartesian coordinates
    utm_epsg <- sf_to_utm_epsg(data)
    # attr(data, "utm_epsg") <- utm_epsg
    data <- st_transform(data, utm_epsg)
    data_attr <- attributes(data)
    cols <- setdiff(colnames(data), attr(data, "sf_column"))
    data <- data[,cols]
    
    # x_y <- st_coordinates(data[[attr(data, "sf_column")]])
    # x_y[is.nan(x_y)] <- NA
    # time_index <- attr(data, "time_column")
    # id_col <- attr(data, "track_id_column") #move2: The `track_id_column` attribute should be a <character> of length 1
    # cols <- setdiff(colnames(data), attr(data, "sf_column"))
    # class(data) <- "list"
    # data <- data[cols]
    # data[["easting"]] <- x_y[, 1]
    # data[["northing"]] <- x_y[, 2]
    # # class(data) <- c("tbl_df", "tbl", "data.frame") #FIXME classes?

    
    x_y <- st_coordinates(data[[attr(data, "sf_column")]])
    x_y[is.nan(x_y)] <- NA
    #FIXME: Can attr(data, "group_col") be null if only 1 track exists?
    if(is.null(easting_col)) {
      easting_col = "easting"
      data[["easting"]] <- x_y[, 1]
    } else {
      data[[easting_col]] <- x_y[, 1] #FIXME: how to check if transformation makes sense?
    }
    
    if(is.null(northing_col)) {
      northing_col = "northing"
      data[["northing"]] <- x_y[, 2]
    } else {
      data[[northing_col]] <- x_y[, 2]
    }

    class(data) <- c("data.frame")
    attr(data, "row.names") <- data_attr[["row.names"]]
    attr(data, "transformation_info") <- transformation_info
    as.trackframe(data, time_col = time_index, easting_col = easting_col,
                  northing_col = northing_col, id_col = id_col, utm_epsg = utm_epsg,
                  sort = sort, coerce_to = coerce_to, verbose = verbose, ...)
}


#' @examples
#' # example for sftrack objects
#' library(sftrack)
#' set.seed(2025)
#' sftrack_dat <- travelpaths::sim_travel_path(5, format = "sftrack")
#' tf <- as.trackframe(data = sftrack_dat)
#' sftrack_dat2 <- tf_backtransform(tf)
#' sftrack_dat2$id <- unlist(sftrack_dat2$sft_group)
#' all.equal(sftrack_dat2, sftrack_dat)
#' 
#' @export
#' @rdname as_trackframe
as.trackframe.sftrack <- function(data,
                                  time_col = NULL,
                                  easting_col = NULL,
                                  northing_col = NULL,
                                  id_col = NULL,
                                  sort = TRUE,
                                  coerce_to = "base",
                                  verbose = FALSE,
                                  ...) {
  
  if(is.null(time_col)) {
    time_index <- attr(data, "time_col")
  } else {
    time_index <- time_col
  }
  if(is.null(id_col)) {
    id_col = "id"
    data[["id"]] <- sapply(data[[attr(data, "group_col")]], deparse)
  }
  transformation_info <- attributes(data)
  transformation_info$crs_code <- sf::st_crs(data)$input
  # transformation to cartesian coordinates
  utm_epsg <- sf_to_utm_epsg(data)
  # attr(data, "utm_epsg") <- utm_epsg
  data <- st_transform(data, utm_epsg)
  data_attr <- attributes(data)
  cols <- setdiff(colnames(data), attr(data, "sf_column"))
  data <- data[,cols]
  


  x_y <- st_coordinates(data[[attr(data, "sf_column")]])
  x_y[is.nan(x_y)] <- NA
  #FIXME: Can attr(data, "group_col") be null if only 1 track exists?
  if(is.null(easting_col)) {
    easting_col = "easting"
    data[["easting"]] <- x_y[, 1]
  } else { #FIXME: check length 1
    data[[easting_col]] <- x_y[, 1] #FIXME: how to check if transformation makes sense?
  }
  
  if(is.null(northing_col)) {
    northing_col = "northing"
    data[["northing"]] <- x_y[, 2]
  } else {
    data[[northing_col]] <- x_y[, 2]
  }
  


  # FIXME: as.data.frame?
  class(data) <- c("data.frame")
  attr(data, "row.names") <- data_attr[["row.names"]]
  attr(data, "transformation_info") <- transformation_info
  as.trackframe(data, time_col = time_index, easting_col = easting_col,
                northing_col = northing_col, id_col = id_col, utm_epsg = utm_epsg,
                sort = sort, coerce_to = coerce_to, verbose = verbose, ...)
}


#' @export
#' @rdname as_trackframe
as.trackframe.trackframe <- function(data,
                                     # time_col = NULL,
                                     # easting_col = NULL,
                                     # northing_col = NULL,
                                     # id_col = NULL,
                                     # sort = TRUE,
                                     # coerce_to = "base",
                                     # verbose = FALSE,
                                     ...) {
  argg <- list(...)
  if(length(argg) > 0) warning("... arguments are ignored in as.trackframe.trackframe()")
  # #FIXME: transformation_info
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
  # # if(is.null(coerce_to)) {
  # # } else if(coerce_to == "base") {
  # #   data <- as.data.frame(data)
  # # } else if(coerce_to == "data.table") {
  # #   data <- as.data.table(data)
  # # } else if(coerce_to == "tibble") {
  # #   data <- as_tibble(data)
  # # }
  # 
  # if(isTRUE(sort)) {
  #   if(is.null(attr(data, "id"))) {
  #     data <- data[order(time(data)), ] 
  #   } else {
  #     data <- data[order(id(data), time(data)), ] 
  #   }
  # }
  data
}


#' Converts a cocomo object to a trackframe
#'
#' @param x matrix of x coordinates (UTM eastings) of all individuals in a group or population (rows) at every time point (columns) x[i,t] gives the x / easting position of individual i at time point t
#' @param y matrix of y coordinates (UTM northings) of all individuals in a group or population (rows) at every time point (columns) y[i,t] gives the y / northing position of individual i at time point t
#' @param t vector of timestamps in posixct corresponding to the columns of x and y matrices. Timestamps must be uniformly sampled, though it is possible to have gaps (e.g. between different days of recording)
#' @param ids  data frame giving information about the tracked individuals, with rows correpsonding to the rows of the x and y matrices. There must be one column called id_code which contains a unique individual identifier for each animal (e.g. for meerkats: 'VCVM001', for hyenas: 'WRTH', for coatis: 'Luna') The other columns contained are flexible, and can include information on age, sex, dominance, etc
#' @param na_omit logical indicator if NAs should be omitted
#' @param utm_epsg crs value for utm zone
#' @param sort logical, if data should be sorted according to id_col and time_col
#' @param coerce_to the format trackframe is coerced to. `base`, `data.table` and `tibble` are supported. Default is `base` and coerces to a `data.frame`.
#' @param verbose logical, default value is \code{TRUE}
#'
#' @return an object of class trackframe
#' @export
#'
#' @examples
#' cocomo <- tf_as_cocomo(travelpaths::sim_travel_paths(3, 3))
#' cocomo_as_tf(cocomo$x, cocomo$y, cocomo$t, cocomo$ids)
cocomo_as_tf <- function(x, y, t, ids, utm_epsg = NULL, na_omit = TRUE,
                         sort = TRUE, coerce_to = "base", verbose = FALSE) {
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
  if (isTRUE(na_omit)) {
    data <- data[!is.na(data[["easting"]]) & !is.na(data[["northing"]]), ]
    rownames(data) <- NULL
  }
  as.trackframe(data, time_col = "time", easting_col = "easting",
                 northing_col = "northing", id_col = "id", utm_epsg = utm_epsg,
                sort = sort, coerce_to = coerce_to, verbose = verbose)
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
#' tf <- travelpaths::sim_travel_paths(3, 3)
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


is.trackframe <- function(x) {
  inherits(x, "trackframe")
}

