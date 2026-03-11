#' Converts a cocomo object to a trackframe
#'
#' @param xs matrix of x coordinates (UTM eastings) of all individuals
#'  in a group or population (rows) at every time point (columns)
#'  x\[i,t\] gives the x / easting position of individual i at time point t
#' @param ys matrix of y coordinates (UTM northings) of all individuals
#'  in a group or population (rows) at every time point (columns)
#'  y\[i,t\] gives the y / northing position of individual i at time point t
#' @param t vector of timestamps in posixct corresponding to the columns
#'  of x and y matrices. Timestamps must be uniformly sampled,
#'  though it is possible to have gaps (e.g. between different days of recording)
#' @param ids  data frame giving information about the tracked individuals,
#'  with rows correpsonding to the rows of the x and y matrices.
#'  There must be one column called id_code which contains a unique
#'  individual identifier for each animal (e.g. for meerkats:
#'  'VCVM001', for hyenas: 'WRTH', for coatis: 'Luna')
#'  The other columns contained are flexible,
#'  and can include information on age, sex, dominance, etc
#' @param na_omit logical indicator if NAs should be omitted
#' @param crs coordinate reference system
#' @param sort logical, if data should be sorted according to id_col and time_col
#' @param coerce_to the format trackframe is coerced to. `base`,
#' `data.table` and `tibble` are supported. Default is `base` and coerces to a `data.frame`.
#' @param verbose logical, default value is `TRUE`
#'
#' @return an object of class trackframe
#' @export
#'
#' @examples
#' cocomo <- tf_as_cocomo(tf_mini)
#' cocomo_as_tf(cocomo$xs, cocomo$ys, cocomo$t, cocomo$ids)
cocomo_as_tf <- function(
  xs,
  ys,
  t,
  ids = data.frame(id_code = seq_len(NROW(xs))),
  crs = NA,
  na_omit = TRUE,
  sort = TRUE,
  coerce_to = "base",
  verbose = FALSE
) {
  assert_matrix(xs)
  assert_matrix(ys)
  assert_true(NCOL(xs) == NCOL(ys))
  assert_true(NROW(xs) == NROW(ys))
  assert_numeric(t, len = NCOL(xs), any.missing = FALSE)
  assert_data_frame(ids)
  assert_choice("id_code", colnames(ids))
  data <- data.frame(
    "time" = t,
    "easting" = as.vector(base::t(xs)),
    "northing" = as.vector(base::t(ys)),
    "id" = rep(ids$id_code, each = NCOL(xs))
  )
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
  as.trackframe(
    data,
    time_col = "time",
    easting_col = "easting",
    northing_col = "northing",
    id_col = "id",
    crs = crs,
    sort = sort,
    coerce_to = coerce_to,
    verbose = verbose
  )
}


#' Convert a `track_frame` into the `cocomo` format
#'
#' This function converts a `track_frame` object into the cocomo format.
#'
#' @param tf an object inheriting from `track_frame`.
#'
#' @return A list with three components:
#'   \item{x}{A matrix of x-coordinates (easting values). If tf has no id attribute,
#'     this is a single-column matrix. If tf has ids, rows represent different tracks
#'     and columns represent time points.}
#'   \item{y}{A matrix of y-coordinates (northing values). Same structure as x matrix.}
#'   \item{t}{A vector of time values, sorted in ascending order.}
#'
#' @examples
#' tf_as_cocomo(tf_mini)
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
    na_val <- as(NULL, mode(tf[[attr(tf, "easting")]]))
    x <- y <- matrix(na_val, nrow = length(ids), ncol = length(time))
    for (i in seq_along(ids)) {
      id <- ids[i]
      idx <- which(tf[[attr(tf, "id")]] == id)
      m <- match(time, tf[[attr(tf, "time")]][idx])
      x[i, ] <- tf[[attr(tf, "easting")]][idx][m]
      y[i, ] <- tf[[attr(tf, "northing")]][idx][m]
    }
    rownames(x) <- rownames(y) <- ids
  }
  list(xs = x, ys = y, t = time, ids = data.frame(id_code = ids))
}
