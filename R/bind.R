#' Combine trackframes by rows
#'
#' Take a sequence of trackframes to combine rows.
#'
#' @param ... objects of class trackframe.
#' @param sort logical, if true (default), data will be sorted by id_col and then by time_col.
#'
#' @return an object of class trackframe
#' @export
#'
#' @examples
#' tf1 <- trackframe::tf_mini
#' tf2 <- tf1
#' tf2$id <- c(rep("A",5), rep("B", 4), rep("C",2))
#' tf2$northing <- 20
#' tf2$easting <- 10
#' tf1_tf2 <- rbind(tf1, tf2)
#' tf1_tf2
#' class(tf1_tf2)
rbind.trackframe <- function(..., sort = FALSE) {
  objl <- list(...)
  obj_attr <- attributes(objl[[1]])
  # check if all trackframes
  if (all(sapply(objl, function(x) "trackframe" %in% class(x)))) {
    # check crs
    stopifnot("crs of trackframes do not coincide" = all(sapply(objl, function(x) {
      isTRUE(all.equal(attributes(x)["crs"], obj_attr["crs"]))
    })))
    # check attributes
    attr_check_vars <- c("time", "easting", "northing", "id")
    stopifnot("Names of key columns of trackframes do not coincide" = all(sapply(objl, function(x) {
      isTRUE(all.equal(attributes(x)[attr_check_vars], obj_attr[attr_check_vars]))
    })))
  } else {
    # check if tf_columns are available
    tf_cols <- tf_colnames(objl[[1]])
    stopifnot("Colnames do not match" =
        all(sapply(objl, function(x) all(colnames(x) %in% tf_cols))))
  }
  #check colnames
  stopifnot("Colnames are not equal for all objects." =
      Reduce(all.equal, lapply(objl, function(x) colnames(x))))
  # check time stamp format
  tcol <- time_col(objl[[1]])
  stopifnot("Class of time cols differ." =
      Reduce(all.equal, lapply(objl, function(x) class(x[[tcol]]))))

  time_id_cols <- tf_colnames(objl[[1]])[c("time", "id")]

  #remove trackframe class for correct S3 dispatch
  objl <- lapply(objl, function(x) {
    class(x) <- setdiff(class(x), "trackframe")
    x
  })

  obj <- do.call(rbind, objl)
  # check for duplicates
  if (any(duplicated(obj[, time_id_cols, with = FALSE]))) {
    stop("duplicated time and id entries.")
  }

  obj_attr$row.names <- attr(obj, "row.names")
  obj_attr$transformation_info <- NULL
  attributes(obj) <- obj_attr
  #sort
  if (isTRUE(sort)) {
    obj <- obj[order(id(obj), time(obj)), ]
  }
  return(obj)
}

#' Combine trackframes by columns
#'
#' Take a sequence of trackframes, data.frame, data.table, tibble to combine columns.
#'
#' @param ... objects of class trackframe. Other R objects may be coerced as appropriate.
#'
#' @return an object of class trackframe
#' @export
#'
#' @examples
#' tf1 <- trackframe::tf_mini
#' tf2 <- tf1
#' tf2$id2 <- "A"
#' tf1_tf2 <- cbind(tf1, tf2)
#' tf1_tf2
#' class(tf1_tf2)
cbind.trackframe <- function(...) {
  objl <- list(...)
  obj_attr <- attributes(objl[[1]])
  # check if all trackframes
  if (all(sapply(objl, function(x) "trackframe" %in% class(x)))) {
    # check crs
    if (!all(sapply(objl, function(x) {
      isTRUE(all.equal(attributes(x)["crs"], obj_attr["crs"]))
    }))) {
      warning("crs of trackframes do not coincide")
    }
    # check attributes
    attr_check_vars <- c("time", "easting", "northing", "id")
    stopifnot("Attributes of trackframes do not coincide" = all(sapply(objl, function(x) {
      isTRUE(all.equal(attributes(x)[attr_check_vars], obj_attr[attr_check_vars]))
    })))

    #check tf_columns
    if (!isTRUE(try(do.call(all.equal,
            c(lapply(objl, function(x) as.data.frame(x[, tf_colnames(x), with = FALSE])),
              list(check.attributes = FALSE))), silent = TRUE))) {
      stop("keycols (time, easting, northing, id) are not equal for all trackframes")
    }

    # remove duplicated tf_columns
    objl[seq_along(objl)[-1]] <- lapply(objl[seq_along(objl)[-1]], function(x) {
      x[, !colnames(x) %in% tf_colnames(x), drop = FALSE, with = FALSE]
    })
  }

  #remove trackframe class for correct S3 dispatch
  objl <- lapply(objl, function(x) {
    class(x) <- setdiff(class(x), "trackframe")
    x
  })

  obj <- do.call(cbind, objl)
  obj_attr$names <- colnames(obj)
  obj_attr$transformation_info <- NULL
  attributes(obj) <- obj_attr
  # check for duplicated colnames
  if (any(duplicated(colnames(obj)))) {
    warning("duplicated colnames")
    if (any(colnames(obj)[duplicated(colnames(obj))] %in% tf_colnames(obj))) {
      stop("duplicated tf_colnames. change colnames of tf_columns before applying cbind.")
    }
  }
  return(obj)
}


#' Merge Two trackframes
#'
#' Merge two trackframes by key colums ("time", "id", "easting", "northing"), or do other versions
#' of database join operations.
#'
#' @param x an object of class trackframe
#' @param y an object of class trackframe
#' @param by specifications of the columns used for merging. Default is tf_colnames(x)
#' @param sort logical, if true (default), data will be sorted by id_col and then by time_col.
#' @param suffixes a character vector of length 2 specifying the suffixes to be used for making
#'   unique the names of columns in the result which are not used for merging (appearing in by etc).
#' @param ... other arguments to be passed to appropriate merge methods
#'
#' @return an object of class trackframe
#' @export
#'
#' @examples
#' tf1 <- trackframe::tf_mini
#' tf2 <- tf1
#' tf2$id2 <- "A"
#' tf1_tf2 <- merge(tf1, tf2)
#' tf1_tf2
#' class(tf1_tf2)
#'
#' tf3 <- tf1
#' tf3$id <- c(rep("A",5), rep("B", 4), rep("C",2))
#' merge(tf1, tf3, all = TRUE)
merge.trackframe <- function(x, y, sort = TRUE, suffixes = c("", ".y"), by = NULL, 
 by.x = tf_colnames(x)[c("time", "id")]
 by.y = tf_colnames(y)[c("time", "id")]
) {
  assert_trackframe(x)
  assert_trackframe(y)
  if (!is.null(by)) stop("Use by.x and by.y instead of by in merge.trackframe.")
  # FIXME: add warning for different crs, easting, northing?
  # }
  x_attr <- attributes(x)

  # check crs
  if (attr(x, "crs") != attr(y, "crs")) {
    warning("crs of trackframes do not coincide")
  }
  # check for easting, northing
  en_cols <- tf_colnames(x)[c("easting", "northing")]
  en_cols_y <- tf_colnames(y)[c("easting", "northing")]
  time_col_y <- time_col(y)
  # remove easting, northing of y if not in by.y and equal - to avoid duplicated cols
  if (any(!en_cols_y %in% by.y)) {
    if (isTRUE(all.equal(x[, tf_colnames(x), with = FALSE], y[, tf_colnames(y), with = FALSE],
          check.attributes = FALSE))) {
      y <- y[, !colnames(y) %in% en_cols_y[!en_cols_y %in% by.y], with = FALSE]
    }
  } else {
    if (isTRUE(all.equal(x[, en_cols, with = FALSE], y[, en_cols_y, with = FALSE],
          check.attributes = FALSE))) {
      warning("easting/northing do not coincide.")
    }
  }

  # check time stamp format
  stopifnot("Class of time cols differ." =
      all.equal(class(x[[time_col(x)]]), class(y[[time_col_y]])))

  class(x) <- setdiff(class(x), "trackframe")
  class(y) <- setdiff(class(y), "trackframe")

  mtf <- merge(x, y, by.x = by.x, by.y = by.y, sort = sort, suffixes = suffixes, ...)

  # check for duplicates
  if (any(duplicated(mtf[, time_id_cols, with = FALSE]))) {
    stop("duplicated time and id entries.")
  }
  x_attr$row.names <- attr(mtf, "row.names")
  x_attr$names <- colnames(mtf)
  x_attr$transformation_info <- NULL
  attributes(mtf) <- x_attr
  #check if tf_colnames exist in mtf
  if (!all(tf_colnames(mtf) %in% colnames(mtf))) {
    stop(sprintf("%s not available in merged objects. Use appropriate suffixes.",
        tf_colnames(mtf)[tf_colnames(mtf) %in% colnames(mtf)]))
  }
  #sort
  if (isTRUE(sort)) {
    mtf <- mtf[order(id(mtf), time(mtf)), ]
  }
  return(mtf)
}
