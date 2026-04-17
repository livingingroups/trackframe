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
  is_trackframe <- sapply(objl, inherits, "trackframe")
  if (sum(is_trackframe) >= 2) {
    # check crs
    obj_crs <- obj_attr[["crs"]]
    crs_equal <- sapply(objl[is_trackframe], function(x) {
      crs_x <- attributes(x)[["crs"]]
      if (is.na(obj_crs) || is.na(crs_x)) {
        warning("crs attribute is NA for at least one object.
          Ensure all inputs share the same crs.")
        return(TRUE)
      }
      isTRUE(all.equal(obj_crs, crs_x))
    })
    stopifnot("crs of trackframes do not coincide" = all(crs_equal))

    #check colnames - if not equal match according to tf cols
    if (length(unique(lapply(objl, function(x) colnames(x)))) > 1) {
      tf_cn <- tf_colnames(objl[[1]])
      objl[seq_along(objl)[-1]] <- lapply(objl[-1], function(x) {
        if (inherits(x, "trackframe")) {
          if (!isTRUE(all.equal(tf_cn, tf_colnames(x)))) {
            tf_colnames(x) <- tf_cn
            warning("Names of key columns of trackframes do not coincide")
          }
        }
        x
      })
    }
  }

  # check time stamp format
  tcol <- time_col(objl[[1]])
  stopifnot("Class of time cols differ." =
      length(unique(lapply(objl, function(x) class(x[[tcol]])))) == 1)

  #remove trackframe class for correct S3 dispatch
  objl <- lapply(objl, function(x) {
    class(x) <- setdiff(class(x), "trackframe")
    x
  })

  obj <- do.call(rbind, objl)
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
  is_trackframe <- sapply(objl, inherits, "trackframe")
  if (sum(is_trackframe) >= 2) {
    # check crs
    if (!all(sapply(objl[is_trackframe], function(x) {
      isTRUE(all.equal(attributes(x)["crs"], obj_attr["crs"]))
    }))) {
      warning("crs of trackframes do not coincide")
    }
  }

  #remove trackframe class for correct S3 dispatch
  objl <- lapply(objl, function(x) {
    class(x) <- setdiff(class(x), "trackframe")
    x
  })

  obj <- do.call(cbind, objl)
  obj_attr$names <- make.unique(colnames(obj), sep = "_")
  obj_attr$transformation_info <- NULL
  attributes(obj) <- obj_attr
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
#' @param by.x specifications of the columns used for merging of x.
#' @param by.y specifications of the columns used for merging of y.
#' @param all logical all = TRUE is shorthand for all.x = TRUE and all.y = TRUE
#' @param all.x logical; if TRUE, then extra rows will be added to the output, one for each row in
#'  x that has no matching row in y. These rows will have NAs in those columns that are usually
#'   filled with values from y. The default is FALSE, so that only rows with data from both x
#'   and y are included in the output.
#' @param all.y logical; analogous to all.x.
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
merge.trackframe <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all,
  all.y = all, sort = TRUE, suffixes = c("", ".y"), ...) {
  assert_trackframe(x)
  validate_tf(x)
  assert_character(by, null.ok = TRUE)
  assert_character(by.x, null.ok = TRUE)
  assert_character(by.y, null.ok = TRUE)
  assert_logical(all)
  assert_logical(all.x)
  assert_logical(all.y)
  assert_logical(sort)
  tf_cols <- tf_colnames(x)[c("time", "id", "easting", "northing")]
  x_attr <- attributes(x)

  # if by is set explicitely, we cannnot check trackframe specific attributes
  if (inherits(y, "trackframe")) {
    validate_tf(y)
    if (!is.null(by)) {
      by.x <- by
      by.y <- by
    } else if ((is.null(by.x) && (is.null(by.y)))) {
      by.x <- tf_colnames(x)[c("time", "id", "easting", "northing")]
      by.y <- tf_colnames(y)[c("time", "id", "easting", "northing")]
    } else {
      stop("Use by, or by.x and by.y to specify the merge columns. Default is tf_colnames(x).")
    }

    # check crs
    if (isTRUE(attr(x, "crs") != attr(y, "crs")) || any(is.na(c(attr(x, "crs"), attr(y, "crs"))))) {
      warning("crs of trackframes do not coincide")
    }

    # check time stamp format
    if (!isTRUE(all.equal(class(x[[time_col(x)]]), class(y[[time_col(y)]]))) &&
        tf_cols["time"] %in% by.x) {
      warning("Class of time cols differ for x and y.")
    }

    class(y) <- setdiff(class(y), "trackframe")
  } else { # no trackframe
    if (!is.null(by)) {
      by.x <- by
      by.y <- by
    } else if (is.null(by.x) || is.null(by.y)) {
      stop("Use by, or by.x and by.y explicitely when merging a trackframe with a non-trackframe
          data.frame/data.table/tibble")
    }
  }

  class(x) <- setdiff(class(x), "trackframe")

  mtf <- merge(x, y, by.x = by.x, by.y = by.y, all = all, all.x = all.x, all.y = all.y, sort = sort,
    suffixes = suffixes, ...)

  x_attr$row.names <- attr(mtf, "row.names")
  x_attr$names <- colnames(mtf)
  x_attr$transformation_info <- NULL
  attributes(mtf) <- x_attr
  #sort
  if (isTRUE(sort)) {
    mtf <- mtf[order(id(mtf), time(mtf)), ]
  }
  return(mtf)
}
