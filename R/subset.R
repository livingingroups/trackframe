# This simple wrapper allows that we allways use with = FALSE
#' @exportS3Method NULL
"[.data.frame" <- function(x, i, j, drop = FALSE, ...) {
  base::`[.data.frame`(x, i, j, drop = drop)
}

#' @noRd
#' @export
"[.trackframe" <- function(x, i, j, drop = FALSE, ...) {
  checkmate::assert_logical(drop)
  x_attr <- attributes(x)
  tf_cn <- tf_colnames(x)
  attr_names <- names(x_attr)
  x_attr[names(x_attr) %in% c("names", "row.names", "class")] <- NULL
  class_tf <- attr(x, "class")
  class_tf2 <- class_tf[
    class_tf %in% c("data.frame", "data.table", "tbl_df", "tbl")
  ]
  class2 <- class_tf2[1]
  if (class2 == "data.table") {
    mc <- match.call(expand.dots = TRUE)
    mc[[1L]] <- quote(data.table:::`[.data.table`)
    mc$drop <- drop
    mc$silent <- NULL
    obj <- eval(mc, parent.frame())
  } else {
    if (missing(i)) {
      i <- seq_len(NROW(x))
    }
    if (missing(j)) {
      j <- seq_len(NCOL(x))
    }
    if (length(i) == 1 && length(j) > 1) {
      drop <- FALSE
    }
    if (class2 == "data.frame") {
      obj <- base::`[.data.frame`(x, i, j)
    } else {
      obj <- base::`[.data.frame`(x, i, j, drop = drop)
    }
  }
  if (!is.null(dim(obj))) {
    if (all(tf_cn %in% colnames(obj))) {
      attributes(obj) <- c(attributes(obj), x_attr)[attr_names]
    } else {
      class(obj) <- class(obj)[!class(obj) %in% "trackframe"]
      attributes(obj)[c(
        "time",
        "easting",
        "northing",
        "id",
        "crs",
        "crs_type",
        "transformation_info"
      )] <- NULL
    }
  }
  return(obj)
}
