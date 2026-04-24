# This simple wrapper allows that we allways use with = FALSE
"[.data.frame" <- function(x, i, j, drop = FALSE, ...) {
  base::`[.data.frame`(x, i, j, drop = drop)
}

#' @noRd
#' @export
"[.trackframe" <- function(x, i, j, drop = NULL, silent = FALSE, ...) {
  x_attr <- attributes(x)
  tf_cn <- tf_colnames(x)
  attr_names <- names(x_attr)
  x_attr[names(x_attr) %in% c("names", "row.names", "class")] <- NULL
  class2 <- attr(x, "class")[2]
  if (is.null(drop)) {
    if (class2 == "data.frame") {
      drop <- TRUE
    } else if (class2 == "data.table") {
      drop <- FALSE
    } else if (class2 %in% c("tbl_df", "tbl")) {
      drop <- FALSE
    }
  }
  if (class2 == "data.table") {
    mc <- match.call(expand.dots = TRUE)
    mc[[1L]] <- quote(data.table:::`[.data.table`)
    mc$drop <- drop
    mc$silent <- NULL
    obj <- eval(mc, parent.frame())
  } else {
    has_j <- !missing(j)
    if (missing(i)) {
      i <- seq_len(NROW(x))
    }
    if (missing(j)) {
      j <- seq_len(NCOL(x))
    }
    to_vec <- FALSE
    if (has_j) {
      if (length(j) == 1 && drop == TRUE) {
        to_vec <- TRUE
      }
    }
    if (length(i) == 1 && length(j) > 1) {
      drop <- FALSE
    }
    obj <- base::`[.data.frame`(x, i, j, drop = drop)
  }
  if(!is.null(dim(obj))) {
    if (all(tf_cn %in% colnames(obj))) {
      attributes(obj) <- c(attributes(obj), x_attr)[attr_names]
    } else {
      if (silent == FALSE) {
        class(obj) <- class(obj)[!class(obj) %in% "trackframe"]
        attributes(obj)[c("time", "easting", "northing", "id", "crs", "crs_type",
          "transformation_info")] <- NULL
        warning(sprintf("Key col %s removed. Dropping class trackframe.\n",
          tf_cn[!tf_cn %in% colnames(obj)])) 
      }
    }
  }
  return(obj)
}