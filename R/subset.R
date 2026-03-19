# This simple wrapper allows that we allways use with = FALSE
"[.data.frame" <- function(x, i, j, drop = FALSE, ...) {
  base::`[.data.frame`(x, i, j, drop = drop)
}

#' @noRd
#' @export
"[.trackframe" <- function(x, i, j, drop = TRUE, ...) {
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
  if (isTRUE(to_vec)) {
    obj <- base::`[.data.frame`(x, i, j, drop = drop)
  } else {
    x_attr <- attributes(x)
    attr_names <- names(x_attr)
    x_attr[names(x_attr) %in% c("names", "row.names", "class")] <- NULL
    obj <- base::`[.data.frame`(x, i, j, drop = drop)
    attributes(obj) <- c(attributes(obj), x_attr)[attr_names]
  }
  return(obj)
}
