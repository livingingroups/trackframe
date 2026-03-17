# This simple wrapper allows that we allways use with = FALSE
"[.data.frame" <- function(x, i, j, drop = FALSE, ...) {
  base::`[.data.frame`(x, i, j, drop = drop)
}

#' @noRd
#' @export
"[.trackframe" <- function(x, i, j, drop = FALSE, ...) {
  x_attr <- attributes(x)
  attr_names <- names(x_attr)
  x_attr[names(x_attr) %in% c("names", "row.names", "class")] <- NULL
  # i <- 1:NROW(x)
  # j <- c("time", "id", "northing", "easting", "id3")
  obj <- base::`[.data.frame`(x, i, j, drop = drop)
  attributes(obj) <- c(attributes(obj), x_attr)[attr_names]
  return(obj)
}
