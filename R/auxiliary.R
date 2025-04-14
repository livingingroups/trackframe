#accessor for cols

#' @export
"index<-track_frame" <- function(x, value) 
{
  stop("TODO")
}

#' @export
index.track_frame <- function(tf){
  tf[, attr(tf, "index")]
}

#longitude
#' @export
longitude <- function(tf){
  assert_class(tf, "track_frame")
  tf[, attr(tf, "lon_col")]
}

#' @export
latitude <- function(tf){
  assert_class(tf, "track_frame")
  tf[, attr(tf, "lat_col")]
}


#select id of trackframe
# id can also be a dataframe
select_id <- function(tf, id) {
  assert_class(tf, "track_frame")
  # tf <- FFT_tf
  # id <- "Abby"
  # id <- c("Abby", "4652")
  
  if(length(id) > 1) {
    tf <- tf[do.call(paste0, tf[, attr(tf, "id_cols")]) %in% paste0(id, collapse = ""), ]
  } else { #TODO we need more sophisticated check here
  tf <- tf[tf[, attr(tf, "id_cols")] == id, ]
  }
  return(tf)
}


# coredata.track.frame <- function(tf){
#   #TODO check what we want to do in coredata
#   ctf <- tf[, c(attr(tf, "index"), attr(tf, "lon_col"), attr(tf, "lat_col"))]
#   return(ctf)
# }