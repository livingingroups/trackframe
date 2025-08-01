tf <- sim_travel_paths(3, 3)
tf

split_by_id <- function(tf) {
  tf_split <- split(tf, id(tf))
  class(tf_split) <- "list_of_trackframes"
  return(tf_split)
}

x <- split_by_id(tf)
class(x[[1]])
class(x)

s <- sapply(split(tf, id(tf)), function(tf){
  average_speed_over_time(easting(tf), northing(tf), time(tf))
})
