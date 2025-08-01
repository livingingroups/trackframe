library(travelpaths)
library(checkmate)
library(trackframe)
set.seed(2025)
move2 <- sim_travel_path(5, format = "move2")
attributes(move2)
data = move2
tf <- as.trackframe(data = move2)
attributes(tf)
attr(tf, "transformation_info")

move2_2 <- tf_as_move2(tf)
move2_2
move2

move2_2 <- tf_as_move2(tf, tf_crs = 32639, crs_new = "EPSG:4326")
move2_2 <- tf_as_move2(tf, tf_crs = 32639, crs_new = 4326)
sf::st_crs(move2_2)

set.seed(2025)
sftrack <- sim_travel_path(5, format = "sftrack")
attributes(sftrack)
data <- sftrack
tf <- as.trackframe(data = sftrack)

df <- sim_travel_path(5, format = "data.frame")
tf <- as.trackframe(data = df)
tf
attr(tf, "transformation_info")
attr(tf, "transformation_info")$attributes
attributes(tf)


tf_backtransform <- function(tf) {
  assert_class(tf, "trackframe")
  transformation_info <- attr(tf, "transformation_info")
  if(is.null(transformation_info)) {
    stop("no transformation info stored to trackframe")
  }
  class_old <- transformation_info$class[1]
  if(class_old == "move2") {
    return(tf_as_move2(tf, tf_crs = attr(tf, "utm_epsg"), crs_new = transformation_info$crs_code))
    #FIXME: data.frame vs. tibble vs.data.table
    #FIXME: drop columns?
    #FIXME: order?
  } else if (class_old ==  "sftrack") {
    return(tf_as_sftrack(tf, tf_crs = attr(tf, "utm_epsg"), crs_new = transformation_info$crs_code))
  } else if (class_old ==  "data.frame") {
  } else if (class_old ==  "matrix") {
    stop("backtransformation not supported for class matrix. Use ?coredata instead.")
  }
}


tfb <- tf_backtransform(tf)
tfb
