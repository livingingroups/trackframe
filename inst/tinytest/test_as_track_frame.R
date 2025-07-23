# library(tinytest)
# Test Suite for change_point_test Function
library(trackframe)
library(travelpaths)
# cpttestdata

test_as_track_frame <- function() {
  #dataframe
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  tf <- track_frame(df, time_col = "time_col", easting_col = "easting_col",
                          northing_col = "northing_col", id_col = "id")
  expect_inherits(tf, "track_frame")
  expect_equal(dim(df), dim(tf))
  expect_error(track_frame(df, time_col = "time_col2", easting_col = "easting_col",
                           northing_col = "northing_col", id_col = "id"))
  expect_equal(easting(tf), df$easting_col)
  expect_equal(northing(tf), df$northing_col)
  expect_equal(id(tf), df$id)
  expect_equal(time(tf), df$time_col)
  
  #dataframe
  matrix_input <- as.matrix(data.frame(time_col = 1:5,
                                 easting_col = runif(5, 0, 10),
                                 northing_col = runif(5, 0, 10),
                                 id = 1:5
  ))
  expect_inherits(matrix_input, "matrix")
  tf <- track_frame(matrix_input, time_col = "time_col", easting_col = "easting_col",
                    northing_col = "northing_col", id_col = "id")
  expect_inherits(tf, "track_frame")
  expect_equal(dim(df), dim(tf))
  expect_equal(easting(tf), matrix_input[, "easting_col"])
  expect_equal(northing(tf), matrix_input[, "northing_col"])
  expect_equal(id(tf), matrix_input[, "id"])
  expect_equal(time(tf), matrix_input[, "time_col"])
  
  #move2
  library(move2)
  albatross_move2 <- mt_read(mt_example()) |>
    sf::st_transform(3857)
  albatross_move2 <- albatross_move2[!sf::st_is_empty(albatross_move2),]
  albatross_tf <- as.track_frame(albatross_move2)
  
  expect_inherits(albatross_tf, "track_frame")
  expect_equal(NROW(albatross_move2), NROW(albatross_tf))

  epsg_code <- trackframe:::sf_to_utm_epsg(albatross_move2)
  albatross_move2_utm <- sf::st_transform(albatross_move2, epsg_code)
  x_y <- sf::st_coordinates(albatross_move2_utm[[attr(albatross_move2_utm, "sf_column")]])
  expect_equal(easting(albatross_tf), x_y[,1])
  expect_equal(northing(albatross_tf), x_y[,2])
  expect_equal(id(albatross_tf), albatross_move2[[attr(albatross_move2, "track_id_column")]])
  expect_equal(time(albatross_tf), albatross_move2[[attr(albatross_move2, "time_column")]])
  #backtransformation
  albatross_move2_bt <- tf_as_move2(albatross_tf[!is.na(northing(albatross_tf)),], crs_new = 3857)
  expect_equal(dim(albatross_move2), dim(albatross_move2_bt))
  expect_equal(sf::st_coordinates(albatross_move2), sf::st_coordinates(albatross_move2_bt))
  expect_equal(attr(albatross_move2, "track_id_column"), attr(albatross_move2_bt, "track_id_column"))
  expect_equal(attr(albatross_move2, "time_column"), attr(albatross_move2_bt, "time_column"))
  # str(albatross_move2)
  # str(albatross_move2_bt)

  
  
  #sftrack
  library("sftrack")
    # Make tracks from raw data
  data("raccoon", package = "sftrack")
  #raccoon <- read.csv(system.file("extdata/raccoon_data.csv", package="sftrack"))
  raccoon$month <- as.POSIXlt(raccoon$timestamp)$mon + 1
  raccoon$time <- as.POSIXct(raccoon$timestamp, tz = "EST")
  coords <- c("longitude","latitude")
  group <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon+1)
  time <- "time"
  error <- "fix"
  crs <- 4326
  # create a sftrack object
  my_sftrack <- as_sftrack(data = raccoon, coords = coords, group = group, time = time, error = error, crs = crs)
  
  sftrack_tf <- as.track_frame(my_sftrack)
  expect_inherits(sftrack_tf, "track_frame")
  expect_equal(NROW(my_sftrack), NROW(sftrack_tf))
  
  epsg_code <- trackframe:::sf_to_utm_epsg(my_sftrack)
  my_sftrack_utm <- sf::st_transform(my_sftrack, epsg_code)
  x_y <- sf::st_coordinates(my_sftrack_utm[[attr(my_sftrack_utm, "sf_column")]]) 
  expect_equal(easting(sftrack_tf), x_y[,1])
  expect_equal(northing(sftrack_tf), x_y[,2])
  expect_equal(id(sftrack_tf), sapply(my_sftrack[[attr(my_sftrack, "group_col")]], deparse))
  expect_equal(time(sftrack_tf), my_sftrack[[attr(my_sftrack, "time_col")]])
  #backtransformation
  my_sftrack_bt <- tf_as_sftrack(sftrack_tf[!is.na(northing(sftrack_tf)),], crs_new = 4326)
  my_sftrack_noNA <- my_sftrack[!is.na(my_sftrack$longitude),]
  expect_equal(NROW(my_sftrack_noNA), NROW(my_sftrack_bt))
  expect_equal(sf::st_coordinates(my_sftrack_noNA), sf::st_coordinates(my_sftrack_bt))
  attributes(my_sftrack_noNA)
  attributes(my_sftrack_bt)
  expect_equal(attr(my_sftrack_noNA, "group_col"), attr(my_sftrack_bt, "group_col"))
  expect_equal(attr(my_sftrack_noNA, "time_col"), attr(my_sftrack_bt, "time_col"))
  # str(my_sftrack)
  # str(my_sftrack_bt)
}


#cocomo
test_cocomo <- function() {
  tf <- sim_travel_paths(3, 3)
  cocomo <-  tf_as_cocomo(tf)
  tf2 <- cocomo_as_tf(cocomo$x, cocomo$y, cocomo$t, cocomo$ids)
  cn <- c("time", "easting", "northing", "id")
  expect_equal(tf[, cn], tf2[, cn])
}


# Run all tests
test_as_track_frame()
test_cocomo()

cat("All tests for as.track_frame completed successfully!\n")
