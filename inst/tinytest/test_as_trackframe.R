library(tinytest)
# Test Suite for change_point_test Function
library(trackframe)
library(travelpaths)
# cpttestdata

test_as_trackframe <- function() {
  #dataframe
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  tf <- trackframe(data = df, time_col = "time_col", easting_col = "easting_col",
                          northing_col = "northing_col", id_col = "id")
  tf2 <- trackframe(data = df)
  expect_equal(tf, tf2)
  tf3 <- as.trackframe(data = df, time_col = "time_col", easting_col = "easting_col",
                   northing_col = "northing_col", id_col = "id")
  expect_equal(tf, tf3)
  tf4 <- as.trackframe(data = df)
  expect_equal(tf, tf4)
  expect_inherits(tf, "trackframe")
  expect_equal(dim(df), dim(tf))
  expect_error(trackframe(df, time_col = "time_col2", easting_col = "easting_col",
                           northing_col = "northing_col", id_col = "id"))
  expect_equal(easting(tf), df$easting_col)
  expect_equal(northing(tf), df$northing_col)
  # expect_equal(units(easting(tf))$numerator, "m")
  # expect_equal(units::drop_units(easting(tf)), df$easting_col)
  # expect_equal(units(northing(tf))$numerator, "m")
  # expect_equal(units::drop_units(northing(tf)), df$northing_col)
  expect_equal(id(tf), df$id)
  expect_equal(time(tf), df$time_col)
  expect_inherits(time(tf), "POSIXct")
  
  
  #dataframe
  matrix_input <- as.matrix(data.frame(time_col = 1:5,
                                 easting_col = runif(5, 0, 10),
                                 northing_col = runif(5, 0, 10),
                                 id = 1:5
  ))
  expect_inherits(matrix_input, "matrix")
  tf <- trackframe(matrix_input, time_col = "time_col", easting_col = "easting_col",
                    northing_col = "northing_col", id_col = "id")
  expect_inherits(tf, "trackframe")
  expect_equal(dim(df), dim(tf))
  expect_equal(easting(tf), matrix_input[, "easting_col"])
  expect_equal(northing(tf), matrix_input[, "northing_col"])
  # expect_equal(units(easting(tf))$numerator, "m")
  # expect_equal(units::drop_units(easting(tf)), matrix_input[, "easting_col"])
  # expect_equal(units(northing(tf))$numerator, "m")
  # expect_equal(units::drop_units(northing(tf)), matrix_input[, "northing_col"])
  expect_equal(id(tf), matrix_input[, "id"])
  expect_equal(time(tf), matrix_input[, "time_col"])
  # expect_inherits(time(tf), "POSIXct")
  
  
  #move2
  library(move2)
  albatross_move2 <- mt_read(mt_example()) |>
    sf::st_transform(3857)
  albatross_move2 <- albatross_move2[!sf::st_is_empty(albatross_move2),]
  albatross_tf <- as.trackframe(albatross_move2)
  
  expect_inherits(albatross_tf, "trackframe")
  expect_equal(NROW(albatross_move2), NROW(albatross_tf))

  epsg_code <- trackframe:::sf_to_utm_epsg(albatross_move2)
  albatross_move2_utm <- sf::st_transform(albatross_move2, epsg_code)
  x_y <- sf::st_coordinates(albatross_move2_utm[[attr(albatross_move2_utm, "sf_column")]])
  expect_equal(easting(albatross_tf), x_y[,1])
  expect_equal(northing(albatross_tf), x_y[,2])
  # expect_equal(units(easting(albatross_tf))$numerator, "m")
  # expect_equal(units::drop_units(easting(albatross_tf)), x_y[,1])
  # expect_equal(units(northing(albatross_tf))$numerator, "m")
  # expect_equal(units::drop_units(northing(albatross_tf)), x_y[,2])
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
  
  sftrack_tf <- as.trackframe(my_sftrack)
  expect_inherits(sftrack_tf, "trackframe")
  expect_equal(NROW(my_sftrack), NROW(sftrack_tf))
  
  epsg_code <- trackframe:::sf_to_utm_epsg(my_sftrack)
  my_sftrack_utm <- sf::st_transform(my_sftrack, epsg_code)
  my_sftrack_utm <- my_sftrack_utm[order(my_sftrack_utm$animal_id, my_sftrack_utm$timestamp) ,]
  x_y <- sf::st_coordinates(my_sftrack_utm[[attr(my_sftrack_utm, "sf_column")]])
  x_y[is.nan(x_y)] <- NA
  expect_equal(easting(sftrack_tf), x_y[,1])
  expect_equal(northing(sftrack_tf), x_y[,2])
  # expect_equal(units(easting(sftrack_tf))$numerator, "m")
  # expect_equal(units::drop_units(easting(sftrack_tf)), x_y[,1]) #FIXME: by improved ordering based on JSON
  # expect_equal(units(northing(sftrack_tf))$numerator, "m")
  # expect_equal(units::drop_units(northing(sftrack_tf)), x_y[,2]) #FIXME: by improved ordering based on JSON
  my_sftrack <- my_sftrack[order(my_sftrack$animal_id, my_sftrack$timestamp) ,]
  expect_equal(id(sftrack_tf), sapply(my_sftrack[[attr(my_sftrack, "group_col")]], deparse))
  expect_equal(time(sftrack_tf), my_sftrack[[attr(my_sftrack, "time_col")]])
  #backtransformation
  my_sftrack_bt <- tf_as_sftrack(sftrack_tf[!is.na(northing(sftrack_tf)),], crs_new = 4326)
  my_sftrack_noNA <- my_sftrack[!is.na(my_sftrack$longitude),]
  expect_equal(NROW(my_sftrack_noNA), NROW(my_sftrack_bt))
  expect_equal(sf::st_coordinates(my_sftrack_noNA), sf::st_coordinates(my_sftrack_bt))
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


test_sort <- function() {
  set.seed(2025)
  df <- sim_travel_paths(2,3, format = "data.frame")
  set.seed(2025)
  df2 <- df[sample(6),]
  tf_df <- as.trackframe(df2)
  expect_warning(as.trackframe(df2)) # no crs provided
  df2_ordered <- df2[order(df2$id, df2$time),]
  expect_equal(as.data.frame(tf_df[ , c("id", "time")]), df2_ordered[, c("id", "time")])
  
  # set.seed(2025)
  # tf <- sim_travel_paths(2,3, format = "trackframe")
  # set.seed(2025)
  # tf2 <- tf[sample(6),]
  # tf2_ordered <- tf2[order(tf2$id, tf2$time),]
  # tf3 <- as.trackframe(tf2)
  # expect_equal(tf3[ , c("id", "time")], tf2_ordered[, c("id", "time")])
}

test_errors <- function() {
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(trackframe(data = df, time_col = "time_col", easting_col = "easting_col",
                   northing_col = "northing_col", id_col = "id"))
  expect_error(trackframe(data = df))
  
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    id = 1:5
  )
  
  df <- data.frame(
    time_col = rep("a", 5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(trackframe(data = df))
  
  df <- data.frame(
    time_col = factor(1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(trackframe(data = df))
  
  expect_error(trackframe(data = df))
  
  df <- data.frame(
    time_col2 = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(trackframe(data = df))
  
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col2 = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(trackframe(data = df))
  
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col2 = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(trackframe(data = df))
  
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id2 = 1:5
  )
  expect_silent(trackframe(data = df))
}

test_warnings <- function() {
  #no crs
  set.seed(2025)
  df <- sim_travel_paths(2,3, format = "data.frame")
  expect_warning(as.trackframe(data = df)) # no crs provided
  
  #duplicated guesses
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_silent(trackframe(data = df))
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 2:6),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_warning(trackframe(data = df))
  
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = 1001:1005,
    easting = 1001:1005,
    northing_col = runif(5, 0, 10)
  )
  expect_silent(trackframe(data = df))
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    easting = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_warning(trackframe(data = df))
  
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting = runif(5, 0, 10),
    northing_col = 1001:1005,
    northing = 1001:1005
  )
  expect_silent(trackframe(data = df))
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    northing = runif(5, 0, 10)
  )
  expect_warning(trackframe(data = df))
  
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5,
    track_id = 1:5
  )
  expect_silent(trackframe(data = df))
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5,
    track_id = 2:6
  )
  expect_warning(trackframe(data = df))
}


test_col_guessing <- function() {

  #duplicated guesses
  # with same values
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_silent(trackframe(data = data))
  guesses <- col_guessing(col_names = colnames(data))
  expect_equal(guesses$time_col, c("time", "time_col"))
  expect_equal(guesses$easting_col, c("easting_col"))
  expect_equal(guesses$northing_col, c("northing_col"))
  expect_equal(guesses$id_col, NA)
  expect_silent(trackframe:::validate_guesses(data, guesses))
  
  # with different values
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 2:6),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  guesses <- col_guessing(col_names = colnames(data))
  expect_warning(trackframe:::validate_guesses(data, guesses))
  
  # easting
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = 1001:1005,
    easting = 1001:1005,
    northing_col = runif(5, 0, 10)
  )
  expect_silent(trackframe(data = data))
  guesses <- col_guessing(col_names = colnames(data))
  expect_equal(guesses$time_col, c("time_col"))
  expect_equal(guesses$easting_col, c("easting", "easting_col"))
  expect_equal(guesses$northing_col, c("northing_col"))
  expect_equal(guesses$id_col, NA)
  expect_silent(trackframe:::validate_guesses(data, guesses))
  
  
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    easting = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  guesses <- col_guessing(col_names = colnames(data))
  expect_warning(trackframe:::validate_guesses(data, guesses))
  
  #northing
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting = runif(5, 0, 10),
    northing_col = 1001:1005,
    northing = 1001:1005
  )
  expect_silent(trackframe(data = data))
  guesses <- col_guessing(col_names = colnames(data))
  expect_equal(guesses$time_col, c("time_col"))
  expect_equal(guesses$easting_col, c("easting"))
  expect_equal(guesses$northing_col, c("northing", "northing_col"))
  expect_equal(guesses$id_col, NA)
  expect_silent(trackframe:::validate_guesses(data, guesses))
  
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    northing = runif(5, 0, 10)
  )
  guesses <- col_guessing(col_names = colnames(data))
  expect_warning(trackframe:::validate_guesses(data, guesses))
  
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5,
    track_id = 1:5
  )
  expect_silent(trackframe(data = data))
  guesses <- col_guessing(col_names = colnames(data))
  expect_equal(guesses$time_col, c("time_col"))
  expect_equal(guesses$easting_col, c("easting_col"))
  expect_equal(guesses$northing_col, c("northing_col"))
  expect_equal(guesses$id_col, c("track_id", "id"))
  expect_silent(trackframe:::validate_guesses(data, guesses))
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5,
    track_id = 2:6
  )
  guesses <- col_guessing(col_names = colnames(data))
  expect_warning(trackframe:::validate_guesses(data, guesses))
  
  #col missing
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10)
  )
  expect_error(col_guessing(col_names = colnames(data)))
  
  data <- data.frame(
    time = as.POSIXct(Sys.time() + 1:5),
    northing_col = runif(5, 0, 10)
  )
  expect_error(col_guessing(col_names = colnames(data)))
  
  data <- data.frame(
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_error(col_guessing(col_names = colnames(data)))
  
  #
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col2 = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id2 = 1:5
  )
  expect_error(as.trackframe(data = data))
  expect_silent(as.trackframe(data = data, easting_col = "easting_col2")) #FIXME should this be recognized?
  expect_error(col_guessing(col_names = colnames(data)))
  guesses <- col_guessing(col_names = colnames(data),
                          easting_col_candidates = "easting_col2")
  expect_equal(guesses$time_col, c("time_col"))
  expect_equal(guesses$easting_col, c("easting_col2"))
  expect_equal(guesses$northing_col, c("northing_col"))
  expect_equal(guesses$id_col, NA)
  expect_silent(trackframe:::validate_guesses(data, guesses))
  
  guesses <- col_guessing(col_names = colnames(data),
                          easting_col_candidates = "easting_col2",
                          id_col_candidates = "id2")
  expect_equal(guesses$id_col, "id2")
  expect_silent(trackframe:::validate_guesses(data, guesses))
}

# Run all tests
test_as_trackframe()
test_cocomo()
test_sort()
test_errors()
test_warnings()
test_col_guessing()

