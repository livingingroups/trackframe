library(tinytest)
library(trackframe)

"[.data.frame" <- function(x, i, j, drop = FALSE, ...)  {
  base::`[.data.frame`(x, i, j, drop = drop)
}

test_as_trackframe <- function(coerce_to = "base") {
  #dataframe
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  tf <- trackframe(
    data = df, time_col = "time_col", easting_col = "easting_col",
    northing_col = "northing_col", id_col = "id", coerce_to = coerce_to
  )
  tf2 <- trackframe(data = df, coerce_to = coerce_to)
  expect_equal(tf, tf2)
  tf3 <- as.trackframe(
    data = df, time_col = "time_col", easting_col = "easting_col",
    northing_col = "northing_col", id_col = "id", coerce_to = coerce_to
  )
  expect_equal(tf, tf3)
  tf4 <- as.trackframe(data = df, coerce_to = coerce_to)
  expect_equal(tf, tf4)
  expect_inherits(tf, "trackframe")
  expect_equal(dim(df), dim(tf))
  expect_error(trackframe(
    df, time_col = "time_col2", easting_col = "easting_col",
    northing_col = "northing_col", id_col = "id", coerce_to = coerce_to
  ))
  expect_equal(easting(tf), df$easting_col)
  expect_equal(northing(tf), df$northing_col)
  # expect_equal(units(easting(tf))$numerator, "m") #FIXME: if decide to use units
  # expect_equal(units::drop_units(easting(tf)), df$easting_col) #FIXME: if decide to use units
  # expect_equal(units(northing(tf))$numerator, "m") #FIXME: if decide to use units
  # expect_equal(units::drop_units(northing(tf)), df$northing_col) #FIXME: if decide to use units
  expect_equal(id(tf), df$id)
  expect_equal(time(tf), df$time_col)
  expect_inherits(time(tf), "POSIXct")

  #dataframe
  matrix_input <- as.matrix(data.frame(
    time_col = 1:5,
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  ))
  expect_inherits(matrix_input, "matrix")
  tf <- trackframe(matrix_input, time_col = "time_col", easting_col = "easting_col",
    northing_col = "northing_col", id_col = "id", coerce_to = coerce_to)
  expect_inherits(tf, "trackframe")
  expect_equal(dim(df), dim(tf))
  expect_equal(easting(tf), matrix_input[, "easting_col"])
  expect_equal(northing(tf), matrix_input[, "northing_col"])
  # expect_equal(units(easting(tf))$numerator, "m") #FIXME: if decide to use units
  # expect_equal(units::drop_units(easting(tf)), matrix_input[, "easting_col"]) #FIXME: if decide to use units #nolint
  # expect_equal(units(northing(tf))$numerator, "m") #FIXME: if decide to use units
  # expect_equal(units::drop_units(northing(tf)), matrix_input[, "northing_col"]) #FIXME: if decide to use units #nolint
  expect_equal(id(tf), matrix_input[, "id"])
  expect_equal(time(tf), matrix_input[, "time_col"])


  #move2
  library(move2)
  albatross_move2 <- mt_read(mt_example()) |>
    sf::st_transform(3857)
  albatross_move2 <- albatross_move2[!sf::st_is_empty(albatross_move2), ]
  albatross_tf <- as.trackframe(albatross_move2, coerce_to = coerce_to)

  expect_inherits(albatross_tf, "trackframe")
  expect_equal(NROW(albatross_move2), NROW(albatross_tf))

  epsg_code <- trackframe:::sf_to_utm_epsg(albatross_move2)
  albatross_move2_utm <- sf::st_transform(albatross_move2, epsg_code)
  x_y <- sf::st_coordinates(albatross_move2_utm[[attr(albatross_move2_utm, "sf_column")]])
  expect_equal(easting(albatross_tf), x_y[, 1])
  expect_equal(northing(albatross_tf), x_y[, 2])
  # expect_equal(units(easting(albatross_tf))$numerator, "m") #FIXME: if decide to use units
  # expect_equal(units::drop_units(easting(albatross_tf)), x_y[,1]) #FIXME: if decide to use units
  # expect_equal(units(northing(albatross_tf))$numerator, "m") #FIXME: if decide to use units
  # expect_equal(units::drop_units(northing(albatross_tf)), x_y[,2]) #FIXME: if decide to use units
  expect_equal(id(albatross_tf), albatross_move2[[attr(albatross_move2, "track_id_column")]])
  expect_equal(time(albatross_tf), albatross_move2[[attr(albatross_move2, "time_column")]])
  #backtransformation
  albatross_move2_bt <- tf_as_move2(albatross_tf[!is.na(northing(albatross_tf)), ], crs_new = 3857)
  expect_equal(dim(albatross_move2), dim(albatross_move2_bt))
  expect_equal(sf::st_coordinates(albatross_move2), sf::st_coordinates(albatross_move2_bt))
  expect_equal(
    attr(albatross_move2, "track_id_column"),
    attr(albatross_move2_bt, "track_id_column")
  )
  expect_equal(attr(albatross_move2, "time_column"), attr(albatross_move2_bt, "time_column"))


  #sftrack
  library("sftrack")
  # Make tracks from raw data
  data("raccoon", package = "sftrack")
  #raccoon <- read.csv(system.file("extdata/raccoon_data.csv", package="sftrack"))
  raccoon$month <- as.POSIXlt(raccoon$timestamp)$mon + 1
  raccoon$time <- as.POSIXct(raccoon$timestamp, tz = "EST")
  coords <- c("longitude", "latitude")
  group <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon + 1)
  time <- "time"
  error <- "fix"
  crs <- 4326
  # create a sftrack object
  my_sftrack <- as_sftrack(
    data = raccoon, coords = coords, group = group, time = time, error = error, crs = crs
  )

  sftrack_tf <- as.trackframe(my_sftrack, coerce_to = coerce_to)
  expect_inherits(sftrack_tf, "trackframe")
  expect_equal(NROW(my_sftrack), NROW(sftrack_tf))

  epsg_code <- trackframe:::sf_to_utm_epsg(my_sftrack)
  my_sftrack_utm <- sf::st_transform(my_sftrack, epsg_code)
  my_sftrack_utm <- my_sftrack_utm[order(my_sftrack_utm$animal_id, my_sftrack_utm$timestamp), ]
  x_y <- sf::st_coordinates(my_sftrack_utm[[attr(my_sftrack_utm, "sf_column")]])
  x_y[is.nan(x_y)] <- NA
  expect_equal(easting(sftrack_tf), x_y[, 1])
  expect_equal(northing(sftrack_tf), x_y[, 2])
  # expect_equal(units(easting(sftrack_tf))$numerator, "m") #FIXME: if decide to use units
  # expect_equal(units::drop_units(easting(sftrack_tf)), x_y[,1]) #FIXME: if decide to use units
  # expect_equal(units(northing(sftrack_tf))$numerator, "m") #FIXME: if decide to use units
  # expect_equal(units::drop_units(northing(sftrack_tf)), x_y[,2]) #FIXME: if decide to use units
  my_sftrack <- my_sftrack[order(my_sftrack$animal_id, my_sftrack$timestamp), ]
  # expect_equal(id(sftrack_tf), sapply(my_sftrack[[attr(my_sftrack, "group_col")]], deparse))
  expect_equal(
    id(sftrack_tf), trackframe:::make_unique_id(my_sftrack[[attr(my_sftrack, "group_col")]]),
    check.attributes = FALSE
  )
  expect_equal(time(sftrack_tf), my_sftrack[[attr(my_sftrack, "time_col")]])
  #backtransformation
  my_sftrack_bt <- tf_as_sftrack(sftrack_tf[!is.na(northing(sftrack_tf)), ], crs_new = 4326)
  my_sftrack_no_na <- my_sftrack[!is.na(my_sftrack$longitude), ]
  expect_equal(NROW(my_sftrack_no_na), NROW(my_sftrack_bt))
  expect_equal(sf::st_coordinates(my_sftrack_no_na), sf::st_coordinates(my_sftrack_bt))
  expect_equal(attr(my_sftrack_no_na, "group_col"), attr(my_sftrack_bt, "group_col"))
  expect_equal(attr(my_sftrack_no_na, "time_col"), attr(my_sftrack_bt, "time_col"))
}


test_sort <- function(coerce_to) {
  df <- tf_as_xyt(trackframe::tf_mini)
  set.seed(2025)
  df2 <- df[sample(6), ]
  tf_df <- as.trackframe(df2, coerce_to = coerce_to)
  df2_ordered <- df2[order(df2$id, df2$time), ]
  expect_equal(
    as.data.frame(tf_df[, c("id", "time")]),
    df2_ordered[, c("id", "time")], check.attributes = FALSE)
}





# Run all tests
# coerce_to = "base"
# coerce_to = "data.table"
# coerce_to = "tibble"
# coerce_to = NA
lapply(c("base", "data.table", "tibble", NA), function(coerce_to) {
  if (is.na(coerce_to)) coerce_to <- NULL
  test_as_trackframe(coerce_to)
})
