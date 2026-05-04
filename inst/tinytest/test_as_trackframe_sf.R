library(trackframe)
library(tinytest)
library(sftrack)
library(move2)

data("sftrack_mini", package = "trackframe")
sft_df <- sftrack_mini
sft_df$easting <- NULL
sft_df$northing <- NULL

data("move2_mini")
m2_df <- move2_mini


new_x <- seq_len(nrow(sft_df))
new_y <- seq_len(nrow(sft_df)) + 1

sft_df$x2 <- new_x
sft_df$y2 <- new_y

m2_df$x2 <- new_x
m2_df$y2 <- new_y

tf_from_m2 <- as.trackframe(m2_df)
tf_from_sft <- as.trackframe(sft_df)

sf_df <- sf::st_as_sf(as.data.frame(tf_from_sft), coords = c("easting", "northing"),
  crs = "EPSG:32632")
class(sf_df)
tf_from_sf <- as.trackframe(sf_df)

###

expect_equal(
  easting(tf_from_m2),
  easting(tf_from_sft)
)
expect_equal(
  northing(tf_from_m2),
  northing(tf_from_sft)
)

expect_equal(
  easting_col(tf_from_m2),
  easting_col(tf_from_sft)
)
expect_equal(
  northing_col(tf_from_m2),
  northing_col(tf_from_sft)
)

expect_equal(
  tf_from_m2$easting,
  sf::st_coordinates(sft_df)[, 1]
)

expect_equal(
  tf_from_m2$northing,
  sf::st_coordinates(sft_df)[, 2]
)

expect_equal(
  tf_from_m2$easting,
  tf_from_sft$easting
)
expect_equal(
  tf_from_m2$northing,
  tf_from_sft$northing
)


test_sf_type <- function(sf_data, tf_data) {
  expect_equal(
    easting(tf_data),
    sf::st_coordinates(sf_data)[, 1]
  )

  expect_equal(
    northing(tf_data),
    sf::st_coordinates(sf_data)[, 2]
  )

  expect_warning(as.trackframe(data = sf_data, crs = 4267),
    info = "onflicting crs info provided")
  expect_error(as.trackframe(data = sf_data, crs = 4267),
    info = "Expected projected coordinates, got geographic coordinates.")

  expect_warning(as.trackframe(data = sf_data, crs = 32633))
  tf_from_sf_data <- as.trackframe(data = sf_data, crs = 32633)
  expect_equal(attr(tf_from_sf_data, "crs"), 32633)

  sf_data$time2 <- sf_data$time
  tf_from_sf_data <- as.trackframe(data = sf_data, time_col = "time2")
  expect_equal(attr(tf_from_sf_data, "crs"), "EPSG:32632")
  expect_equal(time_col(tf_from_sf_data), "time2")

  expect_error(as.trackframe(data = sf_data, time_col = "time3"),
    info = "time_col time3 not available in data.")

  sf_data$id2 <- "track2"
  tf_from_sf_data <- as.trackframe(data = sf_data, id_col = "id2")
  expect_equal(id_col(tf_from_sf_data), "id2")

  tf_from_sf_data <- as.trackframe(sf_data, easting_col = "x2", northing_col = "y2")

  expect_equal(
    easting(tf_from_sf_data),
    sf_data$x2
  )
  expect_equal(
    northing(tf_from_sf_data),
    sf_data$y2
  )

  expect_error(as.trackframe(sf_data, easting_col = "x3", northing_col = "y2"))
  expect_error(as.trackframe(sf_data, easting_col = c("x3", "x4"), northing_col = "y2"))

  tf_from_sf_data <- as.trackframe(sf_data, easting_col = c("x3", "easting"),
    northing_col =  c("y3", "northing"))
  expect_equal(
    easting(tf_from_sf_data),
    tf_from_sf_data$easting
  )
  expect_equal(
    northing(tf_from_sf_data),
    tf_from_sf_data$northing
  )

  tf_from_sf_data <- as.trackframe(sf_data, easting_col = c("x2", "easting"),
    northing_col = c("y2", "northing"))
  expect_equal(
    easting(tf_from_sf_data),
    tf_from_sf_data$easting
  )
  expect_equal(
    northing(tf_from_sf_data),
    tf_from_sf_data$northing
  )

  tf_options("sf_easting_col", "x2")
  expect_equal(tf_options("sf_easting_col"), "x2")
  expect_error(as.trackframe(sf_data))
  tf_options("sf_easting_col", "easting")
  tf_options("sf_northing_col", "y2")
  expect_equal(tf_options("sf_northing_col"), "y2")
  expect_error(as.trackframe(sf_data))
  tf_options("sf_northing_col", "y3")
  tf_from_sf_data <- as.trackframe(sf_data)
  expect_equal(tf_colnames(tf_from_sf_data)[c("easting", "northing")],
    c("easting" = "easting", "northing" = "y3"))

  tf_options("sf_easting_col", "easting")
  tf_options("sf_northing_col", "northing")
}

test_sf_type(sf_data = sft_df, tf_data = tf_from_sft)
test_sf_type(sf_data = m2_df, tf_data = tf_from_m2)
test_sf_type(sf_data = sf_df, tf_data = tf_from_sf)
