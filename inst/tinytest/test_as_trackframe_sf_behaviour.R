library(trackframe)
library(tinytest)

data("sftrack_mini")

sft_df <- sftrack_mini
sft_df$easting <- NULL
sft_df$northing <- NULL

data("move2_mini")
m2_df <- move2_mini

new_x <- seq_len(nrow(sft_df))
new_y <- seq_len(nrow(sft_df)) + 1

sft_df$x2 <- new_x
sft_df$y2 <- new_y
class(sft_df) <- c("sftrack", "sf", "data.frame")
m2_df$x2 <- new_x
m2_df$y2 <- new_y
class(m2_df) <- c("move2", "sf", "data.frame")
tf_from_m2 <- as.trackframe(m2_df, easting_col = "x2", northing_col = "y2")
tf_from_sft <- as.trackframe(sft_df, easting_col = "x2", northing_col = "y2")

# passes uses output of st_coordinates
expect_equal(
  easting(tf_from_sft),
  sf::st_coordinates(sft_df)[, 1]
)

expect_equal(
  northing(tf_from_sft),
  sf::st_coordinates(sft_df)[, 2]
)

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
  tf_from_m2$x2,
  sf::st_coordinates(sft_df)[, 1]
)

expect_equal(
  tf_from_m2$y2,
  sf::st_coordinates(sft_df)[, 2]
)

expect_equal(
  tf_from_m2$x2,
  tf_from_sft$x2
)
expect_equal(
  tf_from_m2$y2,
  tf_from_sft$y2
)

expect_warning(as.trackframe(data = m2_df, crs = "4267"))

m2_df$time2 <- m2_df$time
class(m2_df) <- c("move2", "sf", "data.frame")
tf_from_m2_2 <- as.trackframe(data = m2_df, time_col = "time2")
expect_equal(time_col(tf_from_m2_2), "time2")

m2_df$id2 <- "track2"
class(m2_df) <- c("move2", "sf", "data.frame")
tf_from_m2_3 <- as.trackframe(data = m2_df, id_col = "id2")
expect_equal(id_col(tf_from_m2_3), "id2")
