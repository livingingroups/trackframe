library(trackframe)
library(tinytest)
library(sftrack)
library(move2)

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

m2_df$x2 <- new_x
m2_df$y2 <- new_y

tf_from_m2 <- as.trackframe(m2_df)
tf_from_sft <- as.trackframe(sft_df)

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

expect_warning(as.trackframe(data = m2_df, crs = "4267"))

m2_df$time2 <- m2_df$time
tf_from_m2_2 <- as.trackframe(data = m2_df, time_col = "time2")
expect_equal(time_col(tf_from_m2_2), "time2")

m2_df$id2 <- "track2"
tf_from_m2_3 <- as.trackframe(data = m2_df, id_col = "id2")
expect_equal(id_col(tf_from_m2_3), "id2")

###
tf_from_m2 <- as.trackframe(m2_df, easting_col = "x2", northing_col = "y2")

expect_equal(
  easting(tf_from_m2),
  m2_df$x2
)
expect_equal(
  northing(tf_from_m2),
  m2_df$y2
)

tf_from_sft <- as.trackframe(sft_df, easting_col = "x2", northing_col = "y2")

expect_equal(
  easting(tf_from_sft),
  m2_df$x2
)
expect_equal(
  northing(tf_from_sft),
  m2_df$y2
)

expect_error(as.trackframe(sft_df, easting_col = "x3", northing_col = "y2"))
expect_error(as.trackframe(sft_df, easting_col = c("x3", "x4"), northing_col = "y2"))

tf_from_sft2 <- as.trackframe(sft_df, easting_col = c("x3", "easting"),
  northing_col =  c("y3", "northing"))
expect_equal(
  easting(tf_from_sft2),
  tf_from_sft2$easting
)
expect_equal(
  northing(tf_from_sft2),
  tf_from_sft2$northing
)


tf_from_sft3 <- as.trackframe(sft_df, easting_col = c("x2", "easting"),
  northing_col = c("y2", "northing"))
expect_equal(
  easting(tf_from_sft3),
  tf_from_sft3$easting
)
expect_equal(
  northing(tf_from_sft3),
  tf_from_sft3$northing
)
