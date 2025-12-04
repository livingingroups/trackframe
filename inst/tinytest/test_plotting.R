library(trackframe)
library(tinytest)
#'
data("tf_mini", package = "trackframe")
#'
data <- tf_mini
data$easting <- data$easting + seq(from = 0, by = 0.0001, length.out = NROW(data))

# test starting points
starting_points <- get_starting_points(tf = data)
expect_length(starting_points, 2)
expect_equal(names(starting_points), c("x0", "y0"))
expect_equal(starting_points$x0, c(16.3725, 16.3730, 16.3734), tolerance = 1e-05,
  check.attributes = FALSE)

starting_points <- get_starting_points(tf = data[1:10, ])
expect_equal(starting_points$x0, c(16.3725, 16.3730, 16.3734), tolerance = 1e-05,
  check.attributes = FALSE)

# test direction points
direction_points <- get_direction_points(tf = data)
expect_length(direction_points, 2)
expect_equal(names(direction_points), c("x1", "y1"))
expect_equal(direction_points$x1, c(16.37344, 16.37310, 16.37413), tolerance = 1e-05,
  check.attributes = FALSE)

direction_points <- get_direction_points(tf = data[1:10, ])
expect_equal(direction_points$x1, c(16.37344, 16.37310), tolerance = 1e-05,
  check.attributes = FALSE)

arrow_points <- c(get_starting_points(tf = data), get_direction_points(tf = data))
expect_true(all(lapply(arrow_points, NROW) == 3))

expect_silent(plot(data))
expect_silent(plot(data, direction = TRUE))

expect_error(plot_time_path(data))
expect_silent(plot_time_path(data[data$id == "track_1", ]))

data$cp_id <- 0
data$cp_id[c(2, 4, 9)] <- 1
expect_silent(plot_time_path(data[data$id == "track_1", ], marker = "cp_id"))

expect_silent(plot(data, change_point_id = "cp_id"))
expect_silent(plot(data, direction = TRUE, marker = "cp_id", start_point = TRUE, end_point = TRUE))

expect_silent(plot(data, direction = TRUE, marker = "cp_id", marker_style = list(col = "yellow"),
    start_point = TRUE, end_point = TRUE))

expect_silent(plot(data, direction = TRUE, marker = "cp_id", marker_style = list(cex = 5),
    start_point = TRUE, end_point = TRUE))

expect_silent(plot(data, direction = TRUE, marker = "cp_id", marker_style = list(col = "yellow",
      cex = 1, pch = 4), start_point = TRUE, end_point = TRUE))

expect_silent(plot(data, change_point_id = "cp_id", direction = TRUE, arrow_style = list(col =
        "yellow")))
expect_silent(plot(data, change_point_id = "cp_id", direction = TRUE, arrow_style = list(col =
        "yellow", lwd = 4, length = 3)))
