library(trackframe)
library(tinytest)
#'
data("tf_mini", package = "trackframe")
#'
data <- tf_mini

arrow_points <- trackframe:::get_arrow_points(tf = data)
expect_true(all(lapply(arrow_points, NROW) == 3))

plot(data)
plot(data, direction = TRUE)

expect_error(plot_time_path(data))
plot_time_path(data[data$id == "track_1", ])

data$cp_id <- 0
data$cp_id[c(2, 4, 9)] <- 1
plot_time_path(data[data$id == "track_1", ], marker = "cp_id")

plot(data, change_point_id = "cp_id")
plot(data, direction = TRUE, marker = "cp_id", start_point = TRUE, end_point = TRUE)

plot(data, direction = TRUE, marker = "cp_id", marker_style = list(col = "yellow"),
  start_point = TRUE, end_point = TRUE)

plot(data, direction = TRUE, marker = "cp_id", marker_style = list(cex = 5),
  start_point = TRUE, end_point = TRUE)

plot(data, direction = TRUE, marker = "cp_id", marker_style = list(col = "yellow", cex = 1,
    pch = 4), start_point = TRUE, end_point = TRUE)

plot(data, change_point_id = "cp_id", direction = TRUE, arrow_style = list(col = "yellow"))
plot(data, change_point_id = "cp_id", direction = TRUE, arrow_style = list(col = "yellow",
    lwd = 4, length = 3))
