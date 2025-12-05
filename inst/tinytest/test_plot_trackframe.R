source("plot_testing_helpers.R")
using("tinysnapshot")

# run to create plots (delete plots if new plots should be created)
# tinytest::run_test_file("inst/tinytest/test_plot_trackframe.R")

library(trackframe)
library(tinytest)

data("tf_mini", package = "trackframe")
data <- tf_mini

f <- function() plot(data)
expect_snapshot_plot(f, label = "plot_data_tf_mini")

f <- function() plot(data, direction = TRUE)
expect_snapshot_plot(f, label = "plot_data_tf_mini_direction")

expect_error(plot_time_path(data))

f <- function() plot_time_path(data[data$id == "track_1", ])
expect_snapshot_plot(f, label = "plot_time_path_track_1")


data$cp_id <- 0
data$cp_id[c(2, 4, 9)] <- 1
f <- function() plot_time_path(data[data$id == "track_1", ], marker = "cp_id")
expect_snapshot_plot(f, label = "plot_time_path_track_1_marker")

f <- function() plot(data, marker = "cp_id")
expect_snapshot_plot(f, label = "plot_data_tf_mini_marker")

f <- function() {
  plot(data, direction = TRUE, marker = "cp_id", start_point = TRUE, end_point = TRUE)
}
expect_snapshot_plot(f, label = "plot_data_tf_mini_dir_marker_start_end")

f <- function() {
  plot(data, direction = TRUE, marker = "cp_id", marker_style = list(col = "yellow"),
    start_point = TRUE, end_point = TRUE)
}
expect_snapshot_plot(f, label = "plot_data_tf_mini_dir_marker_style_start_end")

f <- function() {
  plot(data, direction = TRUE, marker = "cp_id", marker_style = list(cex = 5),
    start_point = TRUE, end_point = TRUE)
}
expect_snapshot_plot(f, label = "plot_data_tf_mini_dir_markers_cex_start_end")

f <- function() {
  plot(data, direction = TRUE, marker = "cp_id", marker_style = list(col = "yellow",
      cex = 1, pch = 4), start_point = TRUE, end_point = TRUE)
}
expect_snapshot_plot(f, label = "plot_data_tf_mini_dir_marker_s_all_cex_start_end")

f <- function() {
  plot(data, marker = "cp_id", direction = TRUE, arrow_style = list(col =
        "yellow"))
}
expect_snapshot_plot(f, label = "plot_data_tf_mini_marker_arrow_style")

f <- function() {
  plot(data, change_point_id = "cp_id", direction = TRUE, arrow_style = list(col =
        "yellow", lwd = 4, length = 3))
}
expect_snapshot_plot(f, label = "plot_data_tf_mini_marker_arrow_style2")
