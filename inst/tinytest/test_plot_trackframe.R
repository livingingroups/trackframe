source("plot_testing_helpers.R")
using("tinysnapshot")

# run to create plots (delete plots if new plots should be created)

library(trackframe)
library(tinytest)

data("tf_mini", package = "trackframe")
data <- tf_mini

expect_snapshot_plot(
  \() plot(x = data, direction = TRUE, facet.args = list("free" = TRUE)),
  label = "plot_data_tf_mini_direction"
)

expect_error(plot_coords_by_time(data))

withr::with_timezone("UTC", {
  expect_snapshot_plot(
    \() plot_coords_by_time(data[data$id == "track_1", ]),
    label = "plot_coords_by_time_track_1"
  )

  data$cp_id <- 0
  data$cp_id[c(2, 4, 9)] <- 1
  expect_snapshot_plot(
    \() plot_coords_by_time(data[data$id == "track_1", ], marker = "cp_id"),
    label = "plot_coords_by_time_track_1_marker"
  )
})

expect_snapshot_plot(
  \() plot(data, marker = "cp_id", facet.args = list("free" = TRUE)),
  label = "plot_data_tf_mini_marker"
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      direction = TRUE,
      marker = "cp_id",
      start_point = TRUE,
      end_point = TRUE,
      facet.args = list("free" = TRUE)
    )
  },
  label = "plot_data_tf_mini_dir_marker_start_end"
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      direction = TRUE,
      marker = "cp_id",
      marker_style = list(col = "yellow"),
      start_point = TRUE,
      end_point = TRUE,
      facet.args = list("free" = TRUE)
    )
  },
  label = "plot_data_tf_mini_dir_marker_style_start_end"
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      direction = TRUE,
      marker = "cp_id",
      marker_style = list(cex = 5),
      start_point = TRUE,
      end_point = TRUE,
      facet.args = list("free" = TRUE)
    )
  },
  label = "plot_data_tf_mini_dir_markers_cex_start_end"
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      direction = TRUE,
      marker = "cp_id",
      marker_style = list(col = "yellow", cex = 1, pch = 4),
      start_point = TRUE,
      end_point = TRUE,
      facet.args = list("free" = TRUE)
    )
  },
  label = "plot_data_tf_mini_dir_marker_s_all_cex_start_end"
)
expect_snapshot_plot(
  \() {
    plot(
      data,
      marker = "cp_id",
      direction = TRUE,
      direction_style = list(col = "yellow"),
      facet.args = list("free" = TRUE)
    )
  },
  label = "plot_data_tf_mini_marker_arrow_style"
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      change_point_id = "cp_id",
      direction = TRUE,
      direction_style = list(col = "yellow", lwd = 4, length = 3),
      facet.args = list("free" = TRUE)
    )
  },
  label = "plot_data_tf_mini_marker_arrow_style2"
)


# test facet = FALSE
expect_snapshot_plot(
  \() plot(data, facet = FALSE),
  label = "plot_data_tf_mini_facetF"
)

expect_snapshot_plot(
  \() plot(data, facet = FALSE, start_point = TRUE, end_point = TRUE),
  label = "plot_data_tf_mini_facetF_start_end"
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      facet = FALSE,
      marker = "cp_id",
      marker_style = list(col = "yellow")
    )
  },
  label = "plot_data_tf_mini_facetF_marker_style"
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      facet = FALSE,
      start_point = TRUE,
      end_point = TRUE,
      direction = TRUE
    )
  },
  label = "plot_data_tf_mini_facetF_start_end_dir"
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      facet = FALSE,
      start_point = TRUE,
      end_point = TRUE,
      direction = TRUE,
      direction_style = list(col = "yellow", lwd = 4, length = 3)
    )
  },
  label = "plot_data_tf_mini_facetF_start_end_dir_style"
)
