source("plot_testing_helpers.R")
using("tinysnapshot")

index <- index_fn_factory("markers")

expect_snapshot_plot(
  \() {
    plot(data, marker = "cp_id")
  },
  label = index("data_tf_mini_marker")
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      direction = TRUE,
      marker = "cp_id",
      start_indicator = FALSE,
      end_indicator = FALSE,
      facet = TRUE
    )
  },
  label = index("data_tf_mini_dir_marker_no_start_end")
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      direction = TRUE,
      marker = "cp_id",
      marker_style = list(col = "yellow"),
      start_indicator = FALSE,
      end_indicator = FALSE,
      facet = TRUE
    )
  },
  label = index("data_tf_mini_dir_marker_style_no_start_end")
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      direction = TRUE,
      marker = "cp_id",
      marker_style = list(cex = 5),
      start_indicator = FALSE,
      end_indicator = FALSE,
      facet = TRUE
    )
  },
  label = index("data_tf_mini_dir_markers_cex_no_start_end")
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      marker = "cp_id",
      marker_style = list(col = "black", cex = 1, pch = "\u2605"),
      facet = TRUE
    )
  },
  label = index("data_tf_mini_dir_marker_s_all_cex")
)
expect_snapshot_plot(
  \() {
    plot(
      data,
      marker = "cp_id",
      start_indicator_style = list(col = "yellow"),
      facet = TRUE
    )
  },
  label = index("data_tf_mini_marker_start_arrow_style")
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      marker = "cp_id",
      end_indicator_style = list(col = "yellow"),
      facet = TRUE
    )
  },
  label = index("data_tf_mini_marker_end_arrow_style")
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      change_point_id = "cp_id",
      start_indicator_style = list(col = "yellow", lwd = 4, length = 3),
      facet = TRUE
    )
  },
  label = index("data_tf_mini_marker_start_arrow_style2")
)


# test facet = FALSE
expect_snapshot_plot(
  \() plot(data),
  label = index("data_tf_mini_facetF")
)

expect_snapshot_plot(
  \() plot(data, start_point = TRUE, end_point = TRUE),
  label = index("data_tf_mini_start_end")
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
  label = index("data_tf_mini_marker_style")
)
