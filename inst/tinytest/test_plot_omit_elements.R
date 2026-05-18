source("plot_testing_helpers.R")
using("tinysnapshot")

index <- index_fn_factory("omit_elements")

expect_snapshot_plot(
  \() {
    plot(tf_mini)
    plot(
      tf_mini,
      points = FALSE,
      start_indicator = FALSE,
      end_indicator = FALSE
    )
  },
  label = index("lines_only")
)

expect_snapshot_plot(
  \() {
    plot(tf_mini, lines = FALSE, start_indicator = FALSE, end_indicator = FALSE)
  },
  label = index("points_only")
)

expect_snapshot_plot(
  \() {
    plot(
      tf_mini,
      points_style = list(cex = 8),
      start_indicator = FALSE,
      end_indicator = FALSE
    )
  },
  label = index("no_indicators_points_style")
)

expect_snapshot_plot(
  \() {
    plot(
      tf_mini,
      lines_style = list(lwd = 8),
      start_indicator = FALSE,
      end_indicator = FALSE
    )
  },
  label = index("no_indicators_lines_style")
)
