source("plot_testing_helpers.R")
using("tinysnapshot")

index <- index_fn_factory("basic_plot")

expect_snapshot_plot(
  \() {
    plot(x = data, start_indicator_style = list(arrowhead_loc = .5))
  },
  label = index("long_start_arrow")
)

expect_snapshot_plot(
  \() {
    plot(x = data, end_indicator_style = list(arrowhead_loc = .5))
  },
  label = index("long_end_arrow")
)


expect_snapshot_plot(
  \() {
    plot(
      data
    )
  },
  label = index("data_tf_mini_start_end_dir")
)

expect_snapshot_plot(
  \() {
    plot(
      data,
      start_indicator_style = list(col = "yellow", lwd = 4, length = 3)
    )
  },
  label = index("data_tf_mini_start_end_dir_style")
)

expect_snapshot_plot(
  \() {
    data2 <- data
    data2[[id_col(data2)]] <- paste(id(data2), "_2")
    plot(
      rbind(data, data2)
    )
  },
  label = index("more_tracks")
)
