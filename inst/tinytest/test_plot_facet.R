source("plot_testing_helpers.R")
using("tinysnapshot")

index <- index_fn_factory("facet")

expect_snapshot_plot(
  \() {
    plot(
      x = data,
      facet = TRUE
    )
  },
  label = index("plot_data_tf_mini_facet")
)

expect_silent(
  expect_snapshot_plot(
    \() {
      plot(
        x = data,
        facet = TRUE
      )
    },
    label = index("plot_data_tf_mini_facet")
  )
)

expect_snapshot_plot(
  \() {
    plot(x = data, facet.args = list(free = TRUE))
  },
  label = index("plot_facet_false_free_true")
)

expect_snapshot_plot(
  \() {
    plot(
      x = data,
      facet = TRUE,
      start_indicator_style = list(arrowhead_loc = .5)
    )
  },
  label = index("plot_facet_true_long_start_arrow")
)

expect_snapshot_plot(
  \() {
    plot(x = data, facet = TRUE, end_indicator_style = list(arrowhead_loc = .5))
  },
  label = "plot_facet_true_long_end_arrow"
)

expect_warning(expect_snapshot_plot(
  \() {
    plot(
      x = data,
      facet = TRUE,
      # known issue - this breaks asp=1,
      # tinyplot#555
      facet.args = list("free" = TRUE)
    )
  },
  label = index("plot_data_tf_mini_facet_free")
))
