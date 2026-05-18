source("plot_testing_helpers.R")
using("tinysnapshot")

index <- index_fn_factory("themes")

expect_snapshot_plot(
  \() {
    tinyplot::tinytheme("dark")
    plot(x = data)
  },
  label = index("dark")
)

expect_snapshot_plot(
  \() {
    tinyplot::tinytheme("minimal")
    plot(x = data)
  },
  label = index("minimal")
)


expect_snapshot_plot(
  \() {
    plot(x = data, theme = "dark")
  },
  label = index("dark2")
)

expect_snapshot_plot(
  \() {
    plot(x = data, theme = "minimal")
  },
  label = index("minimal2")
)

expect_snapshot_plot(
  \() {
    tinyplot::tinytheme("tufte")
    plot(x = data)
  },
  label = index("tufte")
)
