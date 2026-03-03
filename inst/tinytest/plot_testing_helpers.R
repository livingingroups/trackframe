library(tinytest)
library(tinyplot)
library(tinysnapshot)

# # Skip tests if not on Linux
options("tinysnapshot_os" = "Linux")
options("tinysnapshot_device" = "svglite")
options(
  "tinysnapshot_device_args" = list(
    user_fonts = fontquiver::font_families("Liberation")
  )
)

# reset theme in every file
tinytheme()

using("tinysnapshot")
expect_snapshot_plot <- function(
  current,
  label,
  ...
) {
  if (covr::in_covr()) {
    current()
  } else {
    tinysnapshot::expect_snapshot_plot(current, label, ...)
  }
}
