# # Skip tests if not on Linux
options("tinysnapshot_os" = "Linux")
options("tinysnapshot_device" = "svglite")
options(
  "tinysnapshot_device_args" = list(
    user_fonts = fontquiver::font_families("Liberation"),
    # Needed for stars to render correctly:
    fix_text_size = FALSE
  )
)

# reset theme in every file
tinyplot::tinytheme()

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

# Append sequential numbers to
# snapshot labels so that they
# are in the same order in the folder
# as in the file
idx <- 1
index_fn_factory <- function(suite_label) {
  function(test_label) {
    label <- sprintf("%s%02d_%s", suite_label, idx, test_label)
    idx <<- idx + 1
    label
  }
}

# turn warnings to errors
withr::local_options(warn = 2)

# set up test data
data("tf_mini", package = "trackframe")
data <- tf_mini
data$cp_id <- FALSE
data$cp_id[c(2, 4, 9)] <- TRUE
