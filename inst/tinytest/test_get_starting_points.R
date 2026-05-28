get_starting_points <- function(tf) {
  trackframe:::get_starting_ending_segments(tf)$startpoint
}

get_direction_points <- function(tf) {
  trackframe:::get_starting_ending_segments(tf)$endpoint
}

data("tf_mini", package = "trackframe")

test_starting_direction <- function(data) {
  data$easting <- data$easting +
    seq(from = 0, by = 0.0001, length.out = NROW(data))

  # test starting points
  starting_points <- get_starting_points(tf = data)
  expect_equal(NROW(starting_points), 3)
  expect_inherits(starting_points, "trackframe")
  expect_equal(
    easting(starting_points),
    c(16.3725, 16.3730, 16.3734),
    tolerance = 1e-05,
    check.attributes = FALSE
  )
  expect_equal(
    northing(starting_points),
    c(48.20835, 48.20835, 48.20835),
    tolerance = 1e-05,
    check.attributes = FALSE
  )

  starting_points <- get_starting_points(tf = data[1:10, ])
  expect_equal(
    easting(starting_points),
    c(16.3725, 16.3730, 16.3734),
    tolerance = 1e-05,
    check.attributes = FALSE
  )

  # test direction points
  direction_points <- get_direction_points(tf = data)
  expect_equal(NROW(direction_points), 3)
  expect_inherits(direction_points, "trackframe")
  expect_equal(
    easting(direction_points),
    c(16.37344, 16.37310, 16.37413),
    tolerance = 1e-05,
    check.attributes = FALSE
  )

  direction_points <- get_direction_points(tf = data[1:10, ])
  expect_equal(
    easting(direction_points),
    c(16.37344, 16.37310),
    tolerance = 1e-05,
    check.attributes = FALSE
  )

  arrow_points <- c(
    get_starting_points(tf = data),
    get_direction_points(tf = data)
  )
  expect_true(all(lapply(arrow_points, NROW) == 3))
}

lapply(c("base", "data.table", "tibble"), function(coerce_to) {
  test_starting_direction(as.trackframe(
    as.data.frame(tf_mini),
    coerce_to = coerce_to,
    crs = NA
  ))
})
