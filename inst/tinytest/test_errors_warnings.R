library(tinytest)
library(trackframe)

"[.data.frame" <- function(x, i, j, drop = FALSE, ...) {
  base::`[.data.frame`(x, i, j, drop = drop)
}

test_errors <- function(coerce_to = coerce_to) {
  set.seed(2025)

  # northing missing
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(trackframe(
    data = df,
    time_col = "time_col",
    easting_col = "easting_col",
    northing_col = "northing_col",
    id_col = "id",
    coerce_to = coerce_to,
    crs = NA
  ))
  expect_error(trackframe(data = df, coerce_to = coerce_to, crs = NA))

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    id = 1:5
  )

  df <- data.frame(
    time_col = rep("a", 5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "time_col invalid"
  )

  df <- data.frame(
    time_col = factor(1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "time_col invalid"
  )

  expect_error(trackframe(data = df, coerce_to = coerce_to, crs = NA))

  df <- data.frame(
    time_col2 = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "`time_col2` not close enough to be guessed"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col2 = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "`easting_col2` not close enough to be guessed"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col2 = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "`northing_col2` not close enough to be guessed"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id2 = 1:5
  )
  expect_silent(
    tf <- trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "id is optional, this doesn't fail, but id2 col is not recognized as id"
  )
  expect_null(id(tf))
}

test_warnings <- function(coerce_to = "base") {
  # no crs
  set.seed(2025)
  tf <- trackframe::tf_mini
  df <- tf_as_xyt(tf) #sim_travel_paths(2,3, format = "data.frame")

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_silent(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "duplicated guesses"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 2:6),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_warning(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "multiple conflicting matches for time col"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = 1001:1005,
    easting = 1001:1005,
    northing_col = runif(5, 0, 10)
  )
  expect_silent(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "multiple matches for easting_col but they have the same data so no warning"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    easting = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_warning(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "multiple conflicting matches for easting_col"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting = runif(5, 0, 10),
    northing_col = 1001:1005,
    northing = 1001:1005
  )
  expect_silent(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "no conflict, northing"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    northing = runif(5, 0, 10)
  )
  expect_warning(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "yes conflict, northing"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5,
    track_id = 1:5
  )
  expect_silent(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "no conflict, id"
  )

  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5,
    track_id = 2:6
  )
  expect_warning(
    trackframe(data = df, coerce_to = coerce_to, crs = NA),
    info = "yes conflict, id"
  )
}

# Run all tests
# coerce_to = "base"
# coerce_to = "data.table"
# coerce_to = "tibble"
# coerce_to = NA
lapply(c("base", "data.table", "tibble", NA), function(coerce_to) {
  if (is.na(coerce_to)) {
    coerce_to <- NULL
  }
  test_errors(coerce_to)
  test_warnings(coerce_to)
})
