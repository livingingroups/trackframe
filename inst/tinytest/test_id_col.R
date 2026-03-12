data <- data.frame(
  time = as.POSIXct(Sys.time() + 2:6),
  easting_col = runif(5, 0, 10),
  northing_col = runif(5, 0, 10)
)

expect_warning({
  tf <- as.trackframe(data, crs = NA)
})
expect_equivalent(tf, {
  data$track_id <- "<id>"
  data
})
