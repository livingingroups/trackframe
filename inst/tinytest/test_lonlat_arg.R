library(trackframe)
data("paths_sftrack", package = "trackframe")
class(paths_sftrack)
data_tf <- as.trackframe(
  data = paths_sftrack,
  time_col = "time",
  easting_col = "longitude",
  northing_col = "latitude"
)
expect_equal(attr(data_tf, "easting"), "easting")
expect_equal(attr(data_tf, "northing"), "northing")

#compare with trackframe
data("paths_trackframe", package = "trackframe")
expect_equal(data_tf$easting, paths_trackframe$easting)
expect_equal(data_tf$northing, paths_trackframe$northing)
