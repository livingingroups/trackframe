library(tinytest)
library(sf)
library(sftrack)
library(move2)

projected_crs <- "EPSG:32632"
geographic_crs <- "EPSG:4326" # website says "Used in gps"

## Dataframe

# no crs -> fail
expect_error({
  as.trackframe(df_mini)
})

# non projected -> fail
expect_error({
  as.trackframe(df_mini, crs = geographic_crs)
})

# yes projected -> succeed
expect_silent({
  tf <- as.trackframe(df_mini, crs = projected_crs)
})
expect_equal(crs_type(tf), "projected")

# explicitly non georeferenced -> succeed
expect_silent({
  tf <- as.trackframe(df_mini, crs = NA)
})
expect_equal(crs_type(tf), "nongeoreferenced")


## move2
# move2 mini has EPSG:32632
m2_geographic <- sf::st_transform(move2_mini, geographic_crs)
expect_error({
  tf <- as.trackframe(move2_geographic)
})
expect_silent({
  tf <- as.trackframe(move2_mini)
})
expect_equal(crs_type(tf), "projected")

## sftrack
sftrack_geographic <- sf::st_transform(sftrack_mini, geographic_crs)
expect_error({
  tf <- as.trackframe(
    sftrack_geographic,
    easting_col = "lon",
    northing_col = "lat"
  )
})
expect_silent({
  tf <- as.trackframe(sftrack_mini)
})
expect_equal(crs_type(tf), "projected")


# ### test crs order ###
tf_options("sf_easting_col", "east")
tf_options("sf_northing_col", "north")
data <- cbind.data.frame("northing" = c(-4:-9), "easting" = c(13:18), "id" = "track_1",
  "time" = Sys.Date() + seq_len(6))

# move2
m2 <- mt_as_move2(data, coords = c("northing", "easting"), time_column = "time",
  track_id_column = "id", crs = 4326)
expect_error(as.trackframe(m2),
  info = "Expected projected coordinates, got geographic coordinates.
  Please project into an appropriate crs.")

m2 <- mt_as_move2(data, coords = c("easting", "northing"), time_column = "time",
  track_id_column = "id", crs = 32633) # correct
m2_32633 <- as.trackframe(m2)
expect_equal(easting(m2_32633), data$easting)
expect_equal(northing(m2_32633), data$northing)
expect_equal(attr(m2_32633, "crs"), "EPSG:32633")
expect_equal(sf:::crs_parameters(st_crs(attr(m2_32633, "crs")))$axes$orientation, c(3, 1))

m2 <- mt_as_move2(data, coords = c("northing", "easting"), time_column = "time",
  track_id_column = "id", crs = 32633) # incorrect
m2_32633_f <- as.trackframe(m2)
expect_equal(easting(m2_32633_f), data$northing)
expect_equal(northing(m2_32633_f), data$easting)
expect_equal(attr(m2_32633_f, "crs"), "EPSG:32633")
expect_equal(sf:::crs_parameters(st_crs(attr(m2_32633_f, "crs")))$axes$orientation, c(3, 1))

m2 <- mt_as_move2(data, coords = c("northing", "easting"), time_column = "time",
  track_id_column = "id", crs = 3903) # correct
m2_3903 <- as.trackframe(m2)
expect_equal(easting(m2_3903), data$easting)
expect_equal(northing(m2_3903), data$northing)
expect_equal(attr(m2_3903, "crs"), "EPSG:3903")
expect_equal(sf:::crs_parameters(st_crs(attr(m2_3903, "crs")))$axes$orientation[1:2], c(1, 3))

m2 <- mt_as_move2(data, coords = c("easting", "northing"), time_column = "time",
  track_id_column = "id", crs = 3903) # incorrect
m2_3903_f <- as.trackframe(m2)
expect_equal(easting(m2_3903_f), data$northing)
expect_equal(northing(m2_3903_f), data$easting)
expect_equal(attr(m2_3903_f, "crs"), "EPSG:3903")
expect_equal(sf:::crs_parameters(st_crs(attr(m2_3903_f, "crs")))$axes$orientation[1:2], c(1, 3))

# sftrack
data$time <- as.POSIXct(data$time)
sft <- as_sftrack(data, coords = c("northing", "easting"), time = "time",
  group = "id", crs = 4326)
expect_error(as.trackframe(sft),
  info = "Expected projected coordinates, got geographic coordinates.
  Please project into an appropriate crs.")

sft <- as_sftrack(data, coords = c("easting", "northing"), time = "time",
  group = "id", crs = 32633) # correct
sft_32633 <- as.trackframe(sft)
expect_equal(easting(sft_32633), data$easting)
expect_equal(northing(sft_32633), data$northing)
expect_equal(attr(sft_32633, "crs"), "EPSG:32633")
expect_equal(sf:::crs_parameters(st_crs(attr(sft_32633, "crs")))$axes$orientation, c(3, 1))

sft <- as_sftrack(data, coords = c("northing", "easting"), time = "time",
  group = "id", crs = 32633) # incorrect
sft_32633_f <- as.trackframe(sft)
expect_equal(easting(sft_32633_f), data$northing)
expect_equal(northing(sft_32633_f), data$easting)
expect_equal(attr(sft_32633_f, "crs"), "EPSG:32633")
expect_equal(sf:::crs_parameters(st_crs(attr(sft_32633_f, "crs")))$axes$orientation, c(3, 1))

sft <- as_sftrack(data, coords = c("northing", "easting"), time = "time",
  group = "id", crs = 3903) # correct
sft_3903 <- as.trackframe(sft)
expect_equal(easting(sft_3903), data$easting)
expect_equal(northing(sft_3903), data$northing)
expect_equal(attr(sft_3903, "crs"), "EPSG:3903")
expect_equal(sf:::crs_parameters(st_crs(attr(sft_3903, "crs")))$axes$orientation[1:2], c(1, 3))

sft <- as_sftrack(data, coords = c("easting", "northing"), time = "time",
  group = "id", crs = 3903) # incorrect
sft_3903_f <- as.trackframe(sft)
expect_equal(easting(sft_3903_f), data$northing)
expect_equal(northing(sft_3903_f), data$easting)
expect_equal(attr(sft_3903_f, "crs"), "EPSG:3903")
expect_equal(sf:::crs_parameters(st_crs(attr(sft_3903_f, "crs")))$axes$orientation[1:2], c(1, 3))


# sf
sf <- st_as_sf(data, coords = c("northing", "easting"), crs = 4326)
expect_error(as.trackframe(sf),
  info = "Expected projected coordinates, got geographic coordinates.
  Please project into an appropriate crs.")

sf <- st_as_sf(data, coords = c("easting", "northing"), crs = 32633) # correct
sf_32633 <- as.trackframe(sf)
expect_equal(easting(sf_32633), data$easting)
expect_equal(northing(sf_32633), data$northing)
expect_equal(attr(sf_32633, "crs"), "EPSG:32633")
expect_equal(sf:::crs_parameters(st_crs(attr(sf_32633, "crs")))$axes$orientation, c(3, 1))

sf <- st_as_sf(data, coords = c("northing", "easting"), crs = 32633) # incorrect
sf_32633_f <- as.trackframe(sf)
expect_equal(easting(sf_32633_f), data$northing)
expect_equal(northing(sf_32633_f), data$easting)
expect_equal(attr(sf_32633_f, "crs"), "EPSG:32633")
expect_equal(sf:::crs_parameters(st_crs(attr(sf_32633_f, "crs")))$axes$orientation, c(3, 1))

sf <- st_as_sf(data, coords = c("northing", "easting"), crs = 3903) # correct
sf_3903 <- as.trackframe(sf)
expect_equal(easting(sf_3903), data$easting)
expect_equal(northing(sf_3903), data$northing)
expect_equal(attr(sf_3903, "crs"), "EPSG:3903")
expect_equal(sf:::crs_parameters(st_crs(attr(sf_3903, "crs")))$axes$orientation[1:2], c(1, 3))

sf <- st_as_sf(data, coords = c("easting", "northing"), crs = 3903) # incorrect
sf_3903_f <- as.trackframe(sf)
expect_equal(easting(sf_3903_f), data$northing)
expect_equal(northing(sf_3903_f), data$easting)
expect_equal(attr(sf_3903_f, "crs"), "EPSG:3903")
expect_equal(sf:::crs_parameters(st_crs(attr(sf_3903_f, "crs")))$axes$orientation[1:2], c(1, 3))
