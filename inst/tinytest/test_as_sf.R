# check tf_as_sf works both with crs set and not set
projected_crs <- "EPSG:32632"

df <- as.data.frame(trackframe::path_trackframe[, c(
  "id",
  "easting",
  "northing",
  "time"
)])
tf <- as.trackframe(df, crs = NA)
sf_no_crs <- trackframe::tf_as_sf(tf)
expect_true(is.na(sf::st_crs(sf_no_crs)))

tf_with_crs <- as.trackframe(df, crs = projected_crs)
sf_with_crs <- trackframe::tf_as_sf(tf_with_crs)
expect_equal(sf::st_crs(sf_with_crs)[[1]], projected_crs)

expect_equal(sf::st_coordinates(sf_no_crs), sf::st_coordinates(sf_with_crs))
