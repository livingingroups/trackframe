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

# projected -> succeed
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
  tf <- as.trackframe(sftrack_geographic)
})
expect_silent({
  tf <- as.trackframe(sftrack_mini)
})
expect_equal(crs_type(tf), "projected")
