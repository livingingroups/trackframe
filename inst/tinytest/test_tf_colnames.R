# - change make a different, existing column the coords
# - rename coord column, keeping it as coord col
# - change contents of coord col

tf <- as.trackframe(df_mini, crs = NA)

expect_equal(
  tf_colnames(tf),
  c(
    time = "time",
    northing = "northing",
    easting = "easting",
    id = "id"
  )
)
expect_equal(colnames(tf), c("time", "northing", "easting", "id"))

tf_colnames(tf)[['id']] <- 'animal_id'

expect_equal(
  tf_colnames(tf),
  c(
    time = "time",
    northing = "northing",
    easting = "easting",
    id = "animal_id"
  )
)

expect_equal(colnames(tf), c("time", "northing", "easting", "animal_id"))

tf_colnames(tf)['id'] <- 'ind_id'

expect_equal(
  tf_colnames(tf),
  c(
    time = "time",
    northing = "northing",
    easting = "easting",
    id = "ind_id"
  )
)

tf$new_id_col <- c('a', 'a', 'b', 'b', 'b')

expect_equal(
  tf_colnames(tf),
  c(
    time = "time",
    northing = "northing",
    easting = "easting",
    id = "ind_id"
  )
)
expect_equal(
  colnames(tf),
  c("time", "northing", "easting", "ind_id", "new_id_col")
)

tf <- as.trackframe(tf, id_col = 'new_id_col')

expect_equal(
  tf_colnames(tf),
  c(
    time = "time",
    northing = "northing",
    easting = "easting",
    id = "new_id_col"
  )
)
expect_equal(
  colnames(tf),
  c("time", "northing", "easting", "ind_id", "new_id_col")
)

tf_colnames(tf) <- c(
  easting = "eep",
  northing = "nooooo",
  time = "mite",
  id = "id_col"
)


tf <- tf[, c(3, 2, 1, 5, 4)]
expect_equal(
  tf_colnames(tf),
  c(
    easting = "eep",
    northing = "nooooo",
    time = "mite",
    id = "id_col"
  )
)

tf <- as.trackframe(tf, id_col = 'ind_id')

expect_equal(
  tf_colnames(tf),
  c(
    easting = "eep",
    northing = "nooooo",
    time = "mite",
    id = "ind_id"
  )
)
