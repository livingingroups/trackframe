library(trackframe)
library(tinytest)

tf <- as.trackframe(df_mini, crs = NA)

expect_equal(names(tf), c("time", "northing", "easting", "id"))

names(tf)[4] <- "animal_id"

expect_equal(
  names(tf),
  c("time", "northing", "easting", "animal_id")
)

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


tf$new_id_col <- c("a", "a", "b", "b", "b")

expect_equal(
  names(tf),
  c("time", "northing", "easting", "animal_id", "new_id_col")
)
expect_equal(
  colnames(tf),
  c("time", "northing", "easting", "animal_id", "new_id_col")
)

expect_equal(
  tf_colnames(tf),
  c(
    time = "time",
    northing = "northing",
    easting = "easting",
    id = "animal_id"
  )
)

tf <- as.trackframe(tf, id_col = "new_id_col")

expect_equal(
  names(tf),
  c("time", "northing", "easting", "animal_id", "new_id_col")
)
expect_equal(
  colnames(tf),
  c("time", "northing", "easting", "animal_id", "new_id_col")
)

names(tf) <- c("time2", "np", "ep", "id_col", "id_col2")

expect_equal(
  names(tf),
  c("time2", "np", "ep", "id_col", "id_col2")
)
expect_equal(
  tf_colnames(tf),
  c(
    time = "time2",
    northing = "np",
    easting = "ep",
    id = "id_col2"
  )
)

expect_error(names(tf) <- c("time2", "np", "ep", "id_col"))


tf <- as.trackframe(tf, id_col = "id_col2")

expect_equal(
  names(tf),
  c("time2", "np", "ep", "id_col", "id_col2")
)

expect_equal(
  tf_colnames(tf),
  c(
    time = "time2",
    northing = "np",
    easting = "ep",
    id = "id_col2"
  )
)


names(tf)[c(1, 4, 5)] <- c("time3", "id_col3", "id_col4")

expect_equal(
  names(tf),
  c("time3", "np", "ep", "id_col3", "id_col4")
)

expect_equal(
  tf_colnames(tf),
  c(
    time = "time3",
    northing = "np",
    easting = "ep",
    id = "id_col4"
  )
)


# data.table
dt <- as.trackframe(tf, coerce_to = "data.table")

names(dt)[c(1, 4, 5)] <- c("time4", "id_col5", "id_col6")
expect_equal(
  names(dt), c("time4", "np", "ep", "id_col5", "id_col6")
)
expect_equal(
  tf_colnames(dt),
  c(
    time = "time4",
    northing = "np",
    easting = "ep",
    id = "id_col6"
  )
)

# tibble
tib <- as.trackframe(tf, coerce_to = "tibble")
names(tib)[c(1, 4, 5)] <- c("time5", "id_col7", "id_col8")
expect_equal(
  names(tib), c("time5", "np", "ep", "id_col7", "id_col8")
)
expect_equal(
  tf_colnames(tib),
  c(
    time = "time5",
    northing = "np",
    easting = "ep",
    id = "id_col8"
  )
)
