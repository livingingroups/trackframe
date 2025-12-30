library(tinytest)
library(trackframe)

"[.data.frame" <- function(x, i, j, drop = FALSE, ...) {
  base::`[.data.frame`(x, i, j, drop = drop)
}

expect_tf_class <- function(actual_tf_class, from_class, coerce_to) {
  tf_classes <- c("trackframe", "data.frame")
  tf_subclasses <- c("data.table", "tbl", "tbl_df")
  expected_idx <- list(
    "base" = c(FALSE, FALSE, FALSE),
    "data.table" = c(TRUE, FALSE, FALSE),
    "tibble" = c(FALSE, TRUE, TRUE),
    "NULL" = tf_subclasses %in% from_class
  )[[coerce_to %||% "NULL"]]

  expect_true(all(tf_classes %in% actual_tf_class))

  expect_equal(
    tf_subclasses %in% actual_tf_class,
    expected_idx,
    info = "Expect presence/absence of tf subclasses is as expected"
  )
  expect_true(
    all(
      actual_tf_class %in%
        c(
          from_class,
          tf_classes,
          tf_subclasses
        )
    ),
    info = "Expect all classes come from either source obj or known tf (sub)classes"
  )
}

test_as_trackframe_data_frame <- function(from = "base", coerce_to = "base") {
  # dataframe
  df <- list(
    "base" = data.frame,
    "data.table" = data.table::as.data.table,
    "tibble" = dplyr::as_tibble
  )[[from]](data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  ))
  tf <- trackframe(
    data = df,
    time_col = "time_col",
    easting_col = "easting_col",
    northing_col = "northing_col",
    id_col = "id",
    coerce_to = coerce_to,
    crs = NA
  )
  tf2 <- trackframe(data = df, coerce_to = coerce_to, crs = NA)
  expect_equal(tf, tf2)
  tf3 <- as.trackframe(
    data = df,
    time_col = "time_col",
    easting_col = "easting_col",
    northing_col = "northing_col",
    id_col = "id",
    coerce_to = coerce_to,
    crs = NA
  )
  expect_equal(tf, tf3)
  tf4 <- as.trackframe(data = df, coerce_to = coerce_to, crs = NA)
  expect_equal(tf, tf4)

  expect_tf_class(class(tf), class(df), coerce_to)
  expect_equal(dim(df), dim(tf))
  expect_error(trackframe(
    df,
    time_col = "time_col2",
    easting_col = "easting_col",
    northing_col = "northing_col",
    id_col = "id",
    coerce_to = coerce_to,
    crs = NA
  ))
  expect_equal(easting(tf), df$easting_col)
  expect_equal(northing(tf), df$northing_col)
  expect_equal(id(tf), df$id)
  expect_equal(time(tf), df$time_col)
  expect_inherits(time(tf), "POSIXct")
}

test_as_trackframe <- function(coerce_to = "base") {
  matrix_input <- as.matrix(data.frame(
    time_col = 1:5,
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  ))
  expect_inherits(matrix_input, "matrix")
  tf <- trackframe(
    matrix_input,
    time_col = "time_col",
    easting_col = "easting_col",
    northing_col = "northing_col",
    id_col = "id",
    coerce_to = coerce_to,
    crs = NA
  )
  expect_inherits(tf, "trackframe")
  expect_equal(dim(matrix_input), dim(tf))
  expect_equal(easting(tf), matrix_input[, "easting_col"])
  expect_equal(northing(tf), matrix_input[, "northing_col"])
  expect_equal(id(tf), matrix_input[, "id"])
  expect_equal(time(tf), matrix_input[, "time_col"])

  #move2
  library(move2)
  albatross_move2 <- mt_read(mt_example()) |>
    sf::st_transform(3857)
  albatross_move2 <- albatross_move2[!sf::st_is_empty(albatross_move2), ]
  albatross_tf <- as.trackframe(albatross_move2, coerce_to = coerce_to)

  expect_inherits(albatross_tf, "trackframe")
  expect_equal(NROW(albatross_move2), NROW(albatross_tf))

  x_y <- sf::st_coordinates(albatross_move2[[attr(
    albatross_move2,
    "sf_column"
  )]])
  expect_equal(easting(albatross_tf), x_y[, 1])
  expect_equal(northing(albatross_tf), x_y[, 2])
  expect_equal(
    id(albatross_tf),
    albatross_move2[[attr(albatross_move2, "track_id_column")]]
  )
  expect_equal(
    time(albatross_tf),
    albatross_move2[[attr(albatross_move2, "time_column")]]
  )
  #backtransformation
  albatross_move2_bt <- tf_as_move2(albatross_tf[
    !is.na(northing(albatross_tf)),
  ])
  expect_equal(dim(albatross_move2), dim(albatross_move2_bt))
  expect_equal(
    sf::st_coordinates(albatross_move2),
    sf::st_coordinates(albatross_move2_bt)
  )
  expect_equal(
    attr(albatross_move2, "track_id_column"),
    attr(albatross_move2_bt, "track_id_column")
  )
  expect_equal(
    attr(albatross_move2, "time_column"),
    attr(albatross_move2_bt, "time_column")
  )

  #sftrack
  library("sftrack")
  # Make tracks from raw data
  data("raccoon", package = "sftrack")
  #raccoon <- read.csv(system.file("extdata/raccoon_data.csv", package="sftrack"))
  raccoon$month <- as.POSIXlt(raccoon$timestamp)$mon + 1
  raccoon$time <- as.POSIXct(raccoon$timestamp, tz = "EST")
  coords <- c("longitude", "latitude")
  group <- list(
    id = raccoon$animal_id,
    month = as.POSIXlt(raccoon$timestamp)$mon + 1
  )
  time <- "time"
  error <- "fix"
  crs <- 4326
  # create a sftrack object
  raccoon_sftrack <- as_sftrack(
    data = raccoon,
    coords = coords,
    group = group,
    time = time,
    error = error,
    crs = crs
  ) |>
    sf::st_transform(suggest_utm_crs(raccoon$latitude, raccoon$longitude))

  raccoon_tf <- as.trackframe(raccoon_sftrack, coerce_to = coerce_to)
  expect_inherits(raccoon_tf, "trackframe")
  expect_equal(NROW(raccoon_sftrack), NROW(raccoon_tf))

  raccoon_sftrack <- raccoon_sftrack[
    order(raccoon_sftrack$animal_id, raccoon_sftrack$timestamp),
  ]
  x_y <- sf::st_coordinates(raccoon_sftrack[[attr(
    raccoon_sftrack,
    "sf_column"
  )]])
  x_y[is.nan(x_y)] <- NA
  expect_equal(easting(raccoon_tf), x_y[, 1])
  expect_equal(northing(raccoon_tf), x_y[, 2])
  raccoon_sftrack <- raccoon_sftrack[
    order(raccoon_sftrack$animal_id, raccoon_sftrack$timestamp),
  ]
  # expect_equal(id(sftrack_tf), sapply(my_sftrack[[attr(my_sftrack, "group_col")]], deparse))
  expect_equal(
    id(raccoon_tf),
    trackframe:::make_unique_id(raccoon_sftrack[[attr(
      raccoon_sftrack,
      "group_col"
    )]]),
    check.attributes = FALSE
  )
  expect_equal(
    time(raccoon_tf),
    raccoon_sftrack[[attr(raccoon_sftrack, "time_col")]]
  )
  #backtransformation
  sftrack_bt <- tf_as_sftrack(raccoon_tf[!is.na(northing(raccoon_tf)), ])
  sftrack_no_na <- raccoon_sftrack[!is.na(raccoon_sftrack$longitude), ]
  expect_equal(NROW(sftrack_no_na), NROW(sftrack_bt))
  expect_equal(
    sf::st_coordinates(sftrack_no_na),
    sf::st_coordinates(sftrack_bt)
  )
  expect_equal(attr(sftrack_no_na, "group_col"), attr(sftrack_bt, "group_col"))
  expect_equal(attr(sftrack_no_na, "time_col"), attr(sftrack_bt, "time_col"))

  raccoon_vanilla_sf <- sf::st_as_sf(
    raccoon[!is.na(raccoon[[coords[1]]]) & !is.na(raccoon[[coords[2]]]), ],
    coords = coords,
    crs = crs
  )
  raccoon_vanilla_sf <- sf::st_transform(
    raccoon_vanilla_sf,
    suggest_utm_crs(raccoon_vanilla_sf)
  )
  # When issue #139 is complete, should be switched to expect_silent
  expect_error(as.trackframe(raccoon_vanilla_sf))
}


test_sort <- function(coerce_to) {
  df <- tf_as_xyt(trackframe::tf_mini)
  set.seed(2025)
  df2 <- df[sample(6), ]
  tf_df <- as.trackframe(df2, coerce_to = coerce_to, crs = NA)
  df2_ordered <- df2[order(df2$id, df2$time), ]
  expect_equal(
    as.data.frame(tf_df[, c("id", "time")]),
    df2_ordered[, c("id", "time")],
    check.attributes = FALSE
  )
}


test_incompatible_sf <- function() {
  sf_df <- capture.output(sf::st_read(system.file(
    "shape/nc.shp",
    package = "sf"
  )))
  expect_error(
    as.trackframe(sf_df)
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
  lapply(c("base", "data.table", "tibble"), function(from) {
    test_as_trackframe_data_frame(from = from, coerce_to = coerce_to)
  })
  test_as_trackframe(coerce_to)
  test_sort(coerce_to)
})

test_incompatible_sf()
