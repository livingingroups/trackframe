library(tinytest)
library(trackframe)

source("helpers.R")

reorder_rows <- function(df) df[sample(seq_len(nrow(df))), ]

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
    info = "Expect all classes come from either input or known tf (sub)classes"
  )
}

test_as_trackframe_data_frame <- function(
  from = "base",
  coerce_to = "base",
  reorder = FALSE,
  sort = TRUE
) {
  if (is.na(coerce_to)) {
    coerce_to <- NULL
  }
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
  df_sorted <- df
  df <- if (reorder) reorder_rows(df) else df
  df_ref <- if (sort) df_sorted else df
  tf <- trackframe(
    data = df,
    time_col = "time_col",
    easting_col = "easting_col",
    northing_col = "northing_col",
    id_col = "id",
    coerce_to = coerce_to,
    crs = NA,
    sort = sort
  )
  tf2 <- trackframe(data = df, coerce_to = coerce_to, crs = NA, sort = sort)
  expect_equal(tf, tf2)
  tf3 <- as.trackframe(
    data = df,
    time_col = "time_col",
    easting_col = "easting_col",
    northing_col = "northing_col",
    id_col = "id",
    coerce_to = coerce_to,
    crs = NA,
    sort = sort
  )
  expect_equal(tf, tf3)
  tf4 <- as.trackframe(data = df, coerce_to = coerce_to, crs = NA, sort = sort)
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
    crs = NA,
    sort = sort
  ))
  expect_equal(easting(tf), df_ref$easting_col)
  expect_equal(northing(tf), df_ref$northing_col)
  expect_equal(id(tf), df_ref$id)
  expect_equal(time(tf), df_ref$time_col)
  expect_inherits(time(tf), "POSIXct")
}

test_as_trackframe <- function(
  coerce_to = "base",
  reorder = FALSE,
  sort = TRUE
) {
  if (is.na(coerce_to)) {
    coerce_to <- NULL
  }
  mat <- as.matrix(data.frame(
    time_col = 1:5,
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  ))
  mat_sorted <- mat
  mat <- if (reorder) reorder_rows(mat) else mat
  mat_ref <- if (sort) mat_sorted else mat

  expect_inherits(mat, "matrix")
  tf <- trackframe(
    mat,
    time_col = "time_col",
    easting_col = "easting_col",
    northing_col = "northing_col",
    id_col = "id",
    coerce_to = coerce_to,
    crs = NA,
    sort = sort
  )
  expect_inherits(tf, "trackframe")
  expect_equal(dim(mat), dim(tf))
  expect_equal(easting(tf), mat_ref[, "easting_col"])
  expect_equal(northing(tf), mat_ref[, "northing_col"])
  expect_equal(id(tf), mat_ref[, "id"])
  expect_equal(time(tf), mat_ref[, "time_col"])

  #move2
  library(move2)
  move2_ex <- mt_read(mt_example()) |>
    sf::st_transform(3857)
  move2_ex <- move2_ex[!sf::st_is_empty(move2_ex), ]

  move2_ex <- if (reorder) reorder_rows(move2_ex) else move2_ex
  move2_ex_ref <- if (sort) {
    move2_ex[order(move2_ex$`individual-local-identifier`, move2_ex$timestamp), ]
  } else {
    move2_ex
  }

  move2_ex_tf <- as.trackframe(move2_ex, coerce_to = coerce_to, sort = sort)

  expect_inherits(move2_ex_tf, "trackframe")
  expect_equal(NROW(move2_ex), NROW(move2_ex_tf))

  x_y <- sf::st_coordinates(move2_ex_ref[[attr(
    move2_ex_ref,
    "sf_column"
  )]])
  expect_equal(easting(move2_ex_tf), x_y[, 1])
  expect_equal(northing(move2_ex_tf), x_y[, 2])
  expect_equal(
    id(move2_ex_tf),
    move2_ex_ref[[attr(move2_ex, "track_id_column")]]
  )
  expect_equal(
    time(move2_ex_tf),
    move2_ex_ref[[attr(move2_ex, "time_column")]]
  )
  #backtransformation
  move2_ex_bt <- tf_backtransform(move2_ex_tf[
    !is.na(northing(move2_ex_tf)),
  ])
  expect_equal(dim(move2_ex), dim(move2_ex_bt))
  !expect_equal(
    sf::st_coordinates(move2_ex),
    sf::st_coordinates(move2_ex_bt)
  )
  expect_equal(
    attr(move2_ex, "track_id_column"),
    attr(move2_ex_bt, "track_id_column")
  )
  expect_equal(
    attr(move2_ex, "time_column"),
    attr(move2_ex_bt, "time_column")
  )

  expect_silent(as.trackframe(
    move2_ex,
    time_col = tf_options("time_col"),
    id_col = c("id", "id2", "individual-local-identifier"),
    coerce_to = coerce_to,
    sort = sort
  ))

  #sftrack
  library(sftrack)
  # Make tracks from raw data
  data("raccoon", package = "sftrack")
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
  sft <- as_sftrack(
    data = raccoon,
    coords = coords,
    group = group,
    time = time,
    error = error,
    crs = crs
  ) |>
    sf::st_transform(suggest_utm_zone_crs(raccoon$latitude, raccoon$longitude))

  sft <- if (reorder) reorder_rows(sft) else sft
  sft_ref <- if (sort) sft[order(sft$animal_id, sft$timestamp), ] else sft

  sft_tf <- as.trackframe(sft, coerce_to = coerce_to, sort = sort)
  expect_inherits(sft_tf, "trackframe")
  expect_equal(NROW(sft), NROW(sft_tf))

  x_y <- sf::st_coordinates(sft_ref[[attr(
    sft_ref,
    "sf_column"
  )]])
  x_y[is.nan(x_y)] <- NA
  expect_equal(easting(sft_tf), x_y[, 1])
  expect_equal(northing(sft_tf), x_y[, 2])
  expected_id <- do.call(
    rbind,
    sft_ref[[attr(
      sft,
      "group_col"
    )]]
  )
  expected_id <- paste(expected_id[, 1], expected_id[, 2], sep = "<;>")

  expect_equal(
    id(sft_tf),
    expected_id,
    check.attributes = FALSE
  )
  expect_equal(
    id(sft_tf),
    trackframe:::make_unique_id(sft_ref[[attr(
      sft_ref,
      "group_col"
    )]]),
    check.attributes = FALSE
  )
  expect_equal(
    time(sft_tf),
    sft_ref[[attr(sft, "time_col")]]
  )

  expect_silent(suppressWarnings(as.trackframe(
    sft,
    time_col = c("t", "timestamp", "time3"),
    id_col = tf_options("id_col"),
    coerce_to = coerce_to,
    sort = sort
  )))
  expect_warning(as.trackframe(
    sft,
    time_col = c("t", "timestamp", "time3"),
    id_col = tf_options("id_col"),
    coerce_to = coerce_to,
    sort = sort
  ))
  expect_silent(as.trackframe(
    sft,
    time_col = c("t", "time", "time3"),
    id_col = NULL,
    coerce_to = coerce_to,
    sort = sort
  ))

  #backtransformation
  sftrack_bt <- tf_backtransform(sft_tf)
  expect_equal(NROW(sft), NROW(sftrack_bt))

  expect_equal(
    sf::st_coordinates(sft),
    sf::st_coordinates(sftrack_bt)
  )
  expect_equal(attr(sft, "group_col"), attr(sftrack_bt, "group_col"))
  expect_equal(attr(sft, "time_col"), attr(sftrack_bt, "time_col"))

  # vsf : vanilla sf
  vsf <- sf::st_as_sf(
    raccoon[!is.na(raccoon[[coords[1]]]) & !is.na(raccoon[[coords[2]]]), ],
    coords = coords,
    crs = crs
  )
  vsf <- sf::st_transform(
    vsf,
    suggest_utm_zone_crs(vsf)
  )

  vsf <- if (reorder) reorder_rows(vsf) else vsf
  vsf_ref <- if (sort) vsf[order(vsf$animal_id, vsf$timestamp), ] else vsf

  expect_silent(as.trackframe(
    vsf,
    time_col = "timestamp",
    id_col = "animal_id"
  ))
  vsf_tf <- as.trackframe(
    vsf,
    time_col = "timestamp",
    id_col = "animal_id"
  )
  expect_equal(as.trackframe(vsf), vsf_tf)
  expect_inherits(vsf_tf, "trackframe")
  expect_equal(NROW(vsf_tf), NROW(vsf))
  expect_equal(
    cbind(
      "X" = easting(vsf_tf),
      "Y" = northing(vsf_tf)
    ),
    sf::st_coordinates(vsf_ref),
    tol = 1e-4
  )
  expect_equal(
    colnames(vsf_tf),
    c(
      "animal_id",
      "timestamp",
      "height",
      "hdop",
      "vdop",
      "fix",
      "month",
      "time",
      "geometry",
      "easting",
      "northing"
    )
  )
}


test_sort <- function(coerce_to) {
  if (is.na(coerce_to)) {
    coerce_to <- NULL
  }
  df <- tf_as_xyt(trackframe::tf_mini)
  set.seed(2025)
  df2 <- reorder_rows(df)
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

test_as_trackframe("base", TRUE, FALSE)


# Run all tests
lapply(c("base", "data.table", "tibble", NA), function(coerce_to) {
  lapply(c(TRUE, FALSE), \(reorder) {
    lapply(c(TRUE, FALSE), \(sort) {
      lapply(c("base", "data.table", "tibble"), function(from) {
        test_as_trackframe_data_frame(from, coerce_to, reorder, sort)
      })
      test_as_trackframe(coerce_to, reorder, sort)
    })
  })
  test_sort(coerce_to)
})

test_incompatible_sf()
