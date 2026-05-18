"[.data.frame" <- trackframe:::`[.data.frame`

test_guess_all_cols <- function() {
  # duplicated guesses
  # with same values
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    animal_id = "A"
  )
  expect_silent(trackframe(data = data, crs = NA))
  guesses <- guess_all_cols(col_names = colnames(data))
  expect_equal(guesses$time_col, c("time", "time_col"))
  expect_equal(guesses$easting_col, c("easting_col"))
  expect_equal(guesses$northing_col, c("northing_col"))
  expect_equal(guesses$id_col, "animal_id")
  expect_silent(trackframe:::warn_if_guess_ambiguous(data, guesses))

  # with different values
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 2:6),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    animal_id = "A"
  )
  guesses <- guess_all_cols(col_names = colnames(data))
  expect_warning(trackframe:::warn_if_guess_ambiguous(data, guesses))

  # easting
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = 1001:1005,
    easting = 1001:1005,
    northing_col = runif(5, 0, 10),
    animal_id = "A"
  )
  expect_silent(trackframe(data = data, crs = NA))
  guesses <- guess_all_cols(col_names = colnames(data))
  expect_equal(guesses$time_col, c("time_col"))
  expect_equal(guesses$easting_col, c("easting", "easting_col"))
  expect_equal(guesses$northing_col, c("northing_col"))
  expect_equal(guesses$id_col, "animal_id")
  expect_silent(trackframe:::warn_if_guess_ambiguous(data, guesses))

  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    easting = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    animal_id = "A"
  )
  guesses <- guess_all_cols(col_names = colnames(data))
  expect_warning(trackframe:::warn_if_guess_ambiguous(data, guesses))

  #northing
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting = runif(5, 0, 10),
    northing_col = 1001:1005,
    northing = 1001:1005,
    animal_id = "A"
  )
  expect_silent(trackframe(data = data, crs = NA))
  guesses <- guess_all_cols(col_names = colnames(data))
  expect_equal(guesses$time_col, c("time_col"))
  expect_equal(guesses$easting_col, c("easting"))
  expect_equal(guesses$northing_col, c("northing", "northing_col"))
  expect_equal(guesses$id_col, "animal_id")
  expect_silent(trackframe:::warn_if_guess_ambiguous(data, guesses))

  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    northing = runif(5, 0, 10),
    animal_id = "A"
  )
  guesses <- guess_all_cols(col_names = colnames(data))
  expect_warning(trackframe:::warn_if_guess_ambiguous(data, guesses))

  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5,
    track_id = 1:5
  )
  expect_silent(trackframe(data = data, crs = NA))
  guesses <- guess_all_cols(col_names = colnames(data))
  expect_equal(guesses$time_col, c("time_col"))
  expect_equal(guesses$easting_col, c("easting_col"))
  expect_equal(guesses$northing_col, c("northing_col"))
  expect_equal(guesses$id_col, c("track_id", "id"))
  expect_silent(trackframe:::warn_if_guess_ambiguous(data, guesses))
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5,
    track_id = 2:6
  )
  guesses <- guess_all_cols(col_names = colnames(data))
  expect_warning(trackframe:::warn_if_guess_ambiguous(data, guesses))

  #col missing
  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    time = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10)
  )
  expect_error(guess_all_cols(col_names = colnames(data)))

  data <- data.frame(
    time = as.POSIXct(Sys.time() + 1:5),
    northing_col = runif(5, 0, 10)
  )
  expect_error(guess_all_cols(col_names = colnames(data)))

  data <- data.frame(
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10)
  )
  expect_error(guess_all_cols(col_names = colnames(data)))

  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col2 = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id = 1:5
  )
  expect_error(as.trackframe(data = data, crs = NA))

  expect_silent(as.trackframe(
    data = data,
    easting_col = "easting_col2",
    crs = NA
  ))

  expect_error(guess_all_cols(col_names = colnames(data)))
  guesses <- guess_all_cols(
    col_names = colnames(data),
    easting_col_candidates = "easting_col2"
  )
  expect_equal(guesses$time_col, c("time_col"))
  expect_equal(guesses$easting_col, c("easting_col2"))
  expect_equal(guesses$northing_col, c("northing_col"))
  expect_equal(guesses$id_col, "id")
  expect_silent(trackframe:::warn_if_guess_ambiguous(data, guesses))

  data <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col2 = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id2 = 1:5
  )
  guesses <- guess_all_cols(
    col_names = colnames(data),
    easting_col_candidates = "easting_col2",
    id_col_candidates = "id2"
  )
  expect_equal(guesses$id_col, "id2")
  expect_silent(trackframe:::warn_if_guess_ambiguous(data, guesses))
}

test_guess_all_cols()
