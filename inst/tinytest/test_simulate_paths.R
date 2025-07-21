# library(tinytest)
# Test Suite for change_point_test Function
library(trackframe)

# cpttestdata

test_simulate_path <- function() {
  data_matrix <- sim_travel_path(100, format = "matrix")
  expect_inherits(data_matrix, "matrix")
  expect_equal(NROW(data_matrix), 100)
  expect_equal(NCOL(data_matrix), 4)
  
  
  data_df <- sim_travel_path(100, format = "data.frame")
  expect_inherits(data_df, "data.frame")
  expect_equal(NROW(data_df), 100)
  expect_equal(NCOL(data_df), 4)
  
  
  tf <- sim_travel_path(100, format = "track_frame")
  expect_true(all(c("time", "easting", "northing") %in% colnames(tf)))
  expect_inherits(tf, "track_frame")
  expect_equal(NROW(tf), 100)
  expect_equal(NCOL(tf), 6)
  
  #TODO sftrack + move2
}


test_simulate_paths <- function() {
  ntracks <- 3
  sizes <- c(2, 4, 5)
  multi_track <- sim_travel_paths(ntracks, sizes)
  expect_inherits(multi_track, "track_frame")
  expect_equal(sum(sizes), NROW(multi_track))
  expect_length(unique(id(multi_track)), ntracks)
  
  track2 <- select_id(multi_track, "track_2")
  expect_inherits(track2, "track_frame")
  expect_equal(NROW(track2), sizes[2])
  expect_length(unique(id(track2)), 1)
}

# Run all tests
test_simulate_path()
test_simulate_paths()

cat("All tests for simulate paths completed successfully!\n")
