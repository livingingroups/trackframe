data <- df_mini[c(4, 1, 3, 5, 2), ]

data_tf <- as.trackframe(data, crs = NA)
data_tf_sorted <- sort(data_tf)
expect_equal(df_mini$time, data_tf_sorted$time)

# with multiple ids
set.seed(2025)
tf_mini2 <- tf_mini[sample(1:11), ]
tf_mini_sorted <- sort(tf_mini2)
expect_equal(tf_mini$time, tf_mini_sorted$time)

# test sorting + undo sorting in backtransform
data_tf <- as.trackframe(data, crs = NA)
data_b <- tf_backtransform(data_tf)
expect_equal(data_b, data)

# test with sort = FALSE
data_tf <- as.trackframe(data, crs = NA, sort = FALSE)
data_b <- tf_backtransform(data_tf)
expect_equal(data_b, data)

# test with random sample rows
# data.frame
set.seed(2025)
idr <- sample(seq_len(NROW(paths_data_frame)))
paths_data_frame_r <- paths_data_frame[idr, ]
paths_tf <- as.trackframe(paths_data_frame_r, crs = NA)

expect_false(all(paths_data_frame_r[, c(1, 4)] == paths_tf[, c(1, 4)]))
expect_equal(
  paths_data_frame_r[order(idr), c(1, 4)],
  paths_tf[, c(1, 4)],
  check.attributes = FALSE
)

paths_tf_b <- tf_backtransform(paths_tf)
expect_equal(paths_tf_b, paths_data_frame_r)

# delete rows
paths_tf_d <- paths_tf[-c(2, 100, 1000:1010, 2000:2100), ]
expect_warning(tf_backtransform(paths_tf_d))

# change row order
set.seed(2026)
idr2 <- sample(seq_len(NROW(paths_data_frame)))
paths_tf_r <- paths_tf[idr2, ]
expect_silent(tf_backtransform(paths_tf_r))


# sftrack
set.seed(2025)
idr <- sample(seq_len(NROW(paths_sftrack)))
paths_sftrack_r <- paths_sftrack[idr, ]
paths_tf <- as.trackframe(paths_sftrack_r)

expect_false(all(
  as.data.frame(paths_sftrack_r)[, c(1, 4)] == paths_tf[, c(1, 4)]
))
expect_equal(
  as.data.frame(paths_sftrack_r)[order(idr), c(1, 4)],
  paths_tf[, c(1, 4)],
  check.attributes = FALSE
)

paths_tf_b <- tf_backtransform(paths_tf)
# NOTE: possible st_crs()$wkt mismatch if data is not created with same version of GDAL
expect_equal(sf::st_crs(paths_tf_b)$input, sf::st_crs(paths_sftrack_r)$input)
expect_equal(paths_tf_b, paths_sftrack_r, check.attributes = FALSE)

# delete rows
paths_tf_d <- paths_tf[-c(2, 100, 1000:1010, 2000:2100), ]
expect_warning(tf_backtransform(paths_tf_d))

# change row order
set.seed(2026)
idr2 <- sample(seq_len(NROW(paths_sftrack)))
paths_tf_r <- paths_tf[idr2, ]
expect_silent(tf_backtransform(paths_tf_r))

# move2
set.seed(2025)
idr <- sample(seq_len(NROW(paths_move2)))
paths_move2_r <- paths_move2[idr, ]
paths_tf <- as.trackframe(paths_move2_r)

expect_false(all(
  as.data.frame(paths_move2_r)[, c(1, 2)] == paths_tf[, c(1, 2)]
))
expect_equal(
  as.data.frame(paths_move2_r)[order(idr), c(1, 2)],
  paths_tf[, c(1, 2)],
  check.attributes = FALSE
)

paths_tf_b <- tf_backtransform(paths_tf)
expect_equal(paths_tf_b, paths_move2_r, check.attributes = FALSE)
