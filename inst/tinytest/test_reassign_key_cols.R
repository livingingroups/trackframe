df <- df_mini
names(df) <- c('when', 'up', 'right', 'who')
tf <- as.trackframe(
  df,
  time_col = 'when',
  easting_col = 'right',
  northing_col = 'up',
  id_col = 'who',
  crs = NA
)
orig_right <- easting(tf)
orig_up <- northing(tf)

# create new columns that we want to be the new coordinates
tf$up_10x <- 10 * tf$up
tf$right_10x <- 10 * tf$right

# up/right and up_10x/right_10x are both part of the trackframe
expect_true(all(c('up', 'up_10x', 'right', 'right_10x') %in% colnames(tf)))

# still same as original
expect_equal(easting(tf), orig_right)
expect_equal(northing(tf), orig_up)
expect_equal(
  tf_colnames(tf),
  c(time = "when", northing = "up", easting = "right", id = "who")
)

# update which columns are used as key columns
expect_silent(
  tf <- as.trackframe(tf, easting = "right_10x", northing = "up_10x", crs = NA)
)

# still all columns are there
expect_true(all(c('up', 'up_10x', 'right', 'right_10x') %in% colnames(tf)))

# upated to 10x
expect_equal(easting(tf), orig_right * 10)
expect_equal(northing(tf), orig_up * 10)
expect_equal(
  tf_colnames(tf),
  c(time = "when", id = "who", northing = "up_10x", easting = "right_10x")
)

# check for warning if coords change, but crs not provided
expect_warning(tf <- as.trackframe(tf, easting = "right"))
