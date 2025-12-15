
df <- df_mini
names(df) <- c('oclock', 'up', 'right', 'who')
tf <- as.trackframe(
  df,
  time_col = 'oclock',
  northing_col = 'up',
  easting_col = 'right',
  id_col = 'who',
  crs = NA
)

# Accessors
expect_equal(easting(tf), df_mini$easting)
expect_equal(northing(tf), df_mini$northing)
expect_equal(id(tf), df_mini$id)
expect_equal(time(tf), df_mini$time)

# Setters Happy Path
new_values <- c(100, 300, 500, 200, 400)

local({
  easting(tf) <- new_values
  expect_equal(easting(tf), new_values)
  expect_equal(tf$right, new_values)
})

local({
  northing(tf) <- new_values
  expect_equal(northing(tf), new_values)
  expect_equal(tf$up, new_values)
})


local({
  time(tf) <- new_values
  expect_equal(time(tf), new_values)
  expect_equal(tf$oclock, new_values)
})

local({
  id(tf) <- new_values
  expect_equal(id(tf), new_values)
  expect_equal(tf$who, new_values)
})

# Setters Warnings and Errors
local({
  easting_old <- easting(tf)
  expect_error(easting(tf) <- c(new_values, 600))
  expect_error(easting(tf) <- new_values[1:4])
  expect_equal(easting_old, easting(tf))
})
