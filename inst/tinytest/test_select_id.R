data("tf_mini")

expect_equal(
  select_id(tf_mini, id = "track_1"),
  tf_mini[tf_mini$id == "track_1"]
)

# UnSorted
tf_mini_us <- tf_mini[c(10, 2, 1, 8, 3:7, 11, 9), ]

# package trackframe
expect_equal(
  select_id(tf_mini_us, id = "track_1"),
  tf_mini_us[tf_mini_us$id == "track_1"]
)
