check_trackframe <- trackframe:::check_trackframe

# Basic functionality
expect_true(check_trackframe(tf_mini))
expect_false(isTRUE(check_trackframe(df_mini)))
expect_false(isTRUE(check_trackframe(8)))


set.seed(2025)

#  test unsorted flag with no id column
mini_reorder <- sample(nrow(tf_mini))
expect_true(check_trackframe(tf_mini[mini_reorder, ]))
expect_false(isTRUE(check_trackframe(
  tf_mini[mini_reorder, ],
  unsorted.ok = FALSE
)))

#  test unsorted flag with multiple tracks
paths_reorder <- sample(nrow(paths_trackframe))
expect_true(check_trackframe(paths_trackframe[paths_reorder, ]))
expect_false(isTRUE(check_trackframe(
  paths_trackframe[paths_reorder, ],
  unsorted.ok = FALSE
)))
