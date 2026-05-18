"[.data.frame" <- trackframe:::`[.data.frame`

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


# Run all tests
lapply(c("base", "data.table", "tibble", NA), function(coerce_to) {
  if (is.na(coerce_to)) {
    coerce_to <- NULL
  }
  test_sort(coerce_to)
})


# test set_facet_ncol
set_facet_ncol <- trackframe:::set_facet_ncol
expect_equal(set_facet_ncol(1), 1)
expect_equal(set_facet_ncol(2), 2)
expect_equal(set_facet_ncol(3), 3)
expect_equal(set_facet_ncol(4), 2)
expect_equal(set_facet_ncol(5), 2)
expect_equal(set_facet_ncol(6), 3)
expect_equal(set_facet_ncol(7), 2)
expect_equal(set_facet_ncol(8), 4)
expect_equal(set_facet_ncol(9), 3)
expect_equal(set_facet_ncol(10), 2)
expect_equal(set_facet_ncol(11), 2)
expect_equal(set_facet_ncol(12), 4)
