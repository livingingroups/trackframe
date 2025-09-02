library(tinytest)
library(trackframe)

"[.data.frame" <- function(x, i, j, drop = FALSE, ...)  {
  base::`[.data.frame`(x, i, j, drop = drop)
}

test_sort <- function(coerce_to) {
  df <- tf_as_xyt(trackframe::tf_mini)
  set.seed(2025)
  df2 <- df[sample(6), ]
  tf_df <- as.trackframe(df2, coerce_to = coerce_to)
  df2_ordered <- df2[order(df2$id, df2$time), ]
  expect_equal(
    as.data.frame(tf_df[, c("id", "time")]),
    df2_ordered[, c("id", "time")], check.attributes = FALSE
  )
}


# Run all tests
# coerce_to = "base"
# coerce_to = "data.table"
# coerce_to = "tibble"
# coerce_to = NA
lapply(c("base", "data.table", "tibble", NA), function(coerce_to) {
  if (is.na(coerce_to)) coerce_to <- NULL
  test_sort(coerce_to)
})
