source("helpers.R")
data("tf_mini")

rev_by_row <- function(df) {
  df[rev(seq_len(nrow(df))), ]
}

tf_options("crs", NA)
tf_options("time_col", "t")

tf_micro <- as.trackframe(
  data.frame(
    x = 1:6,
    y = 1:6,
    t = 1:6,
    id = c("a", "a", "b", "b", "c", "c")
  )
)

split_micro <- split_by_id(tf_micro)

expect_equivalent(
  split_micro,
  list(
    a = as.trackframe(
      data.frame(
        x = 1:2,
        y = 1:2,
        t = 1:2,
        id = "a"
      )
    ),
    b = as.trackframe(
      data.frame(
        x = 3:4,
        y = 3:4,
        t = 3:4,
        id = "b"
      )
    ),
    c = as.trackframe(
      data.frame(
        x = 5:6,
        y = 5:6,
        t = 5:6,
        id = "c"
      )
    )
  )
)

lapply(split_micro, \(tf) {
  expect_most_elements_equal(
    attributes(tf),
    attributes(tf_micro),
    omit = "row.names"
  )
})

split_micro_rev <- split_by_id(rev_by_row(tf_micro))

expect_equivalent(
  split_micro_rev,
  # base::split() sorts levels themselves, but not within levels
  lapply(split_micro, \(tf) {
    rev_by_row(tf)
  })
)
