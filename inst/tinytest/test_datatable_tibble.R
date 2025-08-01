library(tinytest)

set.seed(2025)
df <- data.frame(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep('a', 5), rep('b', 5))
)
tf_df <- as.trackframe(df, coerce_to = "base")


#data.table
set.seed(2025)
library(data.table)
dt <- data.table(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep('a', 5), rep('b', 5))
)
tf_dt <- as.trackframe(dt, coerce_to = "data.table")


#tibble
library(tibble)
set.seed(2025)
tib <- tibble(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep('a', 5), rep('b', 5))
)
tf_tib <- as.trackframe(tib, coerce_to = "tibble")

expect_inherits(tf_df, "trackframe")
expect_inherits(tf_df, "data.frame")

expect_inherits(tf_dt, "trackframe")
expect_inherits(tf_dt, "data.table")
expect_inherits(tf_dt, "data.frame")

expect_inherits(tf_tib, "trackframe")
expect_inherits(tf_tib, "tbl_df")
expect_inherits(tf_tib, "tbl")
expect_inherits(tf_tib, "data.frame")


expect_equal(tf_df[["x"]], tf_dt[["x"]])
expect_equal(tf_df[["x"]], tf_tib[["x"]])

expect_equal(tf_df[["y"]], tf_dt[["y"]])
expect_equal(tf_df[["y"]], tf_tib[["y"]])

expect_equal(tf_df[["t"]], tf_dt[["t"]])
expect_equal(tf_df[["t"]], tf_tib[["t"]])

expect_equal(tf_df[["animal_id"]], tf_dt[["animal_id"]])
expect_equal(tf_df[["animal_id"]], tf_tib[["animal_id"]])


