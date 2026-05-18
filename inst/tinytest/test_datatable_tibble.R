"[.data.frame" <- trackframe:::`[.data.frame`

set.seed(2025)
df <- data.frame(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep("a", 5), rep("b", 5))
)
tf_df <- as.trackframe(df, coerce_to = "base", crs = NA)


#data.table
set.seed(2025)
suppressMessages(library(data.table))
dt <- data.table(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep("a", 5), rep("b", 5))
)
tf_dt <- as.trackframe(dt, coerce_to = "data.table", crs = NA)


#tibble
suppressMessages(library(tibble))
set.seed(2025)
tib <- tibble(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep("a", 5), rep("b", 5))
)
tf_tib <- as.trackframe(tib, coerce_to = "tibble", crs = NA)

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


# Check all combinations

input_data <- list(
  base = df,
  data.table = dt,
  tibble = tib
)

class_ids <- list(
  base = character(0),
  data.table = "data.table",
  tibble = c("tbl_df", "tbl")
)

scenarios <- expand.grid(
  from = names(input_data),
  to = c(names(input_data), NA),
  stringsAsFactors = FALSE
)

for (row_idx in seq_len(nrow(scenarios))) {
  from <- unlist(scenarios[row_idx, "from"])
  to <- unlist(scenarios[row_idx, "to"])
  tf <- as.trackframe(
    input_data[[from]],
    coerce_to = if (is.na(to)) NULL else to,
    crs = NA
  )

  expect_inherits(tf, "data.frame")
  expect_inherits(tf, "trackframe")
  for (class_to_check in names(class_ids)) {
    if (class_to_check == if (is.na(to)) from else to) {
      for (class_id in class_ids[[class_to_check]]) {
        expect_inherits(tf, class_id)
      }
    } else {
      for (class_id in class_ids[[class_to_check]]) {
        expect_false(inherits(tf, class_id))
      }
    }
  }

  for (col in c("x", "y", "t", "animal_id")) {
    expect_equal(tf[[col]], tf_df[[col]])
  }
}
