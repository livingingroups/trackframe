library(tinytest)
library(sftrack)
library(trackframe)


id_list <- make_c_grouping(list(id = rep(c("a", "a", "", "", "a_b"),2),
                     id_2 = rep(c("b", "", "b", "", "a/c"), 2)), active_group = c("id", "id_2"))

expected_outcome <- rep(c("a<;>b", "a<;>", "<;>b", "<;>", "a_b<;>a/c"), 2)
expect_equal(trackframe:::make_unique_id(id_list), expected_outcome, check.attributes = FALSE)
expect_equal(trackframe:::backtransform_id(expected_outcome, group_names = c("id", "id_2")), id_list)
             

test_multiple_ids <- function(coerce_to) {
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id_1 = "A",
    id_2 = c(1,1,2,2,2)
  )
  my_sftrack <- as_sftrack(data = df,
                           coords = df[, c("easting_col", "northing_col")],
                           group = list(id = df$id_1, month = df$id_2),
                           time = df$time_col,
                           # error = error,
                           crs = 32610)
  
  my_sftrack_ids <- trackframe:::make_unique_id(my_sftrack[[attr(my_sftrack, "group_col")]])
  expect_equal(my_sftrack_ids,
               paste(df$id_1, df$id_2, sep = "<;>"), check.attributes = FALSE)
  expect_equal(attr(my_sftrack_ids, "group_names"),
               c("id", "month"))
  
  expect_equal(my_sftrack$sft_group,
               trackframe:::backtransform_id(
                 trackframe:::make_unique_id(my_sftrack[[attr(my_sftrack, "group_col")]]),
                 group_names = attr(my_sftrack$sft_group, "active_group")
                 ))
  
  tf <- as.trackframe(data = my_sftrack, coerce_to = coerce_to)
  my_sftrack2 <- tf_backtransform(tf)
  expect_equal(my_sftrack$sft_group, my_sftrack2$sft_group)
  expect_equal(my_sftrack, my_sftrack2)
  
  df <- data.frame(
    time_col = as.POSIXct(Sys.time() + 1:5),
    easting_col = runif(5, 0, 10),
    northing_col = runif(5, 0, 10),
    id_1 = "A",
    id_2 = c(1,1,2,2,2),
    id_3 = c("x", "x", "y", "y", "y")
  )
  
  my_sftrack <- as_sftrack(data = df,
                           coords = df[, c("easting_col", "northing_col")],
                           group = list(id = df$id_1, month = df$id_2, sex = df$id_3),
                           time = df$time_col,
                           # error = error,
                           crs = 32610)
  
  expect_equal(my_sftrack$sft_group,
               trackframe:::backtransform_id(
                 trackframe:::make_unique_id(my_sftrack[[attr(my_sftrack, "group_col")]]),
                 group_names = attr(my_sftrack$sft_group, "active_group")
               ))
  
  tf <- as.trackframe(data = my_sftrack, coerce_to = coerce_to)
  my_sftrack2 <- tf_backtransform(tf)
  expect_equal(my_sftrack$sft_group, my_sftrack2$sft_group)
  expect_equal(my_sftrack, my_sftrack2)
} 


# Run all tests
# coerce_to = "base"
# coerce_to = "data.table"
# coerce_to = "tibble"
# coerce_to = NA
lapply(c('base', 'data.table', 'tibble', NA), function(coerce_to) {
  if (is.na(coerce_to)) coerce_to <- NULL
  test_multiple_ids(coerce_to)
})
