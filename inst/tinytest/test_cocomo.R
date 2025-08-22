library(tinytest)
library(trackframe)

set.seed(2025)

#cocomo
old_test_cocomo <- function(coerce_to = 'base') {
  # sim_travel_paths is producing some bad stuff
  tf <- as.trackframe(sim_travel_paths(3, 3))
  cocomo <-  tf_as_cocomo(tf) 
  tf2 <- cocomo_as_tf(cocomo$x, cocomo$y, cocomo$t, cocomo$ids) #, coerce_to = coerce_to)
  cn <- c("time", "easting", "northing", "id")
  expect_equal(tf[, cn], tf2[, cn])
}

test_cocomo <- function(coerce_to = 'base') {
  # this is a bit fragile to changes in cocomo package
  # ideally cocomo package should export its test data
  source(system.file(
    "tinytest/helper-group_heading_test_data.R",
    package = "cocomo")
  )
  id_code <- vapply(seq_len(dim(xs)[1]), \(x) paste0(sample(letters, 8), collapse = ''), character(1))
  ids <- data.frame(id_code = id_code)
  t = seq_len(dim(xs)[2])
  tf1 <- cocomo_as_tf(xs, ys, t = t, ids = ids)
  cocomo2 <- tf_as_cocomo(tf1)
  tf2 <- cocomo_as_tf(cocomo2$xs, cocomo2$ys, cocomo2$t, cocomo2$ids) #, coerce_to = coerce_to)
  expect_equivalent(
    cocomo2,
    list(
      xs = xs[order(id_code),],
      ys = ys[order(id_code),],
      t = t,
      ids = data.frame(id_code = sort(id_code))
    )
  )
  expect_equal(dimnames(cocomo2$xs)[[1]], sort(id_code))
  expect_equal(dimnames(cocomo2$ys)[[1]], sort(id_code))

  expect_equivalent(tf1, tf2)

  expect_error(cocomo_as_tf(xs, ys, ids = ids))
  expect_error(cocomo_as_tf(xs, t, ids = ids))
  expect_error(cocomo_as_tf(xs, t = t, ids = ids))
  
  # tf3 <- cocomo_as_tf(xs, ys, t = t)
  # expect_equal(tf1, tf3)
}

lapply(c("base", "data.table", "tibble", NA), function(coerce_to) {
  if (is.na(coerce_to)) coerce_to <- NULL
  #old_test_cocomo()# coerce_to)
  test_cocomo()# coerce_to)
})