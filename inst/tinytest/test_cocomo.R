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

# FIXME: copied from cocomo
###
# this is a bit fragile to changes in cocomo package
# ideally cocomo package should export its test data
# source(system.file(
#   "tinytest/helper-group_heading_test_data.R",
#   package = "cocomo")
# )
###
set.seed(200)

# Helper functions

#' Generates individual variaton around desired centroid
#'
#' This is analogous to random effects model where  you can
#' set between individual variation and within individual variation separately.
scatter_group <- function(
    centroid_coords,
    N,
    sd_between_indv = 1,
    sd_within_indv = .2
) {
  n_times <- length(centroid_coords)
  indv_base <- rnorm(N - 1, sd = sd_between_indv)
  within_indv_variation <- rnorm((N - 1) * n_times, sd = sd_within_indv)
  group_coords <- matrix(
    c(
      # rows 1:N-1
      rep(centroid_coords, N - 1) +
        rep(indv_base, each = n_times) +
        within_indv_variation,
      # row N, to be filled in the next line
      rep(NA, n_times)
    ),
    N,
    n_times,
    byrow = TRUE
  )
  
  # balance out so that the indv_base is exactly the average
  group_coords[N, ] <- centroid_coords * N - colSums(group_coords, na.rm = TRUE)
  
  group_coords
}

# Configure base params/matrices ----

N <- 12
n_times <- 31

# Group follows same path as indvidiual does in heading test data

# nolint start: indentation_linter
x_base <- c(
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
  10,  10,  10,  10,   9,   8,   7,   6,   5,   4,
  4,   5,   6,   6,   6,   6,   6,   7,   8,   9,
  10
)
y_base <- c(
  0,   1,   2,   1,   2,   3,   3,   3,   3,   2,
  1,   2,   3,   4,   5,   6,   7,   8,   8,   8,
  8,   7,   6,   7,   8,   9,  10,  10,  10,  10,
  10
)
# nolint end

# Build group movement matrices ----
# Add noise around the centroids
xs <- scatter_group(x_base, N)
ys <- scatter_group(y_base, N)

# Build matrix to simulate missing data -----

# This sets up a matrix that goes from being all 0s to all NAs
# at a consistent rate over time.
ind_count <- rep(1:N, length.out = n_times)
filter_mat <- matrix(
  do.call(c, lapply(ind_count, function(x) c(rep(0, x), rep(NA, N - x)))),
  N,
  n_times
)
###


test_cocomo <- function(coerce_to = 'base') {
  # this is a bit fragile to changes in cocomo package
  # ideally cocomo package should export its test data
  # source(system.file(
  #   "tinytest/helper-group_heading_test_data.R",
  #   package = "cocomo")
  # )
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
  # test_cocomo()# coerce_to)
})