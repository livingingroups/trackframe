library(tinytest)

# backtransform
library(checkmate)
library(trackframe)

#move2
set.seed(2025)

# FIXME: move some of this to travelpaths?
library(move2)
m2 <- mt_as_move2(df_mini,
  coords = c("latitude", "longitude"),
  time_column = "time",
  track_id_column = "id",
  crs = 4326
)
tf <- as.trackframe(data = m2)
expect_equal(tf_backtransform(tf), m2)

# FIXME: same as above
#sftrack
set.seed(2025)
sftrack_a <- as_sftrack(df_mini, coords = c("latitude", "longitude"), crs = 4326)
tf <- as.trackframe(data = sftrack_a)
sftrack_b <- tf_backtransform(tf)
expect_equal(sftrack_b, sftrack_a)

###
library(sftrack)
# library(trackframe)
data("raccoon", package = "sftrack")
raccoon$month <- as.POSIXlt(raccoon$timestamp)$mon + 1
raccoon$time <- as.POSIXct(raccoon$timestamp, tz = "EST")
coords <- c("longitude", "latitude")
group <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon + 1)
time <- "time"
error <- "fix"
crs <- 4326
# create a sftrack object
my_sftrack <- as_sftrack(
  data = raccoon,
  coords = coords,
  group = group,
  time = time,
  error = error,
  crs = crs
)

my_sftrack <- my_sftrack[c(order(my_sftrack$animal_id, my_sftrack$time)), ]

sftrack_tf <- as.trackframe(my_sftrack)

sftrack_tf_b <- tf_backtransform(tf = sftrack_tf)

expect_equal(NROW(sftrack_tf_b), NROW(my_sftrack))
expect_equal(colnames(sftrack_tf_b), colnames(my_sftrack))
expect_equal(sftrack_tf_b, my_sftrack)


###

df <- df_mini
tf <- as.trackframe(data = df, crs_input = 4326)
attr(tf, "transformation_info")
expect_equal(tf_backtransform(tf), df)

dt <- data.table::as.data.table(df_mini)
expect_equal(tf_backtransform(as.trackframe(data = dt, crs_input = 4326)), dt)
expect_equal(tf_backtransform(
  as.trackframe(data = dt, crs_input = 4326, coerce_to = NULL)
), dt)
expect_equal(tf_backtransform(
  as.trackframe(data = dt, crs_input = 4326, coerce_to = "data.table")
), dt)

tib <- tibble::as_tibble(df_mini)
expect_equal(tf_backtransform(as.trackframe(data = tib, crs_input = 4326)), tib)
expect_equal(tf_backtransform(
  as.trackframe(data = tib, crs_input = 4326, coerce_to = "tibble")
), tib)
expect_equal(tf_backtransform(
  as.trackframe(data = tib, crs_input = 4326, coerce_to = NULL)
), tib)
