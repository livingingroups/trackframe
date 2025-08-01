library(tinytest)

# backtransform
library(travelpaths)
library(checkmate)
library(trackframe)

#move2
set.seed(2025)
move2 <- sim_travel_path(5, format = "move2")
tf <- as.trackframe(data = move2)
attr(tf, "transformation_info")
expect_equal(tf_backtransform(tf), move2)

#sftrack
set.seed(2025)
sftrack <- sim_travel_path(5, format = "sftrack")
tf <- as.trackframe(data = sftrack)
sftrack_b <- tf_backtransform(tf)
sftrack_b$id <- unlist(sftrack_b$sft_group)
expect_equal(sftrack_b, sftrack)

###
library(sftrack)
# library(trackframe)
data("raccoon", package = "sftrack")
raccoon$month <- as.POSIXlt(raccoon$timestamp)$mon + 1
raccoon$time <- as.POSIXct(raccoon$timestamp, tz = "EST")
coords <- c("longitude","latitude")
group <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon+1)
time <- "time"
error <- "fix"
crs <- 4326
# create a sftrack object
my_sftrack <- as_sftrack(data = raccoon,
                         coords = coords,
                         group = group,
                         time = time,
                         error = error,
                         crs = crs)

my_sftrack <- my_sftrack[c(order(my_sftrack$animal_id, my_sftrack$time)), ]

sftrack_tf <- as.trackframe(my_sftrack)

sftrack_tf_b <- tf_backtransform(tf = sftrack_tf)

expect_equal(NROW(sftrack_tf_b), NROW(my_sftrack))
sftrack_tf_b$id <- NULL
expect_equal(colnames(sftrack_tf_b), colnames(my_sftrack))
expect_equal(sftrack_tf_b, my_sftrack)

###

df <- sim_travel_path(5, format = "data.frame")
tf <- as.trackframe(data = df, crs_input = 4326)
attr(tf, "transformation_info")
expect_equal(tf_backtransform(tf), df)

dt <- data.table::as.data.table(sim_travel_path(5, format = "data.frame"))
as.trackframe(data = dt, crs_input = 4326)
tf <- as.trackframe(data = dt, crs_input = 4326, coerce_to = "data.table")
expect_equal(tf_backtransform(tf), dt)

tib <- tibble::as_tibble(sim_travel_path(5, format = "data.frame"))
as.trackframe(data = tib, crs_input = 4326)
tf <- as.trackframe(data = tib, crs_input = 4326, coerce_to = "tibble")
expect_equal(tf_backtransform(tf), tib)
