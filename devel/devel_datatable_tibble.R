set.seed(2025)
df <- data.frame(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep('a', 5), rep('b', 5))
)

df
tf_df <- as.trackframe(df, coerce_to = "base")
class(tf_df)

#data.table
set.seed(2025)
library(data.table)
dt <- data.table(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep('a', 5), rep('b', 5))
)
class(dt)

tf_dt <- as.trackframe(dt, coerce_to = "data.table")
class(tf_dt)


#tibble

library(tibble)
set.seed(2025)
tib <- tibble(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep('a', 5), rep('b', 5))
)
class(tib)

tf_tib <- as.trackframe(tib, coerce_to = "tibble")
class(tf_tib)

library(tinytest)
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


# coerce_to = "base"
class(dt)
dt_df <- as.data.frame(dt)
class(dt_df)

class(tib)
tib_df <- as.data.frame(tib) 
class(tib_df)

# coerce_to = "data.table"
class(df)
df_dt <- as.data.table(df)
class(df_dt)

class(tib)
tib_dt <- as.data.table(tib) 
class(tib_dt)

# coerce_to = "tibble"
class(df)
df_tib <- as_tibble(df)
class(df_tib)

class(dt)
dt_tib <- as_tibble(dt) 
class(dt_tib)


#sort
data <- tf_dt[sample(1:10), ]
data <- data[order(id(data), time(data)), ] 
data

data <- tf_tib[sample(1:10), ]
data <- data[order(id(data), time(data)), ] 
data



# backtransform
library(travelpaths)
library(checkmate)
library(trackframe)
set.seed(2025)
move2 <- sim_travel_path(5, format = "move2")
tf <- as.trackframe(data = move2)
attr(tf, "transformation_info")
class(tf_backtransform(tf))
class(move2)
expect_equal(tf_backtransform(tf), move2)

set.seed(2025)
sftrack <- sim_travel_path(5, format = "sftrack")
tf <- as.trackframe(data = sftrack)
attr(tf, "transformation_info")
sftrack_b <- tf_backtransform(tf)
sftrack_b$id <- unlist(sftrack_b$sft_group)
expect_equal(sftrack_b, sftrack)

###
library(sftrack)
library(trackframe)
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

sftrack_tf <- as.trackframe(my_sftrack)
unlist(sftrack_tf$sft_group)

tf_backtransform(sftrack_tf)
###

df <- sim_travel_path(5, format = "data.frame")
tf <- as.trackframe(data = df, crs_input = 4326)
attr(tf, "transformation_info")
expect_equal(tf_backtransform(tf), df)

dt <- as.data.table(sim_travel_path(5, format = "data.frame"))
as.trackframe(data = dt, crs_input = 4326)
tf <- as.trackframe(data = dt, crs_input = 4326, coerce_to = "data.table")
tf
attr(tf, "transformation_info")
expect_equal(tf_backtransform(tf), dt)

tib <- as_tibble(sim_travel_path(5, format = "data.frame"))
as.trackframe(data = tib, crs_input = 4326)
tf <- as.trackframe(data = tib, crs_input = 4326, coerce_to = "tibble")
tf
attributes(tf)
attr(tf, "transformation_info")
expect_equal(tf_backtransform(tf), tib)
