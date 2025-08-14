# library(devtools)
# install_github('livingingroups/cocomo')

library(cocomo)
#test data
padding <- 25
n_times <- 20 + 3 * padding
N <- 2

xs <- matrix(
  c(
    rep(-1, padding),
    c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1),
    rep(0, padding),
    c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
    rep(1, padding),
    rep(0, n_times)
  ),
  N,n_times,byrow=TRUE
)
ys <- matrix(
  rep(0, N*n_times),
  N,n_times,byrow=TRUE
)
timestamps <- as.POSIXct(1:n_times)

ids <- rbind.data.frame(list("id_code" = 'VCVM001', "age" = 10, "sex" = "m"),
                        list("id_code" = 'WRTH', "age" = 5, "sex" = "f"))

###
dim(xs)
N <- NROW(xs)
nt <- NCOL(xs)

ids$id_code

data <- data.frame("time" = timestamps,
           "easting" = as.vector(t(xs)),
           "northing" = as.vector(t(ys)),
           "track_id" = rep(ids$id_code, each = nt))

cols <- setdiff(colnames(ids), "id_code")
add_cols <- do.call("cbind.data.frame", lapply(ids[, cols], function(x) rep(x, each = nt)))
data <- cbind(data, add_cols)
as.trackframe(data, time_col = "time", easting_col = "easting",
               northing_col = "northing", id_col = "track_id")


library(checkmate)
as.trackframe_from_cocomo <- function(xs, ys, timestamps, ids) {
  assert_matrix(xs)
  assert_matrix(ys)
  assert_posixct(timestamps)
  assert_data_frame(ids)
  assert_choice("id_code", colnames(ids))
  data <- data.frame("time" = timestamps,
                     "easting" = as.vector(t(xs)),
                     "northing" = as.vector(t(ys)),
                     "track_id" = rep(ids$id_code, each = NCOL(xs)))
  cols <- setdiff(colnames(ids), "id_code")
  add_cols <- do.call("cbind.data.frame", lapply(ids[, cols], function(x) rep(x, each = NCOL(xs))))
  data <- cbind(data, add_cols)
  as.trackframe(data, time_col = "time", easting_col = "easting",
                 northing_col = "northing", id_col = "track_id")
}


as.trackframe_from_cocomo(xs, ys, timestamps, ids)

tf <- as.trackframe_from_cocomo(xs, ys, timestamps, ids)
class(tf)
head(tf)
