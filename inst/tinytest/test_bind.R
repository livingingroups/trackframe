# This simple wrapper allows that we allways use with = FALSE
"[.data.frame" <- function(x, i, j, drop = FALSE, ...) {
  base::`[.data.frame`(x, i, j, drop = drop)
}

"[.trackframe" <- function(x, i, j, drop = TRUE, ...) {
  has_j <- !missing(j)
  if (missing(i)) {
    i <- seq_len(NROW(x))
  }
  if (missing(j)) {
    j <- seq_len(NCOL(x))
  }
  to_vec <- FALSE
  if (has_j) {
    if (length(j) == 1 && drop == TRUE) {
      to_vec <- TRUE
    }
  }
  if (length(i) == 1 && length(j) > 1) {
    drop <- FALSE
  }
  if (isTRUE(to_vec)) {
    obj <- base::`[.data.frame`(x, i, j, drop = drop)
  } else {
    x_attr <- attributes(x)
    attr_names <- names(x_attr)
    x_attr[names(x_attr) %in% c("names", "row.names", "class")] <- NULL
    obj <- base::`[.data.frame`(x, i, j, drop = drop)
    attributes(obj) <- c(attributes(obj), x_attr)[attr_names]
  }
  return(obj)
}


library(trackframe)
library(tinytest)

tf1 <- trackframe::tf_mini
tf2 <- tf1
tf2$northing <- 1:11
tf_colnames(tf2)

# equal ids
expect_error(trackframe:::rbind.trackframe(tf1, tf2),
  info = "no duplicated time and id entries in trackframes are supported")
merge_tf1_tf2 <- trackframe:::merge.trackframe(tf1, tf2)
expect_inherits(merge_tf1_tf2, "trackframe")
expect_equal(attr(merge_tf1_tf2, "easting"), "easting")
expect_equal(trackframe:::merge.trackframe(tf1, tf2, all = TRUE), merge_tf1_tf2)
expect_equal(merge_tf1_tf2, cbind.data.frame(tf1[, c("time","id", "northing", "easting")],
                                             tf2[, c("northing", "easting")]), check.attributes = FALSE)
expect_error(trackframe:::cbind.trackframe(tf1, tf2),
  info = "keycols (time, easting, northing, id) are not equal for all trackframes")
expect_error(trackframe:::cbind.trackframe(tf1, as.data.frame(tf2)),
  info = "duplicated tf_colnames. key cols not unique anymore.")

tf2b <- tf2
attr(tf2b, "crs") <- "new_crs"
expect_error(trackframe:::rbind.trackframe(tf1, tf2b))
expect_warning(trackframe:::merge.trackframe(tf1, tf2b))
expect_warning(trackframe:::cbind.trackframe(tf1, tf2b))

tf2c <- tf2
attr(tf2b, "crs") <- NA

#
tf3 <- tf1
names(tf3) <- c("time2", "northing2", "easting2", "id2")

expect_error(trackframe:::rbind.trackframe(tf1, tf3))
merge_tf1_tf3 <- trackframe:::merge.trackframe(tf1, tf3)
expect_equal(tf_colnames(merge_tf1_tf3), tf_colnames(tf1))
expect_equal(trackframe:::merge.trackframe(tf1, tf3, all = TRUE), merge_tf1_tf3)
expect_error(trackframe:::cbind.trackframe(tf1, tf3),
  info = "key cols are different.")

###
tf4 <- tf1
tf4$id3 <- "A"
expect_error(trackframe:::rbind.trackframe(tf1, tf4),
  info = "different cols")
merge_tf1_tf4 <- trackframe:::merge.trackframe(tf1, tf4, sort = FALSE)
expect_inherits(merge_tf1_tf4, "trackframe")
expect_equal(dim(merge_tf1_tf4), c(11, 5))
expect_equal(names(merge_tf1_tf4), c("time", "id", "northing", "easting", "id3"))
expect_equal(tf_colnames(merge_tf1_tf4),
             c(easting = "easting",
               northing = "northing",
               time = "time",
               id = "id"
             ))

expect_equal(trackframe:::merge.trackframe(tf1, tf4, all = TRUE),
             trackframe:::merge.trackframe(tf1, tf4))
cbind_tf1_tf4 <- trackframe:::cbind.trackframe(tf1, tf4)
expect_inherits(cbind_tf1_tf4, "trackframe")
expect_equal(cbind_tf1_tf4[, c("time", "id", "northing", "easting", "id3")],
             merge_tf1_tf4)

#
tf5 <- tf1
tf5$id <- c(rep("A", 5), rep("B", 4), rep("C", 2))
tf5$north <- 20
tf5$east <- 10
expect_error(trackframe:::rbind.trackframe(tf1, tf5),
  info = "different cols")
expect_equal(NROW(trackframe:::merge.trackframe(tf1, tf5)), 0)
merge_tf1_tf5_all <- trackframe:::merge.trackframe(tf1, tf5, all = TRUE)
class(merge_tf1_tf5_all)
tf_colnames(merge_tf1_tf5_all)
expect_inherits(merge_tf1_tf5_all, "trackframe")
expect_equal(dim(merge_tf1_tf5_all), c(22, 8))
expect_equal(names(merge_tf1_tf5_all),
             c("time", "id", "northing", "easting", "northing.y", "easting.y", "north", "east"))
expect_equal(tf_colnames(merge_tf1_tf5_all),
             c(easting = "easting",
               northing = "northing",
               time = "time",
               id = "id"
             ))
expect_error(trackframe:::cbind.trackframe(tf1, tf5),
  info = "key cols are different.")

#TODO 5b - some overlapping
tf5b <- tf5
tf1
tf5b$id[1:2] <- "track_1"
tf5b$id[10] <- "track_3"
merge_tf1_tf5b <- trackframe:::merge.trackframe(tf1, tf5)
merge_tf1_tf5b_all <- trackframe:::merge.trackframe(tf1, tf5, all = TRUE)

tf5c <- tf5b[, tf_colnames(tf5b)]
trackframe:::cbind.trackframe(tf1, tf5c) #FIXME


#
tf6 <- tf1
tf6$id <- c(rep("A", 5), rep("B", 4), rep("C", 2))
tf6$northing <- 20
tf6$easting <- 10
rbind_tf1_tf6 <- trackframe:::rbind.trackframe(tf1, tf6)
expect_inherits(rbind_tf1_tf6, "trackframe")
expect_equal(dim(rbind_tf1_tf6), c(22, 4))
expect_equal(names(rbind_tf1_tf6), c("time", "northing", "easting", "id"))
expect_equal(tf_colnames(rbind_tf1_tf6),
             c(easting = "easting",
               northing = "northing",
               time = "time",
               id = "id"
             ))
expect_equal(id(rbind_tf1_tf6), c(tf1$id, tf6$id))

expect_equal(NROW(trackframe:::merge.trackframe(tf1, tf6)), 0)
merge_tf1_tf6_all <- trackframe:::merge.trackframe(tf1, tf6, all = TRUE)
expect_equal(dim(merge_tf1_tf6_all), c(22, 6))
expect_error(trackframe:::cbind.trackframe(tf1, tf6))

#
tf7 <- tf1
tf7$time <- tf7$time + lubridate::days(1)
rbind_tf1_tf7 <- trackframe:::rbind.trackframe(tf1, tf7)
expect_inherits(rbind_tf1_tf7, "trackframe")
expect_equal(dim(rbind_tf1_tf7), c(22, 4))
expect_equal(names(rbind_tf1_tf7), c("time", "northing", "easting", "id"))
expect_equal(tf_colnames(rbind_tf1_tf7),
             c(easting = "easting",
               northing = "northing",
               time = "time",
               id = "id"
             ))

expect_equal(NROW(trackframe:::merge.trackframe(tf1, tf7)), 0)
merge_tf1_tf7_all <- trackframe:::merge.trackframe(tf1, tf7, all = TRUE)
expect_equal(dim(merge_tf1_tf7_all), c(22, 6))
expect_error(trackframe:::cbind.trackframe(tf1, tf7))

#
tf8 <- tf1
tf8$time <- 1:11
expect_error(trackframe:::rbind.trackframe(tf1, tf8),
  info = "different time class")
expect_error(trackframe:::merge.trackframe(tf1, tf8),
  info = "different time class")
expect_error(trackframe:::cbind.trackframe(tf1, tf8), #FIXME
  info = "different time class")

tf9 <- tf1
tf9$easting <- NULL

expect_error(trackframe:::rbind.trackframe(tf1, tf9))
expect_error(trackframe:::merge.trackframe(tf1, tf9)) #FIXME
expect_error(trackframe:::cbind.trackframe(tf1, tf9))

tf10 <- tf1
tf10$time <- as.Date(tf10$time)
expect_error(trackframe:::rbind.trackframe(tf1, tf10),
  info = "different time class")
expect_error(trackframe:::merge.trackframe(tf1, tf10),
  info = "different time class")
expect_error(trackframe:::cbind.trackframe(tf1, tf10),
  info = "different time class") #FIXME

### data.frame/data.table/tibble/matrix tests

#rbind
df6 <- as.data.frame(tf6)
rbind_tf1_df6 <- trackframe:::rbind.trackframe(tf1, df6)
expect_equal(rbind_tf1_df6, rbind_tf1_tf6)

dt6 <- as.trackframe(tf6, coerce_to = "data.table")
rbind_tf1_dt6 <- trackframe:::rbind.trackframe(tf1, dt6)
expect_equal(rbind_tf1_dt6, rbind_tf1_tf6)

tib6 <- as.trackframe(tf6, coerce_to = "tibble")
rbind_tf1_tib6 <- trackframe:::rbind.trackframe(tf1, tib6)
expect_equal(rbind_tf1_tib6, rbind_tf1_tf6)

# merge
df4 <- as.data.frame(tf4)
expect_error(trackframe:::merge.trackframe(tf1, df4))
# merge.data.frame(tf1, df4, by = tf_colnames(tf1)) #nolint

dt4 <- as.trackframe(tf4, coerce_to = "data.table")
merge_tf1_dt4 <- trackframe:::merge.trackframe(tf1, dt4, sort = FALSE)
expect_equal(merge_tf1_dt4, merge_tf1_tf4)

tib4 <- as.trackframe(tf4, coerce_to = "tibble")
merge_tf1_tib4 <- trackframe:::merge.trackframe(tf1, tib4, sort = FALSE)
expect_equal(merge_tf1_tib4, merge_tf1_tf4)

# cbind
cbind_tf1_dt4 <- trackframe:::cbind.trackframe(tf1, dt4)
expect_equal(cbind_tf1_dt4, cbind_tf1_tf4)

cbind_tf1_tib4 <- trackframe:::cbind.trackframe(tf1, tib4)
expect_equal(cbind_tf1_tib4, cbind_tf1_tf4)

expect_error(trackframe:::cbind.trackframe(tf1, df4))
colnames(df4) <- paste0(colnames(df4), "_2")
cbind_tf1_df4 <- trackframe:::cbind.trackframe(tf1, df4)
expect_inherits(cbind_tf1_df4, "trackframe")
expect_equal(dim(cbind_tf1_df4), c(11, 9))
expect_equal(names(cbind_tf1_df4), c(colnames(tf1), colnames(df4)))
expect_equal(tf_colnames(cbind_tf1_df4),
             c(easting = "easting",
               northing = "northing",
               time = "time",
               id = "id"
             ))

expect_error(trackframe:::cbind.trackframe(tf1, data.table::as.data.table(dt4)))
colnames(dt4) <- paste0(colnames(dt4), "_2")
cbind_tf1_dt4 <- trackframe:::cbind.trackframe(tf1, data.table::as.data.table(dt4))

expect_error(trackframe:::cbind.trackframe(tf1, data.table::as.data.table(tib4)))
colnames(tib4) <- paste0(colnames(tib4), "_2")
cbind_tf1_tib4 <- trackframe:::cbind.trackframe(tf1, tibble::as_tibble(tib4))


# matrix
m1 <- as.matrix(tf1[, 2:3])
colnames(m1) <- c("A", "B")
cbind_tf1_m1 <- trackframe:::cbind.trackframe(tf1, m1)
expect_inherits(cbind_tf1_m1, "trackframe")
expect_equal(dim(cbind_tf1_m1), c(11, 6))
expect_equal(names(cbind_tf1_m1), c(colnames(tf1), colnames(m1)))
expect_equal(tf_colnames(cbind_tf1_m1),
             c(easting = "easting",
               northing = "northing",
               time = "time",
               id = "id"
             ))
