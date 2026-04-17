library(trackframe)
library(tinytest)

tf1 <- trackframe::tf_mini
tf2 <- tf1
tf2$northing <- 1:11
tf_colnames(tf2)

# equal ids
rbind_tf1_tf2 <- trackframe:::rbind.trackframe(tf1, tf2)
expect_inherits(rbind_tf1_tf2, "trackframe")
expect_equal(tf_colnames(rbind_tf1_tf2), tf_colnames(tf1))
expect_equal(rbind_tf1_tf2, rbind.data.frame(tf1, tf2), check.attributes = FALSE)
merge_tf1_tf2 <- trackframe:::merge.trackframe(tf1, tf2)
expect_inherits(merge_tf1_tf2, "trackframe")
expect_equal(NROW(merge_tf1_tf2), 0)
merge_tf1_tf2_all <- trackframe:::merge.trackframe(tf1, tf2, all = TRUE, sort = FALSE)
expect_equal(merge_tf1_tf2_all,
  merge.data.frame(tf1, tf2, all = TRUE, sort = FALSE)[, c("time", "id", "easting", "northing")],
  check.attributes = FALSE)
cbind_tf1_tf2 <- trackframe:::cbind.trackframe(tf1, tf2)
expect_equal(cbind_tf1_tf2,
  cbind.data.frame(tf1, tf2),
  check.attributes = FALSE)
expect_inherits(cbind_tf1_tf2, "trackframe")
expect_equal(tf_colnames(cbind_tf1_tf2), tf_colnames(tf1))
expect_equal(cbind_tf1_tf2, trackframe:::cbind.trackframe(tf1, as.data.frame(tf2)))


tf2b <- tf2
attr(tf2b, "crs") <- "new_crs"
expect_error(trackframe:::rbind.trackframe(tf1, tf2b),
  info = "crs of trackframes do not coincide")
expect_warning(trackframe:::merge.trackframe(tf1, tf2b))
expect_warning(trackframe:::cbind.trackframe(tf1, tf2b))

tf2c <- tf2
attr(tf2c, "crs") <- NA
expect_warning(trackframe:::rbind.trackframe(tf1, tf2c))
expect_warning(trackframe:::merge.trackframe(tf1, tf2c))
expect_warning(trackframe:::cbind.trackframe(tf1, tf2c))

tf2d <- tf2
tf2d <- tf2d[-c(1, 5, 6), ]
expect_inherits(tf2d, "trackframe")
rbind_tf1_tf2d <- trackframe:::rbind.trackframe(tf1, tf2d)
expect_inherits(rbind_tf1_tf2d, "trackframe")
expect_equal(tf_colnames(rbind_tf1_tf2d), tf_colnames(tf1))
expect_equal(rbind_tf1_tf2d, rbind.data.frame(tf1, tf2d), check.attributes = FALSE)
merge_tf1_tf2d <- trackframe:::merge.trackframe(tf1, tf2d)
expect_inherits(merge_tf1_tf2d, "trackframe")
expect_equal(NROW(merge_tf1_tf2d), 0)
merge_tf1_tf2d_all <- trackframe:::merge.trackframe(tf1, tf2d, all = TRUE, sort = FALSE)
expect_equal(merge_tf1_tf2d_all,
  merge.data.frame(tf1, tf2d, all = TRUE, sort = FALSE)[, c("time", "id", "easting", "northing")],
  check.attributes = FALSE)
expect_error(trackframe:::cbind.trackframe(tf1, tf2d),
  info = "arguments imply differing number of rows: 11, 8")


expect_error(trackframe:::rbind.trackframe(tf1, tf2b, tf2c, tf2d),
  info = "crs of trackframes do not coincide")
expect_warning(trackframe:::rbind.trackframe(tf1, tf2, tf2c, tf2d))
expect_equal(trackframe:::rbind.trackframe(tf1, tf2, tf2c, tf2d),
  rbind.data.frame(tf1, tf2, tf2c, tf2d),
  check.attributes = FALSE)
expect_equal(trackframe:::rbind.trackframe(tf1, tf2, tf2c, as.data.frame(tf2d)),
  rbind.data.frame(tf1, tf2, tf2c, tf2d),
  check.attributes = FALSE)

expect_error(trackframe:::cbind.trackframe(tf1, tf2b, tf2c, tf2d),
  info = "crs of arguments imply differing number of rows: 11, 8 do not coincide")
expect_warning(trackframe:::cbind.trackframe(tf1, tf2, tf2c, tf2b))
expect_equal(trackframe:::cbind.trackframe(tf1, tf2, tf2c, tf2b),
  cbind.data.frame(tf1, tf2, tf2c, tf2b),
  check.attributes = FALSE)
expect_equal(trackframe:::cbind.trackframe(tf1, tf2, tf2c, as.data.frame(tf2b)),
  cbind.data.frame(tf1, tf2, tf2c, tf2b),
  check.attributes = FALSE)

#
tf3 <- tf1
names(tf3) <- c("time2", "northing2", "easting2", "id2")
objl <- list(tf1, tf3)
expect_warning(trackframe:::rbind.trackframe(tf1, tf3))
rbind_tf1_tf3 <- trackframe:::rbind.trackframe(tf1, tf3)
expect_equal(colnames(tf1), colnames(rbind_tf1_tf3))
expect_inherits(rbind_tf1_tf3, "trackframe")
merge_tf1_tf3 <- trackframe:::merge.trackframe(tf1, tf3)
expect_equal(tf_colnames(merge_tf1_tf3), tf_colnames(tf1))
expect_equal(trackframe:::merge.trackframe(tf1, tf3, all = TRUE), merge_tf1_tf3)
expect_equal(trackframe:::cbind.trackframe(tf1, tf3),
  cbind.data.frame(tf1, tf3),
  check.attributes = FALSE)

###
tf4 <- tf1
tf4$id3 <- "A"
expect_error(trackframe:::rbind.trackframe(tf1, tf4),
  info = "numbers of columns of arguments do not match")
merge_tf1_tf4 <- trackframe:::merge.trackframe(tf1, tf4, sort = FALSE)
expect_inherits(merge_tf1_tf4, "trackframe")
expect_equal(dim(merge_tf1_tf4), c(11, 5))
expect_equal(names(merge_tf1_tf4), c("time", "id", "easting", "northing", "id3"))
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
expect_equal(cbind_tf1_tf4,
  cbind.data.frame(tf1, tf4),
  check.attributes = FALSE)

#
tf5 <- tf1
tf5$id <- c(rep("A", 5), rep("B", 4), rep("C", 2))
tf5$north <- 20
tf5$east <- 10
expect_error(trackframe:::rbind.trackframe(tf1, tf5),
  info = "numbers of columns of arguments do not match")
expect_equal(NROW(trackframe:::merge.trackframe(tf1, tf5)), 0)
merge_tf1_tf5_all <- trackframe:::merge.trackframe(tf1, tf5, all = TRUE)
expect_inherits(merge_tf1_tf5_all, "trackframe")
expect_equal(dim(merge_tf1_tf5_all), c(22, 6))
expect_equal(names(merge_tf1_tf5_all), c("time", "id", "easting", "northing", "north", "east"))
expect_equal(tf_colnames(merge_tf1_tf5_all),
  c(easting = "easting",
    northing = "northing",
    time = "time",
    id = "id"
  ))
expect_equal(trackframe:::cbind.trackframe(tf1, tf5),
  cbind.data.frame(tf1, tf5),
  check.attributes = FALSE)

# 5b - some overlapping
tf5b <- tf5
tf1
tf5b$id[1:2] <- "track_1"
tf5b$id[10] <- "track_3"
merge_tf1_tf5b <- trackframe:::merge.trackframe(tf1, tf5)
expect_equal(NROW(merge_tf1_tf5b), 0)
merge_tf1_tf5b_all <- trackframe:::merge.trackframe(tf1, tf5, all = TRUE)
expect_equal(NROW(merge_tf1_tf5b_all), NROW(tf1) + NROW(tf5b))

tf5c <- tf5b[, tf_colnames(tf5b)]
expect_equal(trackframe:::cbind.trackframe(tf1, tf5c),
  cbind.data.frame(tf1, tf5c),
  check.attributes = FALSE)


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
expect_equal(dim(merge_tf1_tf6_all), c(22, 4))
expect_equal(trackframe:::cbind.trackframe(tf1, tf6),
  cbind.data.frame(tf1, tf6),
  check.attributes = FALSE)


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
expect_equal(dim(merge_tf1_tf7_all), c(22, 4))
expect_equal(trackframe:::cbind.trackframe(tf1, tf7),
  cbind.data.frame(tf1, tf7),
  check.attributes = FALSE)

#
tf8 <- tf1
tf8$time <- 1:11
expect_error(trackframe:::rbind.trackframe(tf1, tf8),
  info = "Class of time cols differ")
expect_warning(trackframe:::merge.trackframe(x = tf1, y = tf8),
  info = "Class of time cols differ")
expect_equal(trackframe:::cbind.trackframe(tf1, tf8),
  cbind.data.frame(tf1, tf8),
  check.attributes = FALSE)


tf9 <- tf1
tf9$easting <- NULL

expect_error(trackframe:::rbind.trackframe(tf1, tf9),
  info = "numbers of columns of arguments do not match")
expect_error(trackframe:::merge.trackframe(tf1, tf9),
  info = "Not all tf_colnames(tf) are available in trackframe.")
expect_equal(trackframe:::cbind.trackframe(tf1, tf9),
  cbind.data.frame(tf1, tf9),
  check.attributes = FALSE)

tf10 <- tf1
tf10$time <- as.Date(tf10$time)
expect_error(trackframe:::rbind.trackframe(tf1, tf10),
  info = "different time class")
expect_warning(trackframe:::merge.trackframe(tf1, tf10))
expect_equal(trackframe:::cbind.trackframe(tf1, tf10),
  cbind.data.frame(tf1, tf10),
  check.attributes = FALSE)

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
expect_error(trackframe:::merge.trackframe(tf1, df4),
  info = "Use by, or by.x and by.y explicitely when merging a trackframe with a non-trackframe
    data.frame/data.table/tibble")
expect_equal(trackframe:::merge.trackframe(tf1, df4, by = tf_colnames(tf1), sort = FALSE),
  merge.data.frame(tf1, df4, by = tf_colnames(tf1), sort = FALSE),
  check.attributes = FALSE)

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

expect_equal(trackframe:::cbind.trackframe(tf1, df4),
  cbind.data.frame(tf1, df4),
  check.attributes = FALSE)
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

expect_equal(trackframe:::cbind.trackframe(tf1, data.table::as.data.table(dt4)),
  cbind.data.frame(tf1, data.table::as.data.table(dt4)),
  check.attributes = FALSE)

expect_equal(trackframe:::cbind.trackframe(tf1, data.table::as.data.table(tib4)),
  cbind.data.frame(tf1, data.table::as.data.table(tib4)),
  check.attributes = FALSE)

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
