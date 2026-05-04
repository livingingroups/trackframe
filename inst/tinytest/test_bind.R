library(trackframe)
library(tinytest)

tf1 <- trackframe::tf_mini
tf2 <- tf1
tf2$northing <- 1:11
tf_colnames(tf2)


expect_most_elements_equal <- function(current, target, omit = c(), ...) {
  filter_list <- function(l) {
    l <- l[!names(l) %in% omit]
    l[sort(names(l))]
  }
  expect_equal(filter_list(current), filter_list(target), ...)
}

crses_to_string <- function(...) {
  paste(
    "crs vals:",
    paste(vapply(list(...), crs, character(1)), collapse = ", ")
  )
}

# equal ids ----

## rbind ----
rbind_tf1_tf2 <- rbind(tf1, tf2)
expect_inherits(rbind_tf1_tf2, "trackframe")
expect_most_elements_equal(
  attributes(rbind_tf1_tf2),
  attributes(tf1),
  c("transformation_info", "row.names")
)
expect_equal(tf_colnames(rbind_tf1_tf2), tf_colnames(tf1))
expect_equal(
  rbind_tf1_tf2,
  rbind.data.frame(tf1, tf2),
  check.attributes = FALSE
)

## merge ----
merge_tf1_tf2 <- merge(tf1, tf2)
expect_inherits(merge_tf1_tf2, "trackframe")
expect_equal(NROW(merge_tf1_tf2), 0, info = "inner join, no overlap")
merge_tf1_tf2_all <- merge(
  tf1,
  tf2,
  all = TRUE,
  sort = FALSE
)
expect_most_elements_equal(
  attributes(merge_tf1_tf2_all),
  attributes(tf1),
  c("transformation_info", "row.names")
)
expect_equal(
  merge_tf1_tf2_all,
  rbind_tf1_tf2[, c(
    # just puts columns in the right order, does not omit columns
    "time",
    "northing",
    "easting",
    "id"
  )],
  check.attributes = FALSE
)


## cbind ----
cbind_tf1_tf2 <- cbind(tf1, tf2)
expect_equal(
  cbind_tf1_tf2,
  cbind.data.frame(tf1, tf2),
  check.attributes = FALSE
)
expect_inherits(cbind_tf1_tf2, "trackframe")
expect_equal(tf_colnames(cbind_tf1_tf2), tf_colnames(tf1))
expect_equal(
  cbind_tf1_tf2,
  cbind(tf1, as.data.frame(tf2))
)
expect_most_elements_equal(
  attributes(cbind_tf1_tf2),
  attributes(tf1),
  c("transformation_info", "names")
)


# different crs ----

tf2b <- tf2
attr(tf2b, "crs") <- "new_crs"
expect_error(
  rbind(tf1, tf2b),
  info = "crs of trackframes do not coincide"
)
expect_error(merge(tf1, tf2b))
expect_warning(cbind(tf1, tf2b))
expect_most_elements_equal(
  attributes(cbind(tf1, tf2b)),
  attributes(tf1),
  c("transformation_info", "names")
)

# one crs is NA ----
tf2c <- tf2
attr(tf2c, "crs") <- NA_character_
expect_warning(rbind(tf1, tf2c))
expect_most_elements_equal(
  attributes(rbind(tf1, tf2c)),
  attributes(tf1),
  c("transformation_info", "row.names")
)
expect_error(merge(tf1, tf2c))
expect_warning(cbind(tf1, tf2c))
expect_most_elements_equal(
  attributes(cbind(tf1, tf2c)),
  attributes(tf1),
  c("transformation_info", "names")
)

# different dimensions ----
tf2d <- tf2
tf2d <- tf2d[-c(1, 5, 6), ]
expect_inherits(tf2d, "trackframe")

## rbind ----
rbind_tf1_tf2d <- rbind(tf1, tf2d)
expect_most_elements_equal(
  attributes(rbind_tf1_tf2d),
  attributes(tf1),
  c("transformation_info", "row.names")
)
expect_inherits(rbind_tf1_tf2d, "trackframe")
expect_equal(tf_colnames(rbind_tf1_tf2d), tf_colnames(tf1))
expect_equal(
  rbind_tf1_tf2d,
  rbind.data.frame(tf1, tf2d),
  check.attributes = FALSE
)

# all crs is NA ----

expect_silent(rbind(tf2c, tf2c, tf2c))
expect_silent(merge(tf2c, tf2c))
expect_silent(cbind(tf2c, tf2c))

## merge ----
merge_tf1_tf2d <- merge(tf1, tf2d)
expect_most_elements_equal(
  attributes(merge_tf1_tf2d),
  attributes(tf1),
  c("transformation_info", "row.names")
)
expect_inherits(merge_tf1_tf2d, "trackframe")
expect_equal(NROW(merge_tf1_tf2d), 0)
merge_tf1_tf2d_all <- merge(
  tf1,
  tf2d,
  all = TRUE,
  sort = FALSE
)
expect_equal(
  merge_tf1_tf2d_all,
  merge.data.frame(tf1, tf2d, all = TRUE, sort = FALSE)[, c(
    "time",
    "northing",
    "easting",
    "id"
  )],
  check.attributes = FALSE
)

## cbind ----
expect_error(
  cbind(tf1, tf2d),
  info = "arguments imply differing number of rows: 11, 8"
)

# >1 tf, some crs doesn't match ----

## rbind ----
expect_error(
  rbind(tf1, tf2b, tf2c, tf2d),
  info = paste(
    "crs of trackframes do not coincide",
    crses_to_string(tf1, tf2b, tf2c, tf2d),
    sep = " - "
  )
)

# different order
expect_error(
  rbind(tf1, tf2c, tf2d, tf2b),
  info = paste(
    "crs of trackframes do not coincide",
    crses_to_string(tf1, tf2c, tf2d, tf2b),
    sep = " - "
  )
)

expect_warning(
  rbind(tf1, tf2, tf2c, tf2d),
  info = crses_to_string(tf1, tf2, tf2c, tf2d),
)

expect_equal(
  rbind(tf1, tf2, tf2c, tf2d),
  rbind.data.frame(tf1, tf2, tf2c, tf2d),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(rbind(tf1, tf2, tf2c, tf2d)),
  attributes(tf1),
  c("transformation_info", "row.names")
)

expect_equal(
  rbind(tf1, tf2, tf2c, as.data.frame(tf2d)),
  rbind.data.frame(tf1, tf2, tf2c, tf2d),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(rbind(tf1, tf2, tf2c, as.data.frame(tf2d))),
  attributes(tf1),
  c("transformation_info", "row.names")
)

## cbind ----
expect_error(
  cbind(tf1, tf2b, tf2c, tf2d),
  info = "crs of arguments imply differing number of rows: 11, 8 do not coincide"
)
expect_warning(
  cbind(tf1, tf2, tf2c, tf2b),
  info = crses_to_string(tf1, tf2, tf2c, tf2b)
)
expect_equal(
  cbind(tf1, tf2, tf2c, tf2b),
  cbind.data.frame(tf1, tf2, tf2c, tf2b),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf2, tf2c, tf2b)),
  attributes(tf1),
  c("transformation_info", "names")
)

expect_equal(
  cbind(tf1, tf2, tf2c, as.data.frame(tf2b)),
  cbind.data.frame(tf1, tf2, tf2c, tf2b),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf2, tf2c, as.data.frame(tf2b))),
  attributes(tf1),
  c("transformation_info", "names")
)

# different colnames ----
tf3 <- tf1
names(tf3) <- c("time2", "northing2", "easting2", "id2")
objl <- list(tf1, tf3)

## rbind ----
expect_warning(rbind(tf1, tf3))
rbind_tf1_tf3 <- rbind(tf1, tf3)
expect_equal(colnames(tf1), colnames(rbind_tf1_tf3))
expect_equal(tf_colnames(tf1), tf_colnames(rbind_tf1_tf3))
expect_inherits(rbind_tf1_tf3, "trackframe")
tf3_cn <- tf3
colnames(tf3_cn) <- colnames(tf1)
expect_equal(rbind_tf1_tf3,
  rbind.data.frame(tf1, tf3_cn),
  check.attributes = FALSE)
expect_most_elements_equal(
  attributes(rbind_tf1_tf3),
  attributes(tf1),
  c("transformation_info", "row.names")
)

## merge ----
merge_tf1_tf3 <- merge(tf1, tf3)
expect_equal(tf_colnames(merge_tf1_tf3), tf_colnames(tf1))
expect_most_elements_equal(
  attributes(merge_tf1_tf3),
  attributes(tf1),
  c("transformation_info", "row.names")
)

# just testing that `all=TRUE` doesn't change output
expect_equal(merge(tf1, tf3, all = TRUE), merge_tf1_tf3)

## cbind ----
expect_equal(
  cbind(tf1, tf3),
  cbind.data.frame(tf1, tf3),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf3)),
  attributes(tf1),
  c("transformation_info", "names")
)


expect_error(merge(tf1, tf3, by = c("time", "id")),
  info = "Error in fix.by(by.y, y) : 'by' must specify uniquely valid columns")
merge_tf1_tf3_byxy <- merge(tf1, tf3, by.x = c("time", "id"),
  by.y = c("time2", "id2"), sort = FALSE)

# additional column ----
tf4 <- tf1
tf4$id3 <- "A"

## rbind ----
expect_error(
  rbind(tf1, tf4),
  info = "number of columns does not match"
)

## merge ----
merge_tf1_tf4 <- merge(tf1, tf4, sort = FALSE)
expect_inherits(merge_tf1_tf4, "trackframe")
expect_equal(dim(merge_tf1_tf4), c(11, 5))
expect_equal(
  names(merge_tf1_tf4),
  c("time", "northing", "easting", "id", "id3")
)
expect_equal(
  tf_colnames(merge_tf1_tf4),
  c(time = "time", northing = "northing", easting = "easting", id = "id")
)
expect_most_elements_equal(
  attributes(merge_tf1_tf4),
  attributes(tf1),
  c("transformation_info", "names", "row.names")
)
expect_equal(merge_tf1_tf4, tf4, check.attributes = FALSE)

expect_equal(
  merge(tf1, tf4, all = TRUE),
  merge(tf1, tf4)
)


merge_tf1_tf4_by <- merge(tf1, tf4, by = c("time", "id"), sort = FALSE)
merge_tf1_tf4_byxy <- merge(tf1, tf4, by.x = c("time", "id"),
  by.y = c("time", "id"), sort = FALSE)
expect_equal(merge_tf1_tf4_by, merge_tf1_tf4_byxy)
expect_most_elements_equal(
  attributes(merge_tf1_tf4_by),
  attributes(tf1),
  c("transformation_info", "names", "row.names")
)

## cbind ----
cbind_tf1_tf4 <- cbind(tf1, tf4)
expect_inherits(cbind_tf1_tf4, "trackframe")
expect_equal(
  cbind_tf1_tf4,
  cbind.data.frame(tf1, tf4),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind_tf1_tf4),
  attributes(tf1),
  c("transformation_info", "names")
)

# different ids and additional cols

tf5 <- tf1
tf5$id <- c(rep("A", 5), rep("B", 4), rep("C", 2))
tf5$A <- 20
tf5$B <- 10

## rbind ----
expect_error(
  rbind(tf1, tf5),
  info = "numbers of columns of arguments do not match"
)

## merge ----
# no overlap due to different id
expect_equal(NROW(merge(tf1, tf5)), 0)

merge_tf1_tf5_all <- merge(tf1, tf5, all = TRUE)
expect_inherits(merge_tf1_tf5_all, "trackframe")
expect_equal(dim(merge_tf1_tf5_all), c(22, 6))
expect_equal(
  names(merge_tf1_tf5_all),
  c("time", "northing", "easting", "id", "A", "B")
)
expect_equal(
  tf_colnames(merge_tf1_tf5_all),
  c(time = "time", northing = "northing", easting = "easting", id = "id")
)
expect_most_elements_equal(
  attributes(merge_tf1_tf5_all),
  attributes(tf1),
  c("transformation_info", "names", "row.names")
)
expect_equal(merge_tf1_tf5_all$A, c(rep(20, NROW(tf1)), rep(NA, NROW(tf5))))
expect_equal(merge_tf1_tf5_all$B, c(rep(10, NROW(tf1)), rep(NA, NROW(tf5))))

## cbind ----
expect_equal(
  cbind(tf1, tf5),
  cbind.data.frame(tf1, tf5),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf5)),
  attributes(tf1),
  c("transformation_info", "names")
)

# 5b - some overlapping ----
tf5b <- tf5
tf5b$id[1:2] <- "track_1"
tf5b$id[10] <- "track_3"
merge_tf1_tf5b <- merge(tf1, tf5b)
expect_equal(NROW(merge_tf1_tf5b), 3)
merge_tf1_tf5b_all <- merge(tf1, tf5b, all = TRUE)
expect_equal(NROW(merge_tf1_tf5b_all), 2 * NROW(tf1) - 3)
expect_most_elements_equal(
  attributes(merge_tf1_tf5b),
  attributes(tf1),
  c("transformation_info", "names", "row.names")
)

tf5c <- tf5b[, tf_colnames(tf5b)]
expect_equal(
  cbind(tf1, tf5c),
  cbind.data.frame(tf1, tf5c),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf5c)),
  attributes(tf1),
  c("transformation_info", "names")
)


# Key cols different values ----
tf6 <- tf1
tf6$id <- c(rep("A", 5), rep("B", 4), rep("C", 2))
tf6$northing <- 20
tf6$easting <- 10
rbind_tf1_tf6 <- rbind(tf1, tf6)
expect_inherits(rbind_tf1_tf6, "trackframe")
expect_most_elements_equal(
  attributes(rbind_tf1_tf6),
  attributes(tf1),
  c("transformation_info", "row.names")
)

# dim(tf1) is c(11, 4)
expect_equal(dim(rbind_tf1_tf6), c(22, 4))
expect_equal(names(rbind_tf1_tf6), c("time", "northing", "easting", "id"))
expect_equal(
  tf_colnames(rbind_tf1_tf6),
  c(time = "time", northing = "northing", easting = "easting",  id = "id")
)
expect_equal(id(rbind_tf1_tf6), c(tf1$id, tf6$id))
expect_equal(rbind_tf1_tf6, rbind.data.frame(tf1, tf6), check.attributes = FALSE)

expect_equal(NROW(merge(tf1, tf6)), 0)
merge_tf1_tf6_all <- merge(tf1, tf6, all = TRUE)
expect_equal(dim(merge_tf1_tf6_all), c(22, 4))
expect_equal(merge_tf1_tf6_all, sort(rbind(tf1, tf6)), check.attributes = FALSE)

expect_equal(
  cbind(tf1, tf6),
  cbind.data.frame(tf1, tf6),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf6)),
  attributes(tf1),
  c("transformation_info", "names")
)

# Different time values ----
tf7 <- tf1
tf7$time <- tf7$time + lubridate::days(1)
rbind_tf1_tf7 <- rbind(tf1, tf7)
expect_inherits(rbind_tf1_tf7, "trackframe")
expect_equal(dim(rbind_tf1_tf7), c(22, 4))
expect_equal(names(rbind_tf1_tf7), c("time", "northing", "easting", "id"))
expect_equal(
  tf_colnames(rbind_tf1_tf7),
  c(time = "time", northing = "northing", easting = "easting", id = "id")
)
expect_most_elements_equal(
  attributes(rbind_tf1_tf7),
  attributes(tf1),
  c("transformation_info", "row.names")
)

expect_equal(NROW(merge(tf1, tf7)), 0)
merge_tf1_tf7_all <- merge(tf1, tf7, all = TRUE)
expect_equal(dim(merge_tf1_tf7_all), c(22, 4))
expect_equal(
  cbind(tf1, tf7),
  cbind.data.frame(tf1, tf7),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(merge_tf1_tf7_all),
  attributes(tf1),
  c("transformation_info", "row.names")
)

# Different time class ----
tf8 <- tf1
tf8$time <- 1:11
expect_error(
  rbind(tf1, tf8),
  info = "Class of time cols differ"
)
expect_warning(
  merge(x = tf1, y = tf8),
  info = "Class of time cols differ"
)
merge_tf1_tf8 <- merge(x = tf1, y = tf8)
expect_equal(dim(merge_tf1_tf8), c(0, 4))
# check result and check result with all=TRUE?

merge_tf1_tf8_all <- merge(x = tf1, y = tf8, all = TRUE)
expect_equal(dim(merge_tf1_tf8_all), c(22, 4))
expect_equal(sort(merge_tf1_tf8_all$time), sort(c(as.POSIXct(tf8$time), as.POSIXct(tf1$time))))
expect_most_elements_equal(
  attributes(merge_tf1_tf8_all),
  attributes(tf1),
  c("transformation_info", "row.names")
)

expect_equal(
  cbind(tf1, tf8),
  cbind.data.frame(tf1, tf8),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf8)),
  attributes(tf1),
  c("transformation_info", "names")
)


# Totally missing key col ----

tf9 <- tf1
tf9$easting <- NULL

expect_error(
  rbind(tf1, tf9),
  info = "numbers of columns of arguments do not match"
)
expect_error(
  merge(tf1, tf9),
  info = "Not all tf_colnames(tf) are available in trackframe."
)
expect_equal(
  cbind(tf1, tf9),
  cbind.data.frame(tf1, tf9),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf9)),
  attributes(tf1),
  c("transformation_info", "names")
)

expect_equal(
  cbind(tf9, tf1),
  cbind.data.frame(tf9, tf1),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf9, tf1)),
  attributes(tf1),
  c("transformation_info", "names")
)

tf10 <- tf1
tf10$time <- as.Date(tf10$time)
expect_error(
  rbind(tf1, tf10),
  info = "different time class"
)
expect_warning(merge(tf1, tf10))
expect_equal(
  cbind(tf1, tf10),
  cbind.data.frame(tf1, tf10),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, tf10)),
  attributes(tf1),
  c("transformation_info", "names")
)

#data.frame/data.table/tibble/matrix tests ----

## rbind ----
df6 <- as.data.frame(tf6)
rbind_tf1_df6 <- rbind(tf1, df6)
expect_equal(rbind_tf1_df6, rbind_tf1_tf6)
expect_most_elements_equal(
  attributes(rbind_tf1_df6),
  attributes(tf1),
  c("transformation_info", "row.names")
)

dt6 <- as.trackframe(tf6, coerce_to = "data.table")
rbind_tf1_dt6 <- rbind(tf1, dt6)
expect_equal(rbind_tf1_dt6, rbind_tf1_tf6)
expect_most_elements_equal(
  attributes(rbind_tf1_dt6),
  attributes(tf1),
  c("transformation_info", "row.names")
)

tib6 <- as.trackframe(tf6, coerce_to = "tibble")
rbind_tf1_tib6 <- rbind(tf1, tib6)
expect_equal(rbind_tf1_tib6, rbind_tf1_tf6)
expect_most_elements_equal(
  attributes(rbind_tf1_tib6),
  attributes(tf1),
  c("transformation_info", "row.names")
)

## checking inverse ----

rbind_dt6_tf1 <- rbind(dt6, tf1)
expect_inherits(rbind_dt6_tf1, "trackframe")
expect_inherits(rbind_dt6_tf1, "data.table")
expect_equal(sort(rbind_dt6_tf1), sort(rbind_tf1_dt6), check.attributes = FALSE)
expect_most_elements_equal(
  attributes(rbind_dt6_tf1),
  attributes(dt6),
  c("transformation_info", "row.names")
)

rbind_tib6_tf1 <- rbind(tib6, tf1)
expect_inherits(rbind_tib6_tf1, "trackframe")
expect_inherits(rbind_tib6_tf1, "tbl_df")
expect_equal(sort(rbind_tib6_tf1), sort(rbind_tf1_tib6), check.attributes = FALSE)
expect_most_elements_equal(
  attributes(rbind_tib6_tf1),
  attributes(tib6),
  c("transformation_info", "row.names")
)

## merge ----
df4 <- as.data.frame(tf4)
expect_error(
  merge(tf1, df4),
  info = "Use by, or by.x and by.y explicitely when merging a trackframe with a non-trackframe
    data.frame/data.table/tibble"
)
expect_equal(
  merge(tf1, df4, by = tf_colnames(tf1), sort = FALSE),
  merge.data.frame(tf1, df4, by = tf_colnames(tf1), sort = FALSE),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(merge(tf1, df4, by = tf_colnames(tf1), sort = FALSE)),
  attributes(tf1),
  c("transformation_info", "names", "row.names")
)

dt4 <- as.trackframe(tf4, coerce_to = "data.table")
merge_tf1_dt4 <- merge(tf1, dt4, sort = FALSE)
expect_equal(merge_tf1_dt4, merge_tf1_tf4)
expect_most_elements_equal(
  attributes(merge_tf1_dt4),
  attributes(tf1),
  c("transformation_info", "names", "row.names")
)

tib4 <- as.trackframe(tf4, coerce_to = "tibble")
merge_tf1_tib4 <- merge(tf1, tib4, sort = FALSE)
expect_equal(merge_tf1_tib4, merge_tf1_tf4)
expect_most_elements_equal(
  attributes(merge_tf1_tib4),
  attributes(tf1),
  c("transformation_info", "names", "row.names")
)

## checking inverse ----

merge_dt4_tf1 <- merge(dt4, tf1)
expect_inherits(merge_dt4_tf1, "trackframe")
expect_inherits(merge_dt4_tf1, "data.table")
expect_equal(sort(merge_dt4_tf1), sort(merge_tf1_dt4), check.attributes = FALSE)
expect_most_elements_equal(
  attributes(merge_dt4_tf1),
  attributes(dt4),
  c("transformation_info", "row.names")
)

merge_tib4_tf1 <- merge(tib4, tf1)
expect_inherits(merge_tib4_tf1, "trackframe")
expect_inherits(merge_tib4_tf1, "tbl_df")
expect_equal(sort(merge_tib4_tf1), sort(merge_tf1_tib4), check.attributes = FALSE)
expect_most_elements_equal(
  attributes(merge_tib4_tf1),
  attributes(tib4),
  c("transformation_info", "row.names")
)

## cbind ----
cbind_tf1_dt4 <- cbind(tf1, dt4)
expect_equal(cbind_tf1_dt4, cbind_tf1_tf4)
expect_most_elements_equal(
  attributes(cbind_tf1_dt4),
  attributes(tf1),
  c("transformation_info", "names")
)

cbind_tf1_tib4 <- cbind(tf1, tib4)
expect_equal(cbind_tf1_tib4, cbind_tf1_tf4)
expect_most_elements_equal(
  attributes(cbind_tf1_tib4),
  attributes(tf1),
  c("transformation_info", "names")
)

expect_equal(
  cbind(tf1, df4),
  cbind.data.frame(tf1, df4),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, df4)),
  attributes(tf1),
  c("transformation_info", "names")
)

## checking inverse ----

cbind_dt4_tf1 <- cbind(dt4, tf1)
expect_inherits(cbind_dt4_tf1, "trackframe")
expect_inherits(cbind_dt4_tf1, "data.table")
expect_equal(cbind_dt4_tf1, cbind_tf1_dt4[, colnames(cbind_dt4_tf1)], check.attributes = FALSE)
expect_most_elements_equal(
  attributes(cbind_dt4_tf1),
  attributes(dt4),
  c("transformation_info", "names")
)

cbind_tib4_tf1 <- cbind(tib4, tf1)
expect_inherits(cbind_tib4_tf1, "trackframe")
expect_inherits(cbind_tib4_tf1, "tbl_df")
expect_equal(cbind_tib4_tf1, cbind_tf1_tib4[, colnames(cbind_tib4_tf1)], check.attributes = FALSE)
expect_most_elements_equal(
  attributes(cbind_tib4_tf1),
  attributes(tib4),
  c("transformation_info", "names")
)


# Rename key cols  ----

colnames(df4) <- paste0(colnames(df4), "_2")
cbind_tf1_df4 <- cbind(tf1, df4)
expect_inherits(cbind_tf1_df4, "trackframe")
expect_equal(dim(cbind_tf1_df4), c(11, 9))
expect_equal(names(cbind_tf1_df4), c(colnames(tf1), colnames(df4)))
expect_equal(
  tf_colnames(cbind_tf1_df4),
  c(time = "time", northing = "northing", easting = "easting", id = "id")
)
expect_most_elements_equal(
  attributes(cbind_tf1_df4),
  attributes(tf1),
  c("transformation_info", "names")
)

expect_equal(
  cbind(tf1, data.table::as.data.table(dt4)),
  cbind.data.frame(tf1, data.table::as.data.table(dt4)),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, data.table::as.data.table(dt4))),
  attributes(tf1),
  c("transformation_info", "names")
)

expect_equal(
  cbind(tf1, data.table::as.data.table(tib4)),
  cbind.data.frame(tf1, data.table::as.data.table(tib4)),
  check.attributes = FALSE
)
expect_most_elements_equal(
  attributes(cbind(tf1, data.table::as.data.table(tib4))),
  attributes(tf1),
  c("transformation_info", "names")
)


# matrix
m1 <- as.matrix(tf1[, 2:3, silent = TRUE])
colnames(m1) <- c("A", "B")
cbind_tf1_m1 <- cbind(tf1, m1)
expect_inherits(cbind_tf1_m1, "trackframe")
expect_equal(dim(cbind_tf1_m1), c(11, 6))
expect_equal(names(cbind_tf1_m1), c(colnames(tf1), colnames(m1)))
expect_equal(
  tf_colnames(cbind_tf1_m1),
  c(time = "time", northing = "northing", easting = "easting", id = "id")
)
expect_most_elements_equal(
  attributes(cbind_tf1_m1),
  attributes(tf1),
  c("transformation_info", "names")
)
