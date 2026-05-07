# test_subsetting
library(tinytest)
library(trackframe)

# data.frame
tf1 <- trackframe::tf_mini
tf2 <- tf1
tf2$id2 <- "id2"
expect_inherits(tf2, "trackframe")
expect_inherits(tf2, "data.frame")

tf3 <- tf2[, trackframe::tf_colnames(tf2)]
expect_inherits(tf3, "trackframe")
attr_tf2 <- attributes(tf2)
attr_tf2$names <- names(tf3)
expect_equal(attributes(tf3), attr_tf2)
expect_equal(names(tf3), as.character(trackframe::tf_colnames(tf2)))

tf4 <- tf2[1, ]
expect_inherits(tf4, "trackframe")
attr_tf2 <- attributes(tf2)
attr_tf2$row.names <- as.numeric(rownames(tf4))
expect_equal(attributes(tf4), attr_tf2)
expect_equal(rownames(tf4), "1")

tf5 <- tf2[, 2]
expect_inherits(tf5, "numeric")

tf6 <- tf2[-2, ]
expect_inherits(tf6, "trackframe")
attr_tf2 <- attributes(tf2)
attr_tf2$row.names <- as.numeric(rownames(tf6))
expect_equal(attributes(tf6), attr_tf2)
expect_equal(rownames(tf6), rownames(tf2)[-2])

# data.table
library(data.table)
tf1 <- as.trackframe(trackframe::tf_mini, coerce_to = "data.table")
expect_inherits(tf1, "trackframe")
expect_inherits(tf1, "data.table")
tf2 <- tf1
tf2$id2 <- "id2"
expect_inherits(tf2, "trackframe")
expect_inherits(tf2, "data.table")


dt2 <- as.data.table(tf2)
expect_equal(tf2[, tf_colnames(tf2), with = TRUE], dt2[, tf_colnames(tf2)])
expect_equal(tf2[, tf_colnames(tf2), with = FALSE], dt2[, tf_colnames(tf2), with = FALSE],
  check.attributes = FALSE)
tf3 <- tf2[, tf_colnames(tf2), with = FALSE]
expect_inherits(tf3, "trackframe")
expect_inherits(tf3, "data.table")
attr_tf2 <- attributes(tf2)
attr_tf2$names <- names(tf3)
expect_equal(attributes(tf3), attr_tf2)
expect_equal(names(tf3), as.character(trackframe::tf_colnames(tf2)))

tf4 <- tf2[1, ]
expect_inherits(tf4, "trackframe")
expect_inherits(tf4, "data.table")
attr_tf2 <- attributes(tf2)
attr_tf2$row.names <- as.numeric(rownames(tf4))
expect_equal(attributes(tf4), attr_tf2)
expect_equal(rownames(tf4), "1")



# tibble
library(tibble)
tf1 <- as.trackframe(trackframe::tf_mini, coerce_to = "tibble")
expect_inherits(tf1, "trackframe")
expect_inherits(tf1, "tbl")
tf2 <- tf1
tf2$id2 <- "id2"
expect_inherits(tf2, "trackframe")
expect_inherits(tf2, "tbl")

tf3 <- tf2[, tf_colnames(tf2)]
expect_inherits(tf3, "trackframe")
expect_inherits(tf3, "tbl")
attr_tf2 <- attributes(tf2)
attr_tf2$names <- names(tf3)
expect_equal(attributes(tf3), attr_tf2)
expect_equal(names(tf3), as.character(trackframe::tf_colnames(tf2)))

tf4 <- tf2[1, ]
expect_inherits(tf4, "trackframe")
expect_inherits(tf4, "tbl")
attr_tf2 <- attributes(tf2)
attr_tf2$row.names <- as.numeric(rownames(tf4))
expect_equal(attributes(tf4), attr_tf2)
expect_equal(rownames(tf4), "1")



###
# non trackframe vanilla, data.table, tibble
data(df_mini, package = "trackframe")
dt_mini <- as.data.table(df_mini)
tbl_mini <- as_tibble(df_mini)

# trackframe vanilla, data.table, tibble
df_tf_mini <- trackframe::as.trackframe(df_mini, crs = NA)
dt_tf_mini <- trackframe::as.trackframe(dt_mini, coerce_to = NULL, crs = NA)
tbl_tf_mini <- trackframe::as.trackframe(tbl_mini, coerce_to = NULL, crs = NA)

df_tf_mini_time <- df_tf_mini[, "time"]
expect_true(!any(class(df_tf_mini_time) %in% "trackframe"))
expect_equal(df_tf_mini_time, df_mini[, "time"])

dt_tf_mini_time <- dt_tf_mini[, "time"]
expect_silent(dt_tf_mini[, "time"])
expect_true(!any(class(dt_tf_mini_time) %in% "trackframe"))
expect_equal(dt_tf_mini_time, dt_mini[, "time"])

tbl_tf_mini_time <- tbl_tf_mini[, "time"]
expect_silent(tbl_tf_mini[, "time"])
expect_true(!any(class(tbl_tf_mini_time) %in% "trackframe"))
expect_equal(tbl_tf_mini_time, tbl_mini[, "time"])


dt_tf_mini_time2 <- dt_tf_mini[, time]
expect_true(!any(class(dt_tf_mini_time2) %in% "trackframe"))
expect_equal(dt_tf_mini_time2, dt_mini[, time])
expect_equal(dt_tf_mini[, time, drop = FALSE], dt_mini[, time, drop = FALSE])

df_tf_mini$A <- "A"
df_tf_mini2 <- df_tf_mini[, c("time", "id", "easting", "northing")]
expect_equal(class(df_tf_mini2), c("trackframe", "data.frame"))

dt_tf_mini$A <- "A"
dt_tf_mini2 <- dt_tf_mini[, c("time", "id", "easting", "northing")]
expect_equal(class(dt_tf_mini2), c("trackframe", "data.table", "data.frame"))

tbl_tf_mini$A <- "A"
tbl_tf_mini2 <- tbl_tf_mini[, c("time", "id", "easting", "northing")]
expect_equal(class(tbl_tf_mini2), c("trackframe", "tbl_df", "tbl", "data.frame"))

expect_equal(df_tf_mini[1, "time"], df_mini[1, "time"])
expect_equal(dt_tf_mini[1, "time"], dt_mini[1, "time"])
expect_equal(tbl_tf_mini[1, "time"], tbl_mini[1, "time"])
expect_equal(dt_tf_mini[1, time], dt_mini[1, time])
