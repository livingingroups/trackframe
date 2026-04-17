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

tf3 <- tf2[, tf_colnames(tf2)]
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
