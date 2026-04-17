library(tinytest)
library(trackframe)

tf1 <- trackframe::tf_mini
attr(tf1, "transformation_info") <- NULL
tf2 <- tf1
tf2$northing <- 1:11

tf <- trackframe:::rbind.trackframe(tf1, tf1)
expect_equal(deduping(tf), tf1)

tf <- trackframe:::rbind.trackframe(tf1, tf2)
expect_equal(deduping(tf), tf)
expect_equal(deduping(tf, cols = c("time", "id")), tf1)
