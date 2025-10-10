# backtransform
library(trackframe)

projected_crs <- "EPSG:32632"

# rename cols to easting northing
df_mini <- trackframe::df_mini
df_mini[, c("easting", "northing")] <- df_mini[, c("latitude", "longitude")]
df_mini[, c("latitude", "longitude")] <- NULL

# move2
library(move2)
m2 <- mt_as_move2(df_mini,
  coords = c("easting", "northing"),
  time_column = "time",
  track_id_column = "id",
  crs = 4326
)
tf <- as.trackframe(data = m2)
expect_equal(tf_backtransform(tf), m2)

# sftrack
library(sftrack)
sft <- as_sftrack(df_mini, coords = c("easting", "northing"), crs = projected_crs)
tf <- as.trackframe(data = sft)
expect_equal(tf_backtransform(tf), sft)


# rename cols to a,b
df_mini <- trackframe::df_mini
df_mini[, c("a", "b")] <- df_mini[, c("latitude", "longitude")]
df_mini[, c("latitude", "longitude")] <- NULL
sftrack_a_n <- as_sftrack(df_mini, coords = c("a", "b"), crs = projected_crs)
tf_n <- as.trackframe(data = sftrack_a_n)
expect_equal(tf_backtransform(tf_n), sftrack_a_n)
