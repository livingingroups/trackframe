library("sf")
library("sftrack")
data("raccoon", package = "sftrack")


raccoon <- head(raccoon, 20)

sftdf <- as_sftrack(raccoon,
                    coords = c("longitude", "latitude"),
                    group = c(id = "animal_id"),
                    time = "timestamp")
head(sftdf)
attributes(sftdf)

sftdf2 <- as_sftrack(raccoon,
                     coords = c("longitude", "latitude"),
                     group = c(id = "animal_id"),
                     time = "timestamp",
                     crs = 4326)
head(sftdf2)
attributes(sftdf2)


raccoon[, c("animal_id", "timestamp", "longitude", "latitude")]
st_coordinates(sftdf$geometry)
# Transform to a projected (Cartesian) coordinate system first
sftdf_projected <- st_transform(sftdf2, crs = 3857)  # Web Mercator (meters)

# Extract Cartesian coordinates and time into a data frame
coords_time <- data.frame(
    x = st_coordinates(sftdf_projected$geometry)[, "X"],
    y = st_coordinates(sftdf_projected$geometry)[, "Y"],
    time = sftdf_projected$timestamp
)
coords_time





