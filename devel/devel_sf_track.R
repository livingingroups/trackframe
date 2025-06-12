library(trackframe)
as.track_frame.move2 <- function(data, ...) {
  data_attr <- attributes(data)
  x_y <- st_coordinates(data[[attr(data, "sf_column")]])
  time_index <- attr(data, "time_column")
  id_col <- attr(data, "track_id_column") #move2: The `track_id_column` attribute should be a <character> of length 1
  cols <- setdiff(colnames(data), attr(data, "sf_column"))
  class(data) <- "list"
  data <- data[cols]
  #FIXME: transformations to easting/northing
  data[["easting"]] <- x_y[, 1]
  data[["northing"]] <- x_y[, 2]
  class(data) <- c("tbl_df", "tbl", "data.frame")
  attr(data, "row.names") <- data_attr[["row.names"]]
  as.track_frame(data, time_col = time_index, easting_col = "easting",
                 northing_col = "northing", id_col = id_col)
}


sf:::st_coordinates.sf
sf:::st_geometry.sf

# install.packages("sftrack")
library(sf)
library(sftrack)
data("raccoon")
raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
burstz <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon)
# Input is a data.frame
data <- as_sftrack(raccoon,
                       group = burstz, time = "timestamp",
                       error = NA, coords = c("longitude", "latitude")
)


uid <- sapply(data$sft_group, deparse)
lapply(uid, function(text) eval(parse(text = text)))

as.track_frame.sftrack <- function(data, ...) {
  data_attr <- attributes(data)
  x_y <- st_coordinates(data[[attr(data, "sf_column")]]) #FIXME transformation to cartesian coordinates
  time_index <- attr(data, "time_col")
  id_col <- attr(data, "group_col")
  cols <- setdiff(colnames(data), attr(data, "sf_column"))
  # class(data) <- "list"
  data <- data[,cols]
  data[["track_id"]] <- sapply(data[[id_col]], deparse)
  # lapply(uid, function(text) eval(parse(text = text))) # reverse transformation
  data[["easting"]] <- x_y[, 1]
  data[["northing"]] <- x_y[, 2]
  class(data) <- c("data.frame")
  attr(data, "row.names") <- data_attr[["row.names"]]
  as.track_frame(data, time_col = time_index, easting_col = "easting",
                 northing_col = "northing", id_col = "track_id")
}


data_tf <- as.track_frame.sftrack(data)
attributes(data_tf)

data2 <- data
data2
class(data2)
head(data2)
cols
data2[,cols]

class(data2) <- "list"
data2 <- data2[cols]

###
head(data)
unique(data$animal_id)
unique(data$sft_group)
str(data)

data_attr <- attributes(data)
x_y <- st_coordinates(data[[attr(data, "sf_column")]])

# st_crs(data)
# roads <- st_transform(data, crs = 4326)

time_index <- attr(data, "time_col")
grouping_col <- 
id_cols <- todo
# id_col <- attr(data, "track_id_column") #move2: The `track_id_column` attribute should be a <character> of length 1
cols <- setdiff(colnames(data), attr(data, "sf_column"))
class(data) <- "list"
data <- data[cols]
#FIXME: transformations to easting/northing
data[["easting"]] <- x_y[, 1]
data[["northing"]] <- x_y[, 2]
class(data) <- c("tbl_df", "tbl", "data.frame")
attr(data, "row.names") <- data_attr[["row.names"]]
as.track_frame(data, time_col = time_index, easting_col = "easting",
               northing_col = "northing", id_col = id_col)



library("sf")
df1 <- raccoon[!is.na(raccoon$latitude), ]
sf_df <- st_as_sf(df1, coords = c("longitude", "latitude"))
new_sftrack <- as_sftrack(sf_df, group = c(id = "animal_id"), time = "timestamp")
head(new_sftrack)
attributes(new_sftrack)
unique(new_sftrack$sft_group)


