library("checkmate")

# tsibble: key, index
# move2:  coords: 
# sftrack: 
# sftime: time_column

guess_col <- function(cols, candidates) {
    idx <- which(cols %in% candidates)
    if (length(idx) > 0) idx[1] else NA_integer_
}


update_colnames <- function(cols, col, colname, candidates) {
    if (missing(col) && !is.null(candidates)) {
        colid <- guess_col(cols, candidates)
        if (is.na(colid)) {
            emsg <- "%s column could not be infered, please provide argument '%s'!"
            stop(sprintf(emsg, colname, colname))
        }
    } else if (!missing(col)) {
        assert_choice(col, cols)
        colid <- which(cols == col)
    } else {
        return(cols)
    }
    cols[colid] <- colname
    cols
}


track_frame <- function(data, time_col, lon_col, lat_col, alt_col, id_col) {
    colnames(data) <- update_colnames(colnames(data), col = time_col, colname = "time", candidates = c("time", "timestamp"))
    colnames(data) <- update_colnames(colnames(data), col = lon_col, colname = "lon", candidates = c("lon", "longitude"))
    colnames(data) <- update_colnames(colnames(data), col = lat_col, colname = "lat", candidates = c("lat", "latitude"))
    colnames(data) <- update_colnames(colnames(data), col = alt_col, colname = "alt", candidates = NULL)
    colnames(data) <- update_colnames(colnames(data), col = id_col, colname = "id", candidates = NULL)
    if (!missing(id_col)) {
        id_col
    }
    cols <- c("id", "time", "lon", "lat", "alt")
    cols <- c(intersect(cols, colnames(data)), setdiff(colnames(data), cols))
    data <- data[, cols]
    class(data) <- union("track_frame", class(data))
    data
}


data <- head(raccoon[!is.na(raccoon$latitude), ])
track_frame(data)
track_frame(data, "timestamp")
track_frame(data, id_col = "animal_id")



check_time_col <- function()



as.track_frame <- function(data, ...) {
    UseMethod("as.track_frame")
}


check_time_index <- function(x) {
    
}

id_cols <- c("a", "b")
choices <- letters
check_choice(id_cols, choices, null.ok = TRUE)

as.track_frame.sftrack <- function(data, ...) {
    data_attr <- attributes(data)
    lon_lat <- st_coordinates(data[[attr(data, "sf_column")]])
    time_col <- attr(data, "time_col")
    id_cols <- attr(data, "group_col")
    cols <- setdiff(colnames(data), attr(data, "sf_column"))
    class(data) <- "list"
    data <- data[cols]
    data[["lon"]] <- lon_lat[, 1]
    data[["lat"]] <- lon_lat[, 2]
    class(data) <- c("tbl_df", "tbl", "data.frame")
    attr(data, "row.names") <- data_attr[["row.names"]]
    as.track_frame(data, time_col = time_col, lon_col = "lon", lat_col = "lat", id_cols = id_cols)
}





?tsibble

#
# convert sftrack
#
library("sftrack")

data("raccoon")
d <- head(raccoon[!is.na(raccoon$latitude), ])
str(d)
obj_sftrack <- as_sftrack(d,
                          coords = c("longitude", "latitude"),
                          group = c(id = "animal_id"),
                          time = "timestamp")
str(obj_sftrack)
str(attributes(obj_sftrack))
str(obj_sftrack$sft_group)
obj_sftrack$geometry

as.data.frame(obj_sftrack)


data <- obj_sftrack
class(data[[attr(data, "sf_column")]])
str(data)

data[[attr(data, "group_col")]]

data[[attr(data, "time_col")]]


as.track_frame.sftrack <- function(data, ...) {
    lon_lat <- sf::st_coordinates(data[[attr(data, "sf_column")]])    
    colnames(lon_lat) <- c("lon", "lat")

}




time <- "timestamp"

 = "time"
  = ""
   = ""
    = "altitude"
 = "id"


#
# convert move2
#
library("move2")

fisher_data <- mt_read(mt_example())
class(fisher_data)
str(fisher_data)
str(attributes(fisher_data))
# List of 10
#  $ names          : chr [1:21] "event-id" "visible" "timestamp" "behavioural-classification" ...
#  $ row.names      : int [1:47347] 1 2 3 4 5 6 7 8 9 10 ...
#  $ spec           :
#   .. cols(
#   ..   `event-id` = col_big_integer(),
#   ..   visible = col_logical(),
#   ..   timestamp = col_datetime(format = ""),
#   ..   `location-long` = col_double(),
#   ..   `location-lat` = col_double(),
#   ..   `behavioural-classification` = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#   ..   `eobs:battery-voltage` = col_integer(),
#   ..   `eobs:fix-battery-voltage` = col_integer(),
#   ..   `eobs:horizontal-accuracy-estimate` = col_double(),
#   ..   `eobs:key-bin-checksum` = col_big_integer(),
#   ..   `eobs:speed-accuracy-estimate` = col_double(),
#   ..   `eobs:start-timestamp` = col_datetime(format = ""),
#   ..   `eobs:status` = col_factor(levels = c("A", "B", "C", "D"), ordered = TRUE, include_na = FALSE),
#   ..   `eobs:temperature` = col_integer(),
#   ..   `eobs:type-of-fix` = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#   ..   `eobs:used-time-to-get-fix` = col_integer(),
#   ..   `ground-speed` = col_double(),
#   ..   heading = col_double(),
#   ..   `height-above-ellipsoid` = col_double(),
#   ..   `manually-marked-outlier` = col_logical(),
#   ..   `sensor-type` = col_factor(levels = c("bird-ring", "gps", "radio-transmitter", "argos-doppler-shift", 
#   ..     "natural-mark", "acceleration", "solar-geolocator", "accessory-measurements", 
#   ..     "solar-geolocator-raw", "barometer", "magnetometer", "orientation", 
#   ..     "solar-geolocator-twilight", "acoustic-telemetry", "gyroscope", 
#   ..     "heart-rate", "sigfox-geolocation", "proximity", "geolocation-api", 
#   ..     "gnss", "derived"), ordered = FALSE, include_na = FALSE),
#   ..   `individual-taxon-canonical-name` = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#   ..   `tag-local-identifier` = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#   ..   `individual-local-identifier` = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#   ..   `study-name` = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
#   ..   .delim = ","
#   .. )
#  $ problems       :<externalptr> 
#  $ sf_column      : chr "geometry"
#  $ agr            : Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA ...
#   ..- attr(*, "names")= chr [1:20] "event-id" "visible" "timestamp" "behavioural-classification" ...
#  $ time_column    : chr "timestamp"
#  $ track_id_column: chr "individual-local-identifier"
#  $ track_data     :'data.frame':        8 obs. of  4 variables:
#   ..$ individual-local-identifier    : Factor w/ 8 levels "F1","F2","F3",..: 1 2 3 4 5 6 7 8
#   ..$ individual-taxon-canonical-name: Factor w/ 1 level "Martes pennanti": 1 1 1 1 1 1 1 1
#   ..$ study-name                     : Factor w/ 1 level "Martes pennanti LaPoint New York": 1 1 1 1 1 1 1 1
#   ..$ tag-local-identifier           : Factor w/ 8 levels "1072","1465",..: 1 2 3 4 5 6 7 8
#  $ class          : chr [1:6] "move2" "sf" "spec_tbl_df" "tbl_df" ...
cols <- c("individual-local-identifier", "individual-taxon-canonical-name", "study-name", "tag-local-identifier")
cols %in% colnames(fisher_data)
attr(fisher_data, "track_data")

data <- head(fisher_data)
str(attributes(data))

data[[attr(data, "sf_column")]]
colnames(data)
as.data.frame(data)
attr(data, "agr")
attr(data, "track_data")


as.track_frame.move2 <- function(data, ...) {
    data_attr <- attributes(data)
    lon_lat <- sf::st_coordinates(data[[attr(data, "sf_column")]])
    time_col <- attr(data, "time_column")
    id_col <- attr(data, "track_id_column")
    cols <- setdiff(colnames(data), attr(data, "sf_column"))
    class(data) <- "list"
    data <- data[cols]
    data[["lon"]] <- lon_lat[, 1]
    data[["lat"]] <- lon_lat[, 2]
    class(data) <- c("tbl_df", "tbl", "data.frame")
    attr(data, "row.names") <- data_attr[["row.names"]]
    as.track_frame(data, time_col = time_col, lon_col = "lon", lat_col = "lat", id_col = id_col)
}


str(data_attr)

attributes(data)

str(data)


, lon_col, lat_col, alt_col, 

#
# convert move
#


