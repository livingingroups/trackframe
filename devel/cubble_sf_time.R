options(width = 120)
#
# cubble
#
# install.packages("cubble")
library("cubble")

data(package = "cubble")

d <- cubble(
  id = rep(c("perth", "melbourne", "sydney"), each = 3),
  date = rep(as.Date("2020-01-01") + 0:2, times = 3),
  long = rep(c(115.86, 144.96, 151.21), each = 3),
  lat = rep(c(-31.95, -37.81, -33.87), each = 3),
  value = rnorm(n = 9),
  key = id, index = date, coords = c(long, lat)
)

str(attributes(d))
str(unclass(d))

make_cubble(spatial = stations, temporal = meteo,
            key = id, index = date, coords = c(long, lat))



ts_nested <- make_cubble(spatial = stations, temporal = meteo_ts, coords = c(long, lat))
(ts_long <- face_temporal(ts_nested))
ts_long
attributes(ts_long)


#
#
#
# install.packages("sftime")
library("sftime")
# Ist wie sftrack nur dass es kein group_col argument welches den identifier
# des individums angibt.
climate_aus <- cubble::climate_aus

# convert to sftime
climate_aus_sftime <- st_as_sftime(climate_aus[1:4, ])
climate_aus_sftime
glimpse(attributes(climate_aus_sftime), width = 120)
# List of 6
#  $ names      : chr [1:11] "id" "long" "lat" "elev" ...
#  $ row.names  : int [1:1464] 1 2 3 4 5 6 7 8 9 10 ...
#  $ sf_column  : chr "geometry"
#  $ agr        : Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA
#   ..- attr(*, "names")= chr [1:10] "id" "long" "lat" "elev" ...
#  $ class      : chr [1:3] "sftime" "sf" "data.frame"
#  $ time_column: chr "date"



#
# convert sftrack
#
library("sftrack")

data("raccoon")
d <- head(raccoon[!is.na(raccoon$latitude), ])
obj_sftrack <- as_sftrack(d,
                          coords = c("longitude", "latitude"),
                          group = c(id = "animal_id"),
                          time = "timestamp")
class(obj_sftrack)
str(attributes(obj_sftrack))
# List of 8
#  $ names    : chr [1:10] "animal_id" "latitude" "longitude" "timestamp" ...
#  $ row.names: int [1:6] 2 5 6 7 8 10
#  $ class    : chr [1:3] "sftrack" "sf" "data.frame"
#  $ sf_column: chr "geometry"
#  $ agr      : Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA
#   ..- attr(*, "names")= chr [1:9] "animal_id" "latitude" "longitude" "timestamp" ...
#  $ group_col: chr "sft_group"
#  $ time_col : chr "timestamp"
#  $ error_col: logi NA


library("move2")

fisher_data <- mt_read(mt_example())
class(fisher_data)
str(fisher_data)
str(attributes(fisher_data))
# List of 10
#  $ names          : chr [1:21] "event-id" "visible" "timestamp" "behavioural-classification" ...
#  $ row.names      : int [1:47347] 1 2 3 4 5 6 7 8 9 10 ...
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
colnames(fisher_data)
m5 <- fisher_data[fisher_data[["individual-local-identifier"]] == "M5", ]
attr(m5, "track_data")


#
# tsibble
#
# install.packages("nycflights13")
library("dplyr")
library("tsibble")
weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
colnames(weather)
weather_tsbl <- as_tsibble(weather, key = origin, index = time_hour)
colnames(weather_tsbl)
str(attributes(weather_tsbl))
# List of 7
#  $ class    : chr [1:4] "tbl_ts" "tbl_df" "tbl" "data.frame"
#  $ row.names: int [1:26115] 1 2 3 4 5 6 7 8 9 10 ...
#  $ names    : chr [1:5] "origin" "time_hour" "temp" "humid" ...
#  $ key      : tibble [3 × 2] (S3: tbl_df/tbl/data.frame)
#   ..$ origin: chr [1:3] "EWR" "JFK" "LGA"
#   ..$ .rows : list<int> [1:3] 
#   .. ..$ : int [1:8703] 1 2 3 4 5 6 7 8 9 10 ...
#   .. ..$ : int [1:8706] 8704 8705 8706 8707 8708 8709 8710 8711 8712 8713 ...
#   .. ..$ : int [1:8706] 17410 17411 17412 17413 17414 17415 17416 17417 17418 17419 ...
#   .. ..@ ptype: int(0) 
#   ..- attr(*, ".drop")= logi TRUE
#  $ index    : chr "time_hour"
#   ..- attr(*, "ordered")= logi TRUE
#  $ index2   : chr "time_hour"
#  $ interval : interval [1:1] 1h
#   ..@ .regular: logi TRUE


#
# trajectories
#
# options(timeout = Inf)
# install.packages("taxidata", repos = "https://cran.uni-muenster.de/pebesma", type = "source")
library("trajectories")
library("taxidata")
Beijing <- taxidata
Beijing <- Beijing[1:2000]
class(Beijing)

Z <- lapply(X = 1:length(Beijing), function(i) {
    q <- cut(Beijing[[i]], "day", touch = F)
    return(q@tracks[[3]])
})

plot(Z[[21]], xlim=c(420000, 470000), ylim=c(4390000, 4455000), lwd=2)
plot(Z[[26]], add=T, col="orange", lwd=2)
plot(Z[[20]], add=T, col=2, lwd=2)
plot(Z[[12]], add=T, col=3, lwd=2)
plot(Z[[15]], add=T, col=4, lwd=2)


tracks1 <- Tracks(list(Beijing[[1]], Beijing[[2]]))
tracks2 <- Tracks(list(Beijing[[2]], Beijing[[1]]))
dists(tracks1, tracks2,mean)
plot(meandist,type="l",lwd=2,cex.axis=1.7,cex.lab=1.7)


