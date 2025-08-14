# Package Trackframe

The `trackframe` package simplifies the process of supporting multiple common formats of animal movement data as inputs analysis algorithms.

This is achieved by providing the `trackframe` object, a non-sf dataframe object along with methods to convert to and from move2, sftrack, vanilla dataframe, and matrix formats. Coordinates are stored in UTM easting/northing format with a given epsg time zone. Informations about the columns in which `time`, `id`, `easting`, `northing` are stored, are set with attributes. These columns can be easily accessed by accessor functions.

## Example Use

Suppose you are writing a simple xyt function like average speed over time:

```{r}
average_speed_over_time <- function(x, y, t)  weighted.mean(
  t <- as.numeric(t)
  sqrt(diff(x)^2 + diff(y)^2) / diff(t),
  diff(t)
)
```

You would like apply this function to multiple animal tracks, and you would like your code to work for different representations of the tracks: move2, sftrack, and non-sf dataframe. You will probably end up with something like this:
```{r}
# Assign variables based on input data format
if ('sf' %in% class(data)) {
  coords <- sf::st_coordinates(sf::st_transform(data, 3857))
  if ('sftrack' %in% class(data)) {
    track_id <- sapply(data[[attr(data, "group_col")]], deparse)
    timestamp <- data[[attr(data, 'time_col')]]
  } else if ('move2' %in% class(data)) {
    track_id <- move2::mt_track_id(data)
    timestamp <- move2::mt_time(data)
  }
} else {
  coords <- as.matrix(data[,c('x', 'y')])
  track_id <- data$animal_id
  timestamp <- data$t
}

# Calculate average speed for each track
sapply(
  unique(track_id),
  function(focal_track_id){
    idx <- which(track_id == focal_track_id)
    average_speed_over_time(coords[idx,1], coords[idx,2], timestamp[idx])
  }
)
```

With trackframe, this is simplified to:

```{r}
library(trackframe)

tf <- as.trackframe(data, id_col = 'animal_id')
s <- sapply(split(tf, id(tf)), function(tf){
  average_speed_over_time(easting(tf), northing(tf), time(tf))
})
```

Running example in quickstart vignette TODO: link

## Why non-`sf` based?

Trackframe is designed to be complementary to more full featured geospatial libraries such as `sf`.In particular `trackframe` is focused on supporting analyses that operate on the geometry and timing of x, y, t animal movement path itself rather than how the path is embedded in other geospatial systems. To facilitate efficient R code for these usecases, `trackframe` objects store euclidean coordinates directly as dataframe columns. In contrast, sf uses a geometry list column that could be in euclidean or haversine coordinates. This allows `sf` to support many geometry types (`LINESTRING`, `POLYGON` etc.) that are not supported by trackframe and facilitates interfacing with non-R geospatial/geometry programs. However, it does not allow the user to perform vectorized R operations on directly on the coordinates. It also requires the user to explicitly check and possibly convert coordinate systems.
