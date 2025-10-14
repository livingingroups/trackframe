# trackframe

## Overview

The **trackframe** package simplifies the process of supporting multiple common formats of animal movement track data as function inputs. Animal movement track data (henceforth, animal tracks) refers to any data that consists of a time series of location points. This package is intended for developers who want to write functions and packages that manipulate and analyze animal tracks, and that has easy integration with other R packages that are commonly used in this field (e.g., move2, sf, etc). Explore the benefits of **trackframe** over other packages [here](#{Why trackframe}).

## Installation

[code chunk to show installation calls... this seems to be a pretty universal section of R package readmes]

## Getting started

Refer to the following vignettes:

-   Quick start guide: Converting to and from a `trackframe` object, assuring that coordinate projections are properly maintained

-   Advanced: ...

-   Performance: Reasons to use trackframe

## Features

The **trackframe** package provides the `trackframe` object as well as simple universal methods to convert to and from other common animal track data types, including move2, sftrack, vanilla dataframe, and matrix formats. `trackframe` objects store one location per row, with columns for `easting`, `northing`, `time`, and `id`. Coordinates are stored in UTM easting/northing format with a given [EPSG](https://epsg.io/) zone. Relevant information about all columns are set and stored as attributes. All columns can be easily accessed by accessor functions, and the `trackframe` itself is manipulatable in the same way as any other dataframe object. The **trackframe** package also provides simple functions for extracting relevant columns, regardless of what the use has named them.

### Example

Suppose you are writing a simple xyt function to get the average speed over time of an animal track:

```         
average_speed_over_time <- function(x, y, t) {
  t <- as.numeric(t)
  weighted.mean(
  sqrt(diff(x)^2 + diff(y)^2) / diff(t),
  diff(t),
  na.rm = TRUE
  )
}
```

If you would like apply this function to multiple animal tracks, and you would like your code to work for different types of track input (e.g., move2, sftrack, and non-sf dataframe objects), then you can use **trackframe** to facilitate this functionality. With **trackframe**, you can first convert your input to a `trackframe` object, and refer to relevant columns without directly referencing their names. The above function would thus look like this:

```         
library(trackframe)

#convert whatever representation of the data that you have to a trackframe object
tf <- as.trackframe(data, id_col = 'animal_id')

#split the trackframe object by unique animal id
tf_split <- split_by_id(tf)

# loop through each unique id and calculate each animal's average speed
output <- c(NA)

for (i in seq_along(unique(id(tf)))) {
  tf_subset <- select_id(tf, unique(id(tf))[i])
  output[i] <- average_speed_over_time(easting(tf_subset), northing(tf_subset), time(tf_subset))
}
```

Without **trackframe**, you would need to write your function such that it checks the input class, extracts the relevant columns (using class-specific functions and object-specific names), and then applies the average speed calculation. You would therefore probably end up with something like this:

```         
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

**trackframe** thus simplifies the development of functions and packages that manipulate and analyze animal tracks, enabling the accommodation of multiple types of animal track data. For a more detailed comparison of trackframe and non-trackframe methods, see [[comparison vignette]].

## Why **trackframe**?

**trackframe** is designed to be complementary to more fully featured geospatial libraries such as `sf`. It offers a flexible object (`trackframe`) for representing animal tracks with an underlying `data.frame`, `data.table`, or `tibble` object where the coordinates, time, id, etc., data are stored in columns. Using the `as.trackframe()` function, multiple input objects can be easily coerced to a `trackframe` object, including move2, sftrack, vanilla dataframes and matrices. It is also easy to backtransform `trackframe` objects to their original data type using the `tf_backgransform()` function. In a `trackframe` object, relevant information, including which columns contain coordinate, time, and id values, are stored as attributes an so these columns can be easily accessed by the `easting()`, `northing()`, `time()`, and `id()` functions respectively. Coordinates are always stored in UTMs, and are thus projected into Euclidean space. Because coordinates are stored as columns (`easting` and `northing`), it is easy to perform vectorized R operations directly on these coordinates. **trackframe** also makes it easy to deal with multiple individuals, recognizing that different values of `id` represent completely separate tracks. Finally, **trackframe** is fast [[see performance vignette]].

### Why non-`sf` based?

**trackframe** is designed to be complementary to more full featured geospatial libraries such as **sf**. The **sf** library uses a geometry list column to store Euclidean or Haversine coordinates. This allows **sf** to support many geometry types (`LINESTRING`, `POLYGON` etc.) and facilitates interfacing with non-R geospatial/geometry programs. However, it does not allow the user to perform vectorized R operations directly on the coordinates. It also requires the user to explicitly check and possibly convert coordinate systems. In contrast, **trackframe** is focused on supporting analyses that operate on the geometry and timing of the x, y, t data itself, rather than how the track is embedded in other geospatial systems. To facilitate efficient R code for these use cases, `trackframe` objects store Euclidean coordinates directly as dataframe columns.

## Extra stuff that i'm not sure where to put

The S3 method `as.trackframe` (currently available for `data.frame`, `sftrack` and `move2`) automatically transforms coordinates to UTM easting/northing. `time_col`, `easting_col`, `northing_col` and `id_col` are stored as attributes.
