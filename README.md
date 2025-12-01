# trackframe

## Overview

The trackframe package simplifies the process of supporting multiple common representations of animal track data as function inputs. Animal track data refers to any data that consists of a time series of coordinates that represent sequential locations of a moving entity. This package is intended for developers who want to write functions and packages that manipulate and analyze animal tracks, and that has easy integration with other R packages that are commonly used in this field (e.g., move2, sf, etc). Explore the benefits of trackframe over other packages here: [Why trackframe?](comparison.html)

## Installation

``` r
install.packages("trackframe")

library(trackframe)
```

## Getting started

Refer to the following vignettes:

<!--- We can update these links to be the actual web addresses once we know them... For now, I left referential because it should allow to click between them on local version (i think) --->

-   [Why trackframe?](comparison.html): Learn about the advantages of using trackframe over non-trackframe approaches to writing functions for manipulating and analyzing animal tracks.

-   [Quickstart guide](trackframe.html): Learn how to create `trackframe` objects, convert to and from other common representations of animal track data, and manipulate and subset columns of a `trackframe` object.

-   [Identifying the Key Columns](identifying_columns.html): Learn about how `as.trackframe` identifies the columns representing time, coordinates, and animal id, and how you can configure or override the sources it uses.

-   [Compatible Coordinate Systems](crs.html): Learn about the coordinate systems that are compatible with trackframe, and how to set the crs of a `trackframe` object.

## Features

The **trackframe** package provides the `trackframe` object as well as simple universal methods to convert to and from other common animal track data representations, including `move2`, `sftrack`, `data.frame`, and `matrix` object formats. 
`trackframe` objects store one location per row, with columns for `easting`, `northing`, `time`, and `id`. 
Coordinates are stored in UTM easting/northing format with a given [EPSG](https://epsg.io/) zone. 
Relevant information about all columns are set and stored as attributes. 
All columns can be easily accessed by accessor functions, and the `trackframe` itself is manipulatable in the same way as any other dataframe object. 
The **trackframe** package also provides simple functions for extracting relevant columns, regardless of what the use has named them.

For more details about advantages of using trackframe over non-trackframe approaches to writing functions for manipulating and analyzing animal tracks, see [Why trackframe?](comparison.html)

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

If you would like apply this function to multiple animal tracks, and you would like your code to work for different types of track input (e.g., `move2`, `sftrack`, and non-sf `data.frame` objects), then you can use trackframe to facilitate this functionality. With trackframe, you can first convert your input to a `trackframe` object, and refer to relevant columns without directly referencing their names. The above function would thus look like this:

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

Without trackframe, you would need to write your function such that it checks the input class, extracts the relevant columns (using class-specific functions and object-specific names), and then applies the average speed calculation. You would therefore probably end up with something like this:

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

Trackframe thus simplifies the development of functions and packages that manipulate and analyze animal tracks, enabling the accommodation of multiple representations of animal track data. For a more detailed comparison of trackframe and non-trackframe methods, see [Why trackframe?](comparison.html)
