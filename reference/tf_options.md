# Options for col guessing in trackframe

Options for col guessing in trackframe

## Usage

``` r
tf_options(option, value)
```

## Arguments

- option:

  a character string name of option. "time_col", "easting_col",
  "northing_col", "id_col", "crs", "sf_easting_col", or
  "sf_northing_col"

- value:

  vector of characters with possible candidates

## Value

a vector of candidates for col guessing

## Examples

``` r
tf_options("time_col", c("t", "timestamp", "time", "time_index",
"time_col", "time_column", "tindex"))
tf_options("easting_col", c("easting", "east", "utm.easting", "easting_col",
"easting_column", "lon", "long", "longitude", "x"))
tf_options("northing_col", c("northing", "north", "utm.northing",
"northing_col", "northing_column", "lat", "latitude", "y"))
tf_options("id_col", c("track_id", "track_id", "trackid", "trackid_col",
"trackid_column", "id"))
tf_options("crs", NA)
```
