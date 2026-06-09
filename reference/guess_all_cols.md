# Guesses columns

Guesses columns

## Usage

``` r
guess_all_cols(
  col_names,
  time_col_candidates = tf_options("time_col"),
  easting_col_candidates = tf_options("easting_col"),
  northing_col_candidates = tf_options("northing_col"),
  id_col_candidates = tf_options("id_col")
)
```

## Arguments

- col_names:

  vector of column names of the input data

- time_col_candidates:

  a vector of candidates for time column. Typically provided in
  tf_options("time_col").

- easting_col_candidates:

  a vector of candidates for easting column. Typically provided in
  tf_options("easting_col").

- northing_col_candidates:

  a vector of candidates for northing column. Typically provided in
  tf_options("northing_col").

- id_col_candidates:

  a vector of candidates for id column. Typically provided in
  tf_options("id_col").

## Value

a list of guesses

## Examples

``` r
data("path_trackframe")
path_trackframe$time_col <- path_trackframe$time
guess_all_cols(colnames(path_trackframe))
#> $time_col
#> [1] "time"     "time_col"
#> 
#> $easting_col
#> [1] "easting"
#> 
#> $northing_col
#> [1] "northing"
#> 
#> $id_col
#> [1] "id"
#> 
```
