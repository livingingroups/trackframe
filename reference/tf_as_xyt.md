# Convert a Track Frame to XYT Format

This function extracts the core spatial-temporal data from a trackframe
object, returning a simplified data frame with just the easting (x),
northing (y), time (t), and optionally the track ID columns.

## Usage

``` r
tf_as_xyt(x, ...)
```

## Arguments

- x:

  A `trackframe` object containing the tracking data.

- ...:

  other arguments passed to coredata

## Value

A data frame with the easting, northing, time index, and optionally
track ID columns.

## Examples

``` r
tf_as_xyt(tf_mini)
#>     easting northing                time      id
#> 1  16.37250 48.20835 2025-10-14 13:48:46 track_1
#> 2  16.37334 48.20891 2025-10-14 13:49:46 track_1
#> 3  16.37334 48.20891 2025-10-14 14:15:46 track_1
#> 4  16.37319 48.20820 2025-10-14 14:16:46 track_1
#> 5  16.37328 48.20852 2025-10-14 14:17:46 track_1
#> 6  16.37250 48.20835 2025-10-14 13:48:46 track_2
#> 7  16.37250 48.20835 2025-10-14 13:52:46 track_2
#> 8  16.37250 48.20835 2025-10-14 14:22:46 track_2
#> 9  16.37231 48.20921 2025-10-14 14:23:46 track_2
#> 10 16.37250 48.20835 2025-10-14 13:48:46 track_3
#> 11 16.37313 48.20935 2025-10-14 13:49:46 track_3
```
