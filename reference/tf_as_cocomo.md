# Convert a `track_frame` into the `cocomo` format

This function converts a `track_frame` object into the cocomo format.

## Usage

``` r
tf_as_cocomo(tf)
```

## Arguments

- tf:

  an object inheriting from `track_frame`.

## Value

A list with three components:

- x:

  A matrix of x-coordinates (easting values). If tf has no id attribute,
  this is a single-column matrix. If tf has ids, rows represent
  different tracks and columns represent time points.

- y:

  A matrix of y-coordinates (northing values). Same structure as x
  matrix.

- t:

  A vector of time values, sorted in ascending order.

## Examples

``` r
tf_as_cocomo(tf_mini)
#> $xs
#>            [,1]     [,2]    [,3]     [,4]     [,5]     [,6]    [,7]     [,8]
#> track_1 16.3725 16.37334      NA 16.37334 16.37319 16.37328      NA       NA
#> track_2 16.3725       NA 16.3725       NA       NA       NA 16.3725 16.37231
#> track_3 16.3725 16.37313      NA       NA       NA       NA      NA       NA
#> 
#> $ys
#>             [,1]     [,2]     [,3]     [,4]    [,5]     [,6]     [,7]     [,8]
#> track_1 48.20835 48.20891       NA 48.20891 48.2082 48.20852       NA       NA
#> track_2 48.20835       NA 48.20835       NA      NA       NA 48.20835 48.20921
#> track_3 48.20835 48.20935       NA       NA      NA       NA       NA       NA
#> 
#> $t
#> [1] "2025-10-14 13:48:46 UTC" "2025-10-14 13:49:46 UTC"
#> [3] "2025-10-14 13:52:46 UTC" "2025-10-14 14:15:46 UTC"
#> [5] "2025-10-14 14:16:46 UTC" "2025-10-14 14:17:46 UTC"
#> [7] "2025-10-14 14:22:46 UTC" "2025-10-14 14:23:46 UTC"
#> 
#> $ids
#>   id_code
#> 1 track_1
#> 2 track_2
#> 3 track_3
#> 
```
