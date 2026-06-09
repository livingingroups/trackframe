# Combine trackframes by columns

Take a sequence of trackframes, data.frame, data.table, tibble to
combine columns.

## Usage

``` r
# S3 method for class 'trackframe'
cbind(...)
```

## Arguments

- ...:

  objects of class trackframe. Other R objects may be coerced as
  appropriate.

## Value

an object of class trackframe

## Examples

``` r
tf1 <- trackframe::tf_mini
tf2 <- tf1
tf2$id2 <- "A"
tf1_tf2 <- cbind(tf1, tf2)
tf1_tf2
#>                   time northing  easting      id              time_1 northing_1
#> 1  2025-10-14 13:48:46 48.20835 16.37250 track_1 2025-10-14 13:48:46   48.20835
#> 2  2025-10-14 13:49:46 48.20891 16.37334 track_1 2025-10-14 13:49:46   48.20891
#> 3  2025-10-14 14:15:46 48.20891 16.37334 track_1 2025-10-14 14:15:46   48.20891
#> 4  2025-10-14 14:16:46 48.20820 16.37319 track_1 2025-10-14 14:16:46   48.20820
#> 5  2025-10-14 14:17:46 48.20852 16.37328 track_1 2025-10-14 14:17:46   48.20852
#> 6  2025-10-14 13:48:46 48.20835 16.37250 track_2 2025-10-14 13:48:46   48.20835
#> 7  2025-10-14 13:52:46 48.20835 16.37250 track_2 2025-10-14 13:52:46   48.20835
#> 8  2025-10-14 14:22:46 48.20835 16.37250 track_2 2025-10-14 14:22:46   48.20835
#> 9  2025-10-14 14:23:46 48.20921 16.37231 track_2 2025-10-14 14:23:46   48.20921
#> 10 2025-10-14 13:48:46 48.20835 16.37250 track_3 2025-10-14 13:48:46   48.20835
#> 11 2025-10-14 13:49:46 48.20935 16.37313 track_3 2025-10-14 13:49:46   48.20935
#>    easting_1    id_1 id2
#> 1   16.37250 track_1   A
#> 2   16.37334 track_1   A
#> 3   16.37334 track_1   A
#> 4   16.37319 track_1   A
#> 5   16.37328 track_1   A
#> 6   16.37250 track_2   A
#> 7   16.37250 track_2   A
#> 8   16.37250 track_2   A
#> 9   16.37231 track_2   A
#> 10  16.37250 track_3   A
#> 11  16.37313 track_3   A
class(tf1_tf2)
#> [1] "trackframe" "data.frame"
```
