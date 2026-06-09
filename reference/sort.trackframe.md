# Sorting of Trackframes

Sort a trackframe into ascending or descending order, by track ID and
time. If no id column is available only sorted by time.

## Usage

``` r
# S3 method for class 'trackframe'
sort(x, decreasing = FALSE, ...)
```

## Arguments

- x:

  an object of class trackframe

- decreasing:

  logical indicating if sort should be increasing or decreasing.

- ...:

  additional arguments passed to order

## Value

an object of class trackframe

## Examples

``` r
sort(
  as.trackframe(
    data.frame(
      x = 1:6,
      y = 1:6,
      t = as.POSIXct(c(2, 2, 2, 1, 1, 1)),
      id = c("Asa", "Betty", "Charlie", "Asa", "Betty", "Charlie")
    ),
    crs = NA
  )
)
#>   x y                   t      id
#> 4 4 4 1970-01-01 00:00:01     Asa
#> 1 1 1 1970-01-01 00:00:02     Asa
#> 5 5 5 1970-01-01 00:00:01   Betty
#> 2 2 2 1970-01-01 00:00:02   Betty
#> 6 6 6 1970-01-01 00:00:01 Charlie
#> 3 3 3 1970-01-01 00:00:02 Charlie
```
