# Extract or Assign Trackframe Time Index

This function retrieves or updates the time index values from a
`trackframe` object. The `trackframe` has a data frame-like structure
with an attribute specifying the column containing time index values.

## Usage

``` r
# S3 method for class 'trackframe'
time(x, ...)

time(x) <- value

# S3 method for class 'trackframe'
time(x) <- value
```

## Arguments

- x:

  A `trackframe` object containing the tracking data. Must have an
  attribute indicating the time column (`time`).

- ...:

  ...

- value:

  vector of POSIX timestamps with length nrow(x)

## Value

A vector of time index values extracted from the `trackframe`.

## Examples

``` r
time(tf_mini)
#>  [1] "2025-10-14 13:48:46 UTC" "2025-10-14 13:49:46 UTC"
#>  [3] "2025-10-14 14:15:46 UTC" "2025-10-14 14:16:46 UTC"
#>  [5] "2025-10-14 14:17:46 UTC" "2025-10-14 13:48:46 UTC"
#>  [7] "2025-10-14 13:52:46 UTC" "2025-10-14 14:22:46 UTC"
#>  [9] "2025-10-14 14:23:46 UTC" "2025-10-14 13:48:46 UTC"
#> [11] "2025-10-14 13:49:46 UTC"
time(tf_mini) <- seq_along(nrow(tf_mini))
```
