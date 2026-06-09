# Extract or Assign from a Track Frame Coordinate Reference System

This function retrieves the crs value from a `trackframe` object.

## Usage

``` r
crs(tf)

crs(tf) <- value
```

## Arguments

- tf:

  A `trackframe` object containing the tracking data.

- value:

  a valid projected crs or NA

## Value

A representation of crs extracted from the `trackframe`. If crs_type is
geographic or projected, then crs is a valid input to sf::st_crs. If
crs_type is nongeoreferenced, then crs can be any non-null value
including NA.

tf with new crs

## Examples

``` r
crs(tf_mini)
#> [1] "EPSG:32632"
crs(tf_mini) <- NA
crs(tf_mini)
#> [1] NA
```
