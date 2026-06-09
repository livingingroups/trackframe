# Extract Coordinate Reference System Type from a Track Frame

This function retrieves the crs value from a `trackframe` object.

## Usage

``` r
crs_type(tf)
```

## Arguments

- tf:

  A `trackframe` object containing the tracking data.

## Value

One of: `projected` e.g. utm `nongeoreferenced` designed for use in
captive or simulated scenarios

## Examples

``` r
crs_type(tf_mini)
#> [1] "projected"
```
