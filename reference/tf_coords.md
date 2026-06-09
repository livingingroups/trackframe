# Extract or Assign Trackframe coordinates

This function retrieves the easting values from a `trackframe` object.
The `trackframe` has a data frame-like structure with an attribute
specifying the column containing easting values.

## Usage

``` r
easting(tf)

easting(tf) <- value

northing(tf)

northing(tf) <- value
```

## Arguments

- tf:

  A `trackframe` object containing the tracking data. Must have an
  attribute indicating the easting column (`easting`).

- value:

  numeric vector of new coordinate values

## Value

A vector of easting or northing values extracted from the `trackframe`.

## Examples

``` r
easting(tf_mini)
#>  [1] 16.37250 16.37334 16.37334 16.37319 16.37328 16.37250 16.37250 16.37250
#>  [9] 16.37231 16.37250 16.37313

easting(tf_mini) <- -easting(tf_mini)
northing(tf_mini)
#>  [1] 48.20835 48.20891 48.20891 48.20820 48.20852 48.20835 48.20835 48.20835
#>  [9] 48.20921 48.20835 48.20935

northing(tf_mini) <- northing(tf_mini)*5
```
