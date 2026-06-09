# Extract Unique IDs from a Track Frame

This function retrieves the unique track IDs from a `trackframe` object.
The `trackframe` should be a data frame-like structure with an attribute
specifying the column containing track IDs.

## Usage

``` r
unique_ids(tf)
```

## Arguments

- tf:

  A `trackframe` object containing the tracking data. Must have an
  attribute indicating the track ID column (`id`).

## Value

A vector of unique track IDs extracted from the `trackframe`.

## See also

[`as.trackframe()`](as_trackframe.md)

## Examples

``` r
unique_ids(tf_mini)
#> [1] "track_1" "track_2" "track_3"
```
