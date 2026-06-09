# Extract or Assign Trackframe Track ID

This function retrieves the track ID values from a `trackframe` object.
The `trackframe` has a data frame-like structure with an attribute
specifying the column containing track ID values.

## Usage

``` r
id(tf)

id(tf) <- value
```

## Arguments

- tf:

  A `trackframe` object containing the tracking data. Must have an
  attribute indicating the track ID column (`id`).

- value:

  new values for id column

## Value

A vector of track ID values extracted from the `trackframe`. NULL if no
id column is configured.

## Examples

``` r
id(tf_mini)
#>  [1] "track_1" "track_1" "track_1" "track_1" "track_1" "track_2" "track_2"
#>  [8] "track_2" "track_2" "track_3" "track_3"
id(tf_mini) <- rep("new_trackname", nrow(tf_mini))
```
