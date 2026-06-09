# Split trackframe by ID

This function splits a `trackframe` object into a list of trackframes by
the id.

## Usage

``` r
split_by_id(tf)
```

## Arguments

- tf:

  A `trackframe` object containing the tracking data. Must have an
  attribute indicating the track ID column.

## Value

an object of class `list_of_trackframes` containing a `trackframe` in
each list element.

## Examples

``` r
tf_split <- split_by_id(tf_mini)
class(tf_split)
#> [1] "list_of_trackframes"
```
