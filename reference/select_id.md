# Select Tracks by ID from a Track Frame

This function filters a `trackframe` object to include only tracks with
the specified ID(s). It supports selecting a single ID or multiple IDs
simultaneously.

## Usage

``` r
select_id(tf, id)
```

## Arguments

- tf:

  A `trackframe` object containing the tracking data. Must have an
  attribute indicating the track ID column.

- id:

  A character or vector of characters representing the track ID(s) to
  select.

## Value

A filtered `trackframe` containing only the specified track(s).

## Examples

``` r
single_track <- select_id(tf_mini, "track_1")
single_track
#>                  time northing  easting      id
#> 1 2025-10-14 13:48:46 48.20835 16.37250 track_1
#> 2 2025-10-14 13:49:46 48.20891 16.37334 track_1
#> 3 2025-10-14 14:15:46 48.20891 16.37334 track_1
#> 4 2025-10-14 14:16:46 48.20820 16.37319 track_1
#> 5 2025-10-14 14:17:46 48.20852 16.37328 track_1
multiple_tracks <- select_id(tf_mini, c("track_2", "track_3"))
multiple_tracks
#>                   time northing  easting      id
#> 6  2025-10-14 13:48:46 48.20835 16.37250 track_2
#> 7  2025-10-14 13:52:46 48.20835 16.37250 track_2
#> 8  2025-10-14 14:22:46 48.20835 16.37250 track_2
#> 9  2025-10-14 14:23:46 48.20921 16.37231 track_2
#> 10 2025-10-14 13:48:46 48.20835 16.37250 track_3
#> 11 2025-10-14 13:49:46 48.20935 16.37313 track_3
```
