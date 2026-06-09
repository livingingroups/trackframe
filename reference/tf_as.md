# Convert a Track Frame to Simple Features (sf) Object

This function converts a `trackframe` object into a Simple Features (sf)
object, enabling spatial analysis and visualization. The easting and
northing columns are used as coordinates for the sf object.

## Usage

``` r
tf_as_sf(tf, ...)

tf_as_sftrack(tf)

tf_as_move2(tf)
```

## Arguments

- tf:

  A `trackframe` object containing the tracking data. Must have
  attributes specifying the easting and northing columns (`easting` and
  `northing`).

- ...:

  Additional arguments to be passed to `st_as_sf`.

## Value

An sf object representing the spatial data contained in the
`trackframe`.

## Examples

``` r
sf_object <- tf_as_sf(tf_mini)
print(sf_object)
#> Simple feature collection with 11 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 16.37231 ymin: 48.2082 xmax: 16.37334 ymax: 48.20935
#> Projected CRS: WGS 84 / UTM zone 32N
#> First 10 features:
#>                   time      id                  geometry
#> 1  2025-10-14 13:48:46 track_1  POINT (16.3725 48.20835)
#> 2  2025-10-14 13:49:46 track_1 POINT (16.37334 48.20891)
#> 3  2025-10-14 14:15:46 track_1 POINT (16.37334 48.20891)
#> 4  2025-10-14 14:16:46 track_1  POINT (16.37319 48.2082)
#> 5  2025-10-14 14:17:46 track_1 POINT (16.37328 48.20852)
#> 6  2025-10-14 13:48:46 track_2  POINT (16.3725 48.20835)
#> 7  2025-10-14 13:52:46 track_2  POINT (16.3725 48.20835)
#> 8  2025-10-14 14:22:46 track_2  POINT (16.3725 48.20835)
#> 9  2025-10-14 14:23:46 track_2 POINT (16.37231 48.20921)
#> 10 2025-10-14 13:48:46 track_3  POINT (16.3725 48.20835)
```
