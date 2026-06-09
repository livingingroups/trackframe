# Suggest a utm crs

Uses [calculate_utm_zone_crs](calculate_utm.md) to determine the UTM
zone for each datapoint. Returns the most common zone in the dataset.

## Usage

``` r
suggest_utm_zone_crs(lat, lon = NULL)

# S3 method for class 'sf'
suggest_utm_zone_crs(lat, lon = NULL)

# S3 method for class 'numeric'
suggest_utm_zone_crs(lat, lon = NULL)
```

## Arguments

- lat:

  vector of latitudes or an sf object

- lon:

  vector of longitudes (empty in case x is an sf object)

## Value

crs corresponding to the utm zone that the most data points fall into

## Details

No weighting or averaging is done. Simply the zone that the most of
points fall into. Arbitrary in case of a tie. Future versions may use a
different (better) methodology to chose a zone when points fall into
multiple zones.

## Examples

``` r
suggest_utm_zone_crs(
  lat = c(47.6839952, 47.6839941, 47.6839939, 38.5382329, 38.5382306),
  lon = c(9.175119, 9.17498, 9.175254, -121.764227, -121.764351)
)
#> [1] 32632

suggest_utm_zone_crs(
  trackframe::move2_mini
)
#> [1] 32731
```
