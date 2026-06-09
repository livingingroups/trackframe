# Calculate which utm zones are appropreate for input data

Calculate which utm zones are appropreate for input data

## Usage

``` r
calculate_utm_zone_crs(lat, lon = NULL)

# S3 method for class 'sf'
calculate_utm_zone_crs(lat, lon = NULL)

# S3 method for class 'numeric'
calculate_utm_zone_crs(lat, lon = NULL)
```

## Arguments

- lat:

  vector of latitudes or an sf object

- lon:

  vector of longitudes (empty in case x is an sf object)

## Value

vector of utm crs of the same length as input indicating which zone the
data fall into

## Examples

``` r
trackframe::calculate_utm_zone_crs(
  lat = c(47.6839952, 47.6839941, 47.6839939, 38.5382329, 38.5382306),
  lon = c(9.175119, 9.17498, 9.175254, -121.764227, -121.764351)
)
#> [1] 32632 32632 32632 32610 32610

trackframe::calculate_utm_zone_crs(
  trackframe::move2_mini
)
#> [1] 32731 32731 32631 32731 32731
```
