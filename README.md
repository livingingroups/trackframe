# Package Trackframe

## Why Trackframe?

We aim to enable fast analysis of animal movement data. This provides an efficient data structure upon which to build analyses for x, y, t, track_id data.

Trackframe is designed to be complementary to more full featured geospatial librariis. For example, `sf` support many geometry types (`LINESTRING`, `POLYGON` etc.) that are not supported by trackframe. To accomidate this flexibility, it uses a geometry list column. This works well when interfacing with non-R geospatial/geometry programs. However, it does not allow the user to perform vectorized R operations on directly on the coordinates.

## Example usage

### TODO Actual usage examples

### Converting from x,y,t (cocomo style)

```{r}
# Cocomo Style data
n_ind <- 5
n_t <- 100
ts <- as.POSIXct(1:n_t)
x <- matrix(
  runif(n_ind * n_t, -180, 180),
  n_ind,
  n_t
)
y <- matrix(
  runif(n_ind * n_t, -180, 180),
  n_ind,
  n_t
)

# Conversion
df <- data.frame(
   time = ts,
   lon = as.vector(x),
   lat = as.vector(y),
   id = rep(1:nrow(x), each = ncol(x))
)
coco_tf <- as.track_frame(df, index = "time", lon_col = "lon", lat_col = "lat", id_cols = "id")
```

TODO: converting back to cocomo style

### sf/move2

move2 -> track_frame

```{r}
albatross_tf <- move2::mt_read(move2::mt_example()) |>
  sf::st_transform(3857) |>
  as.track_frame()
```

track_frame -> sf

```{r}
recovered_sf <- tf_to_sf(albatross_tf[!is.na(latitude(albatross_tf)),])
```

## TODOs

### Data structure
- We should try the current data structure but evaluate during the development
  if a modified version feels more natural.
