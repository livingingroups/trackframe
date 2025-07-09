# Package Trackframe

## IMPORTANT

Important Information for Developers:

In order to deal with `data.frame`, `data.table` and `tibble` objects at the same time we overwrite the `"[.data.frame"` accessor. Hence, when executing code manually the file `R/subset.R` needs to be sourced.

## Why Trackframe?

We aim to enable fast analysis of animal movement data. This provides an efficient data structure upon which to build analyses for x, y, t, track_id data.

Trackframe is designed to be complementary to more full featured geospatial librariis. For example, `sf` support many geometry types (`LINESTRING`, `POLYGON` etc.) that are not supported by trackframe. To accomidate this flexibility, it uses a geometry list column. This works well when interfacing with non-R geospatial/geometry programs. However, it does not allow the user to perform vectorized R operations on directly on the coordinates.


## TODOs

### Data structure
- We should try the current data structure but evaluate during the development
  if a modified version feels more natural.
  
## Citation
TODO write citation()
