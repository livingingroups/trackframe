# Package index

## Functions

All `trackframe` package functions.

- [`trackframe()`](as_trackframe.md)
  [`as.trackframe()`](as_trackframe.md) :

  Convert an object to a `trackframe`

- [`calculate_utm_zone_crs()`](calculate_utm.md) : Calculate which utm
  zones are appropreate for input data

- [`cbind(`*`<trackframe>`*`)`](cbind.trackframe.md) : Combine
  trackframes by columns

- [`check_trackframe()`](check_trackframe.md)
  [`assert_trackframe()`](check_trackframe.md)
  [`expect_trackframe()`](check_trackframe.md) : Checkmate style check
  if x is a trackframe object

- [`cocomo_as_tf()`](cocomo_as_tf.md) : Converts a cocomo object to a
  trackframe

- [`crs()`](crs.md) [`` `crs<-`() ``](crs.md) : Extract or Assign from a
  Track Frame Coordinate Reference System

- [`crs_type()`](crs_type.md) : Extract Coordinate Reference System Type
  from a Track Frame

- [`deduping()`](deduping.md) : Deduping of trackframe

- [`df_demo`](df_demo.md) : Artificial Travel Path

- [`guess_all_cols()`](guess_all_cols.md) : Guesses columns

- [`merge(`*`<trackframe>`*`)`](merge.trackframe.md) : Merge Two
  trackframes

- [`plot(`*`<trackframe>`*`)`](plot.trackframe.md) : Plot trackframes

- [`rbind(`*`<trackframe>`*`)`](rbind.trackframe.md) : Combine
  trackframes by rows

- [`select_id()`](select_id.md) : Select Tracks by ID from a Track Frame

- [`sort(`*`<trackframe>`*`)`](sort.trackframe.md) : Sorting of
  Trackframes

- [`split_by_id()`](split_by_id.md) : Split trackframe by ID

- [`suggest_utm_zone_crs()`](suggest_utm.md) : Suggest a utm crs

- [`tf_as_sf()`](tf_as.md) [`tf_as_sftrack()`](tf_as.md)
  [`tf_as_move2()`](tf_as.md) : Convert a Track Frame to Simple Features
  (sf) Object

- [`tf_as_cocomo()`](tf_as_cocomo.md) :

  Convert a `track_frame` into the `cocomo` format

- [`tf_as_xyt()`](tf_as_xyt.md) : Convert a Track Frame to XYT Format

- [`tf_backtransform()`](tf_backtransform.md) : Backtransform

- [`time_col()`](tf_colnames.md) [`` `time_col<-`() ``](tf_colnames.md)
  [`id_col()`](tf_colnames.md) [`` `id_col<-`() ``](tf_colnames.md)
  [`easting_col()`](tf_colnames.md)
  [`` `easting_col<-`() ``](tf_colnames.md)
  [`northing_col()`](tf_colnames.md)
  [`` `northing_col<-`() ``](tf_colnames.md)
  [`tf_colnames()`](tf_colnames.md)
  [`` `tf_colnames<-`() ``](tf_colnames.md) : Get and set column names
  of Trackframe Key columns

- [`easting()`](tf_coords.md) [`` `easting<-`() ``](tf_coords.md)
  [`northing()`](tf_coords.md) [`` `northing<-`() ``](tf_coords.md) :
  Extract or Assign Trackframe coordinates

- [`id()`](tf_id.md) [`` `id<-`() ``](tf_id.md) : Extract or Assign
  Trackframe Track ID

- [`tf_options()`](tf_options.md) : Options for col guessing in
  trackframe

- [`time(`*`<trackframe>`*`)`](tf_time.md)
  [`` `time<-`() ``](tf_time.md) : Extract or Assign Trackframe Time
  Index

- [`type_arrows()`](type_arrows.md) : Add arrows to a plot

- [`unique_ids()`](unique_ids.md) : Extract Unique IDs from a Track
  Frame

## Datasets

Simulated datasets for testing trackframe.

- [`path_data_frame`](path_data_frame.md) : Simulated Travel Path
- [`path_matrix`](path_matrix.md) : Simulated Travel Path
- [`path_move2`](path_move2.md) : Simulated Travel Path
- [`path_sftrack`](path_sftrack.md) : Simulated Travel Path
- [`path_trackframe`](path_trackframe.md) : Simulated Travel Path
- [`paths_data_frame`](paths_data_frame.md) : Simulated Travel Path with
  multiple IDs
- [`paths_matrix`](paths_matrix.md) : Simulated Travel Path with
  multiple IDs
- [`paths_move2`](paths_move2.md) : Simulated Travel Path with multiple
  IDs
- [`paths_sftrack`](paths_sftrack.md) : Simulated Travel Path with
  multiple IDs
- [`paths_trackframe`](paths_trackframe.md) : Simulated Travel Path with
  multiple IDs
- [`df_mini`](df_mini.md) : Simulated Travel Path
- [`move2_mini`](move2_mini.md) : Simulated Travel Path
- [`sftrack_mini`](sftrack_mini.md) : Simulated Travel Path
- [`tf_mini`](tf_mini.md) : Simulated Travel Path with multiple IDs
