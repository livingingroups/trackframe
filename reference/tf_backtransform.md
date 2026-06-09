# Backtransform

Backtransform

## Usage

``` r
tf_backtransform(tf)
```

## Arguments

- tf:

  an object of class `trackframe`

## Value

an object which has been coerced to `trackframe`

## Examples

``` r
library(trackframe)
# for move2
tf <- as.trackframe(data = move2_mini)
tfb <- tf_backtransform(tf)
tfb
#> A <move2> with `track_id_column` "id" and `time_column` "time"
#> Containing 1 track lasting 4 mins in a
#> Simple feature collection with 5 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -0.0003615696 ymin: 0 xmax: 0.0005120919 ymax: 0.001230933
#> Projected CRS: WGS 84 / UTM zone 32N
#>                  time      id                       geometry
#> 1 2025-10-14 13:48:47 track_1                    POINT (0 0)
#> 2 2025-10-14 13:49:47 track_1 POINT (-4.847714e-05 2.8431...
#> 3 2025-10-14 13:50:47 track_1 POINT (0.0005120919 3.69360...
#> 4 2025-10-14 13:51:47 track_1 POINT (-0.0002322027 0.0003...
#> 5 2025-10-14 13:52:47 track_1 POINT (-0.0003615696 0.0012...
#> Track features:
#>        id
#> 1 track_1
move2_mini
#> A <move2> with `track_id_column` "id" and `time_column` "time"
#> Containing 1 track lasting 4 mins in a
#> Simple feature collection with 5 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -0.0003615696 ymin: 0 xmax: 0.0005120919 ymax: 0.001230933
#> Projected CRS: WGS 84 / UTM zone 32N
#>                  time      id                       geometry
#> 1 2025-10-14 13:48:47 track_1                    POINT (0 0)
#> 2 2025-10-14 13:49:47 track_1 POINT (-4.847714e-05 2.8431...
#> 3 2025-10-14 13:50:47 track_1 POINT (0.0005120919 3.69360...
#> 4 2025-10-14 13:51:47 track_1 POINT (-0.0002322027 0.0003...
#> 5 2025-10-14 13:52:47 track_1 POINT (-0.0003615696 0.0012...
#> Track features:
#>        id
#> 1 track_1

# for sftrack
tf <- as.trackframe(data = sftrack_mini)
tfb <- tf_backtransform(tf)
tfb
#> Sftrack with 5 features and 4 fields (0 empty geometries) 
#> Geometry : "geometry" (XY, crs: WGS 84 / UTM zone 32N) 
#> Timestamp : "time" (POSIXct in no timezone) 
#> Groupings : "sft_group" (*id*) 
#> -------------------------------
#>                  time      id     sft_group                       geometry
#> 1 2025-10-14 13:48:47 track_1 (id: track_1)                    POINT (0 0)
#> 2 2025-10-14 13:49:47 track_1 (id: track_1) POINT (-4.847714e-05 2.8431...
#> 3 2025-10-14 13:50:47 track_1 (id: track_1) POINT (0.0005120919 3.69360...
#> 4 2025-10-14 13:51:47 track_1 (id: track_1) POINT (-0.0002322027 0.0003...
#> 5 2025-10-14 13:52:47 track_1 (id: track_1) POINT (-0.0003615696 0.0012...
sftrack_mini
#> Sftrack with 5 features and 4 fields (0 empty geometries) 
#> Geometry : "geometry" (XY, crs: WGS 84 / UTM zone 32N) 
#> Timestamp : "time" (POSIXct in no timezone) 
#> Groupings : "sft_group" (*id*) 
#> -------------------------------
#>                  time      id     sft_group                       geometry
#> 1 2025-10-14 13:48:47 track_1 (id: track_1)                    POINT (0 0)
#> 2 2025-10-14 13:49:47 track_1 (id: track_1) POINT (-4.847714e-05 2.8431...
#> 3 2025-10-14 13:50:47 track_1 (id: track_1) POINT (0.0005120919 3.69360...
#> 4 2025-10-14 13:51:47 track_1 (id: track_1) POINT (-0.0002322027 0.0003...
#> 5 2025-10-14 13:52:47 track_1 (id: track_1) POINT (-0.0003615696 0.0012...

# for data.frame
tf <- as.trackframe(data = df_mini, crs = NA)
tfb <- tf_backtransform(tf)
tfb
#>                  time      northing       easting      id
#> 1 2025-10-14 13:48:46  0.000000e+00  0.0000000000 track_1
#> 2 2025-10-14 13:49:46 -6.375425e-05 -0.0009523287 track_1
#> 3 2025-10-14 13:50:46 -3.326043e-04 -0.0012222081 track_1
#> 4 2025-10-14 13:51:46 -6.402753e-04 -0.0020070494 track_1
#> 5 2025-10-14 13:52:46 -2.096415e-04 -0.0029277994 track_1
df_mini
#>                  time      northing       easting      id
#> 1 2025-10-14 13:48:46  0.000000e+00  0.0000000000 track_1
#> 2 2025-10-14 13:49:46 -6.375425e-05 -0.0009523287 track_1
#> 3 2025-10-14 13:50:46 -3.326043e-04 -0.0012222081 track_1
#> 4 2025-10-14 13:51:46 -6.402753e-04 -0.0020070494 track_1
#> 5 2025-10-14 13:52:46 -2.096415e-04 -0.0029277994 track_1
```
