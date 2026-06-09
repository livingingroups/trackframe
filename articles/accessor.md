# Accessing and Modifying Key Columns

## Accessing and Modifying Key Columns

### Setup

Starting with our df_mini, let’s rename columns to truly custom names.

``` r

library(trackframe)

df <- trackframe::df_mini

df
```

    ##                  time      northing       easting      id
    ## 1 2025-10-14 13:48:46  0.000000e+00  0.0000000000 track_1
    ## 2 2025-10-14 13:49:46 -6.375425e-05 -0.0009523287 track_1
    ## 3 2025-10-14 13:50:46 -3.326043e-04 -0.0012222081 track_1
    ## 4 2025-10-14 13:51:46 -6.402753e-04 -0.0020070494 track_1
    ## 5 2025-10-14 13:52:46 -2.096415e-04 -0.0029277994 track_1

``` r

colnames(df) <- c('when', 'up', 'right', 'who')
```

Now let’s convert this dataframe to a `trackframe` object, telling
`as.trackframe` which columns are the key columns.

``` r

tf <- as.trackframe(
  df,
  time_col = 'when',
  easting_col = 'right',
  northing_col = 'up',
  id_col = 'who',
  crs = NA
)
tf
```

    ##                  when            up         right     who
    ## 1 2025-10-14 13:48:46  0.000000e+00  0.0000000000 track_1
    ## 2 2025-10-14 13:49:46 -6.375425e-05 -0.0009523287 track_1
    ## 3 2025-10-14 13:50:46 -3.326043e-04 -0.0012222081 track_1
    ## 4 2025-10-14 13:51:46 -6.402753e-04 -0.0020070494 track_1
    ## 5 2025-10-14 13:52:46 -2.096415e-04 -0.0029277994 track_1

### Accessing key contents

Without changing our funky column names, we can access the key columns
in a generic way:

``` r

easting(tf)
```

    ## [1]  0.0000000000 -0.0009523287 -0.0012222081 -0.0020070494 -0.0029277994

``` r

northing(tf)
```

    ## [1]  0.000000e+00 -6.375425e-05 -3.326043e-04 -6.402753e-04 -2.096415e-04

``` r

time(tf)
```

    ## [1] "2025-10-14 13:48:46 UTC" "2025-10-14 13:49:46 UTC"
    ## [3] "2025-10-14 13:50:46 UTC" "2025-10-14 13:51:46 UTC"
    ## [5] "2025-10-14 13:52:46 UTC"

``` r

id(tf)
```

    ## [1] "track_1" "track_1" "track_1" "track_1" "track_1"

### Modifying key columns

We can also update the contents of columns using these same functions:

``` r

time(tf) <- time(tf) + as.difftime(1, unit = 'days')
tf$when # sucessfully updated
```

    ## [1] "2025-10-15 13:48:46 UTC" "2025-10-15 13:49:46 UTC"
    ## [3] "2025-10-15 13:50:46 UTC" "2025-10-15 13:51:46 UTC"
    ## [5] "2025-10-15 13:52:46 UTC"

### Accessing key column names

If we need to access the column names, we can do that with the \*\_col
functions.

``` r

time_col(tf)
```

    ## [1] "when"

``` r

easting_col(tf)
```

    ## [1] "right"

``` r

northing_col(tf)
```

    ## [1] "up"

``` r

id_col(tf)
```

    ## [1] "who"

Alternatively, we can use the `tf_colnames` function. Note: this will
just return the names of key columns, not any other columns in the
trackframe. The vector returned by `tf_colnames` also itself has names
indicating which key column is which.

``` r

tf$why <- "just because"

colnames(tf)
```

    ## [1] "when"  "up"    "right" "who"   "why"

``` r

tf_colnames(tf)
```

    ##     time northing  easting       id 
    ##   "when"     "up"  "right"    "who"

### Modifying key column names

[`tf_colnames()`](../reference/tf_colnames.md) and the individual
`*_col()` functions can be used to change the column names as well.

``` r

id_col(tf) <- "track_id"

tf_colnames(tf) # updated
```

    ##       time   northing    easting         id 
    ##     "when"       "up"    "right" "track_id"

``` r

tf_colnames(tf)[["time"]] <- "timestamp"
tf_colnames(tf) # updated
```

    ##        time    northing     easting          id 
    ## "timestamp"        "up"     "right"  "track_id"

### Changing which are key columns

What if you want to change which column within the data frame is
identified as a key column. That can be done by rerunning
`as.trackframe` with a new column name. For example:

``` r

# create new columns that we want to be the new coordinates
tf$up_10x <- 10 * tf$up
tf$right_10x <- 10 * tf$right

# up/right and up_10x/right_10x are both part of the trackframe
colnames(tf)
```

    ## [1] "timestamp" "up"        "right"     "track_id"  "why"       "up_10x"   
    ## [7] "right_10x"

``` r

# still same as original
min(easting(tf))
```

    ## [1] -0.002927799

``` r

min(northing(tf))
```

    ## [1] -0.0006402753

``` r

tf_colnames(tf) # also unchanged
```

    ##        time    northing     easting          id 
    ## "timestamp"        "up"     "right"  "track_id"

``` r

# update which columns are used as key columns

tf <- as.trackframe(tf, easting = "right_10x", northing = "up_10x", crs = NA)

# Now the easting and northing values are coming from the *_10x columns
min(easting(tf))
```

    ## [1] -0.02927799

``` r

min(northing(tf))
```

    ## [1] -0.006402753

``` r

# likewise, tf_colnames is updated
tf_colnames(tf)
```

    ##        time          id    northing     easting 
    ## "timestamp"  "track_id"    "up_10x" "right_10x"

``` r

# up and right (1x) columns are still there.
# They are just not identified as the easting and northing columns
colnames(tf)
```

    ## [1] "timestamp" "up"        "right"     "track_id"  "why"       "up_10x"   
    ## [7] "right_10x"

### Accessing and updating crs

[`crs()`](../reference/crs.md) can be used to access and update crs.
[`crs_type()`](../reference/crs_type.md) can

``` r

crs(tf)
```

    ## [1] NA

``` r

crs_type(tf)
```

    ## [1] "nongeoreferenced"

``` r

crs(tf) <- "EPSG:32632"
crs(tf)
```

    ## [1] "EPSG:32632"

``` r

crs_type(tf)
```

    ## [1] "projected"
