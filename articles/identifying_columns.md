# Identifying the Key Columns

## Identifying the Key Columns

When creating a `trackframe` object from another animal track data
representation, `as.trackframe` identifies the columns representing time
(`time_col`), coordinates (`northing_col` & `easting_col`), and track id
(`id_col`). It obtains this information from 3 potential sources: 1)
metadata inherited from the input object, 2) arguments supplied directly
to `as.trackframe`, and/or 3) pre-set global package options.

### Load Package

``` r

library(trackframe)
```

### 1) Identifying key columns from inherited information

If the input object given to `as.trackframe` is a…

- a `data.frame`, `data.table`, or `tibble`: no information about the
  (`time_col`), coordinates (`northing_col` & `easting_col`), and track
  id (`id_col`) columns is inherited, all are identified from other
  sources.
- an `sf` object (but not `sftrack` or `move2`): the `easting_col` and
  `northing_col` are identified from the input object’s `geometry`
  column. The `time_col` and `id_col` are identified from other sources.
- an `sftrack` or `move2` object: all 4 of the key columns are
  identifiable from the input object’s attributes.

### 2) Identifying key columns from arguments

To explicitly tell `as.trackframe` which are the key columns, you can
specify the column names that correspond to the `time_col`,
`easting_col`, `northing_col` and `id_col` using the respective
arguments in the `as.trackframe` function. If you want, you can also
give each argument a vector of possible column names; this is useful if
you have an idea of what column names your input object will have, but
there is more than one possibility for any of the key columns.

``` r

# Create example data
df <- data.frame(
  timestamp = rep(as.POSIXct(Sys.time() + 1:5), times = 5),
  x = runif(25, 0, 10),
  y = runif(25, 0, 10),
  id = rep(1:5, each = 5)
)

# Convert to trackframe with one column name per key column given
# in arguments
tf_arg <- as.trackframe(
  df,
  time_col = "timestamp",
  easting_col = "x",
  northing_col = "y",
  id_col = "id",
  crs = NA
)

head(tf_arg)
```

    ##             timestamp          x         y id
    ## 1 2026-06-11 17:04:38 0.80750138 7.4152153  1
    ## 2 2026-06-11 17:04:39 8.34333037 0.5144628  1
    ## 3 2026-06-11 17:04:40 6.00760886 5.3021246  1
    ## 4 2026-06-11 17:04:41 1.57208442 6.9582388  1
    ## 5 2026-06-11 17:04:42 0.07399441 6.8855600  1
    ## 6 2026-06-11 17:04:38 4.66393497 0.3123033  2

``` r

attributes(tf_arg)[1:6]
```

    ## $names
    ## [1] "timestamp" "x"         "y"         "id"       
    ## 
    ## $class
    ## [1] "trackframe" "data.frame"
    ## 
    ## $row.names
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## 
    ## $time
    ## [1] "timestamp"
    ## 
    ## $easting
    ## [1] "x"
    ## 
    ## $northing
    ## [1] "y"

``` r

# Convert to trackframe with multiple possible column names per key
# column given in arguments
tf_arg_2 <- as.trackframe(
  df,
  time_col = c("date_time", "time", "timestamp"),
  easting_col = c("X", "x"),
  northing_col = c("Y", "y"),
  id_col = c("id", "ID"),
  crs = NA
)

head(tf_arg_2)
```

    ##             timestamp          x         y id
    ## 1 2026-06-11 17:04:38 0.80750138 7.4152153  1
    ## 2 2026-06-11 17:04:39 8.34333037 0.5144628  1
    ## 3 2026-06-11 17:04:40 6.00760886 5.3021246  1
    ## 4 2026-06-11 17:04:41 1.57208442 6.9582388  1
    ## 5 2026-06-11 17:04:42 0.07399441 6.8855600  1
    ## 6 2026-06-11 17:04:38 4.66393497 0.3123033  2

``` r

attributes(tf_arg_2)[1:6]
```

    ## $names
    ## [1] "timestamp" "x"         "y"         "id"       
    ## 
    ## $class
    ## [1] "trackframe" "data.frame"
    ## 
    ## $row.names
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## 
    ## $time
    ## [1] "timestamp"
    ## 
    ## $easting
    ## [1] "x"
    ## 
    ## $northing
    ## [1] "y"

This approach works best if you are applying `as.trackframe` directly in
a script to some known input data, or you are building a package where
you know for certain the column names in your user’s input data object.
If you cannot be sure of the column names in the input data, then it is
better to use pre-set global options to identify the key columns.

### 3) Identifying key columns from pre-set global options

To allow the most flexibility, you can have `as.trackframe` identify the
key columns by matching the column names to vectors of pre-set global
options. This is useful if you are running `as.trackframe` in many
places throughout your code and you want to be able to easily change the
vector of possible column names for all occurrences at once. Also, for R
package developers, using `tf_options` instead of passing arguments
directly to `as.trackframe` allows your user to override the choices
that you’ve set, if needed.

By default, the vectors of possible column names are as follows:

``` r

tf_options("time_col")
```

    ## [1] "t"           "timestamp"   "time"        "time_index"  "time_col"   
    ## [6] "time_column" "tindex"      "datetime"

``` r

tf_options("easting_col")
```

    ## [1] "easting"        "east"           "utm.easting"    "easting_col"   
    ## [5] "easting_column" "x"              "utm.x"

``` r

tf_options("northing_col")
```

    ## [1] "northing"        "north"           "utm.northing"    "northing_col"   
    ## [5] "northing_column" "y"               "utm.y"

``` r

tf_options("id_col")
```

    ## [1] "track_id"                    "animal_id"                  
    ## [3] "trackid"                     "trackid_col"                
    ## [5] "trackid_column"              "id"                         
    ## [7] "individual_local_identifier"

You can modify the `tf_options` globally, as follows:

``` r

# e.g., add "tindex2" to the default options for the time_col
tf_options("time_col", c(tf_options("time_col"), "tindex2"))
tf_options("time_col")
```

    ## [1] "t"           "timestamp"   "time"        "time_index"  "time_col"   
    ## [6] "time_column" "tindex"      "datetime"    "tindex2"

``` r

# e.g., replace the default options for the time_col with
# just "date_time", "time" and "timestamp"
tf_options("time_col", c("date_time", "time", "timestamp"))
tf_options("time_col")
```

    ## [1] "date_time" "time"      "timestamp"

``` r

# e.g., replace the default options for the time_col with
# just "date_time"
tf_options("time_col", "date_time")
tf_options("time_col")
```

    ## [1] "date_time"

``` r

# reset to default
tf_options("time_col", c("t", "timestamp", "time", "time_index",
    "time_col", "time_column", "tindex"))
```

In the following examples, pre-set global options (alone, and then in
combination with options given in arguments), are used by
`as.trackframe` to identify the key columns.

``` r

# Create example data
df <- data.frame(
  tindex = rep(as.POSIXct(Sys.time() + 1:5), times = 5),
  timestamp = rep(as.POSIXct(Sys.time() + 1:5), times = 5),
  x = runif(25, 0, 10),
  y = runif(25, 0, 10),
  y_north = runif(25, 0, 10),
  id = rep(1:5, each = 5)
)

# Convert to trackframe, identifying key columns using pre-set global options
tf_go <- as.trackframe(
  df,
  crs = NA
)

head(tf_go)
```

    ##                tindex           timestamp        x         y  y_north id
    ## 1 2026-06-11 17:04:38 2026-06-11 17:04:38 4.611865 0.2006522 8.056800  1
    ## 2 2026-06-11 17:04:39 2026-06-11 17:04:39 3.152418 3.7697093 8.140513  1
    ## 3 2026-06-11 17:04:40 2026-06-11 17:04:40 1.746759 5.5991284 4.039110  1
    ## 4 2026-06-11 17:04:41 2026-06-11 17:04:41 5.315735 8.5708359 2.184310  1
    ## 5 2026-06-11 17:04:42 2026-06-11 17:04:42 4.936370 3.8480971 4.183614  1
    ## 6 2026-06-11 17:04:38 2026-06-11 17:04:38 7.793086 5.2791704 6.688707  2

``` r

attributes(tf_go)[1:6]
```

    ## $names
    ## [1] "tindex"    "timestamp" "x"         "y"         "y_north"   "id"       
    ## 
    ## $class
    ## [1] "trackframe" "data.frame"
    ## 
    ## $row.names
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## 
    ## $time
    ## [1] "timestamp"
    ## 
    ## $easting
    ## [1] "x"
    ## 
    ## $northing
    ## [1] "y"

``` r

# Convert to trackframe, using a mix of pre-set global options and column names
# supplied to arguments
tf_go_2 <- as.trackframe(
  df,
  time_col = "tindex",
  northing_col = "y_north",
  crs = NA
)

head(tf_go_2)
```

    ##                tindex           timestamp        x         y  y_north id
    ## 1 2026-06-11 17:04:38 2026-06-11 17:04:38 4.611865 0.2006522 8.056800  1
    ## 2 2026-06-11 17:04:39 2026-06-11 17:04:39 3.152418 3.7697093 8.140513  1
    ## 3 2026-06-11 17:04:40 2026-06-11 17:04:40 1.746759 5.5991284 4.039110  1
    ## 4 2026-06-11 17:04:41 2026-06-11 17:04:41 5.315735 8.5708359 2.184310  1
    ## 5 2026-06-11 17:04:42 2026-06-11 17:04:42 4.936370 3.8480971 4.183614  1
    ## 6 2026-06-11 17:04:38 2026-06-11 17:04:38 7.793086 5.2791704 6.688707  2

``` r

attributes(tf_go_2)[1:6]
```

    ## $names
    ## [1] "tindex"    "timestamp" "x"         "y"         "y_north"   "id"       
    ## 
    ## $class
    ## [1] "trackframe" "data.frame"
    ## 
    ## $row.names
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## 
    ## $time
    ## [1] "tindex"
    ## 
    ## $easting
    ## [1] "x"
    ## 
    ## $northing
    ## [1] "y_north"

### Dealing with multiple key column name matches

When `as.trackframe` is identifying the key columns from vectors with
multiple options (whether they’ve been specified in the arguments of the
function, or via pre-set global options), and it encounters multiple
matches for a given column (e.g., the input data as a column named “y”
*and* a column named “northing”), then the first match in the options
vector is chosen. If the data in the matched columns is identical, then
the first match is silently chosen, whereas if the data are not
identical, then a warning is given.

``` r

# Create example data
df <- data.frame(
  timestamp = rep(as.POSIXct(Sys.time() + 1:5), times = 5),
  x = runif(25, 0, 10),
  y = runif(25, 0, 10),
  northing = runif(25, 0, 10),
  id = rep(1:5, each = 5),
  track_id = rep(1:5, each = 5)
)

# Convert to trackframe, using pre-set global options
tf_2 <- as.trackframe(
  df,
  crs = NA
)
```

    ## Warning in warn_if_guess_ambiguous(data, guesses): multiple possible columns
    ## found. northing chosen as northing_col

``` r

head(tf_2)
```

    ##             timestamp        x        y northing id track_id
    ## 1 2026-06-11 17:04:38 2.485387 6.998294 7.506033  1        1
    ## 2 2026-06-11 17:04:39 4.028812 2.200003 6.678158  1        1
    ## 3 2026-06-11 17:04:40 7.696302 7.279909 4.079732  1        1
    ## 4 2026-06-11 17:04:41 1.194854 2.170845 3.512488  1        1
    ## 5 2026-06-11 17:04:42 1.946950 4.562302 7.380916  1        1
    ## 6 2026-06-11 17:04:38 1.645692 3.327998 6.642855  2        2

``` r

attributes(tf_2)[1:6]
```

    ## $names
    ## [1] "timestamp" "x"         "y"         "northing"  "id"        "track_id" 
    ## 
    ## $class
    ## [1] "trackframe" "data.frame"
    ## 
    ## $row.names
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## 
    ## $time
    ## [1] "timestamp"
    ## 
    ## $easting
    ## [1] "x"
    ## 
    ## $northing
    ## [1] "northing"

In the above example, the columns `id` and `track_id` contain identical
values. The `track_id` column was silently chosen to be the `id_col`,
because “track_id” comes before “id” in the `tf_options("id_col")`
vector. The columns `y` and `northing` do not contain identical values.
Here, the `northing` column was chosen to be the `northing_col` because
“northing” comes before “y” in the `tf_options("northing_col")` vector.
Because the two columns (`y` and `northing`) do not contain identical
values, a warning is issued.

You can avoid this warning in three ways:

1.  Specify the key columns directly when calling `as.trackframe`, using
    the function arguments.
2.  Remove or rename non-key columns whose names match those in the
    key-column option vectors, before converting your data to a
    `trackframe` object.
3.  Modify the global `tf_options` so that column names in your data
    that are not actual key columns are excluded from the options
    vector.

``` r

# Create example data
df <- data.frame(
  timestamp = rep(as.POSIXct(Sys.time() + 1:5), times = 5),
  x = runif(25, 0, 10),
  y = runif(25, 0, 10),
  northing = runif(25, 0, 10),
  id = rep(1:5, each = 5),
  track_id = rep(1:5, each = 5)
)

# Convert to trackframe
tf_2 <- as.trackframe(
  df,
  crs = NA
) # --> this generates a WARNING
```

    ## Warning in warn_if_guess_ambiguous(data, guesses): multiple possible columns
    ## found. northing chosen as northing_col

``` r

# Convert to trackframe, option 1: specify key columns in arguments
tf_2 <- as.trackframe(
  df,
  northing_col = "y",
  crs = NA
) # --> no warning

# Convert to trackframe, option 2: remove or rename relevant columns
names(df)[which(names(df) == "northing")] <- "irrelevant_new_name"

tf_2 <- as.trackframe(
  df,
  crs = NA
) # --> no warning

# Convert to trackframe, option 3: modify global options
tf_options("northing_col",
  tf_options("northing_col")[-which(tf_options("northing_col") ==
        "northing")])

tf_2 <- as.trackframe(
  df,
  crs = NA
) # --> no warning

#reset to default
tf_options("northing_col", c("northing", tf_options("northing_col")))
```

If you are seeing “Warning message: In warn_if_guess_ambiguous(data,
guesses) : multiple possible columns found” while using a function or
workflow that is not part of the trackframe package, you may be using a
package that calls [`as.trackframe()`](../reference/as_trackframe.md)
behind the scenes.

In this case, you still have the same three options. You can remove or
rename relevant columns (option 2) or modify the global `tf_options`
(option 3). Alternatively, you can convert your data to a `trackframe`
object yourself, using `as.trackframe` and explicitly specify the
correct column names (option 1). Once you have a valid `trackframe`
object, you can then run the unction or workflow that originally
generated the warning on your new `trackframe` object instead of on your
original data.
