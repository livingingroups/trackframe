# Convert an object to a `trackframe`

This function converts an object into a `trackframe` object, and checks
that the key columns exist and have valid data types. The coordinate
system must be Cartesian: either a projected coordinate system for
georeferenced data or another coordinate system in Euclidean space for
non-georeferenced data (e.g., for captive or simulated data).

## Usage

``` r
trackframe(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  crs = NULL,
  ...
)

as.trackframe(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  crs = NULL,
  ...
)

# S3 method for class 'data.frame'
as.trackframe(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  crs = tf_options("crs"),
  ...
)

# S3 method for class 'matrix'
as.trackframe(
  data,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  crs = tf_options("crs"),
  ...
)

# S3 method for class 'move2'
as.trackframe(
  data,
  time_col = NULL,
  easting_col = NULL,
  northing_col = NULL,
  id_col = NULL,
  sort = TRUE,
  coerce_to = "base",
  ...
)

# S3 method for class 'sftrack'
as.trackframe(
  data,
  time_col = NULL,
  easting_col = NULL,
  northing_col = NULL,
  id_col = NULL,
  sort = TRUE,
  coerce_to = "base",
  ...
)

# S3 method for class 'sf'
as.trackframe(
  data,
  time_col = tf_options("time_col"),
  easting_col = NULL,
  northing_col = NULL,
  id_col = tf_options("id_col"),
  sort = TRUE,
  coerce_to = "base",
  ...
)

# S3 method for class 'trackframe'
as.trackframe(
  data,
  time_col = NULL,
  easting_col = NULL,
  northing_col = NULL,
  id_col = NULL,
  sort = TRUE,
  coerce_to = NULL,
  crs = NULL,
  ...
)
```

## Arguments

- data:

  a `data.frame`, `matrix`, `sftrack` object or `move2` object
  containing timestamped coordinates of one or multiple animals'
  locations. The object should have one row per location, and must
  contain at least a column for each of time, easting, and northing.

- time_col:

  a character string or vector of strings specifying the name, or
  possible names, of the time column in `data`. If a vector is provided,
  the first element matching a column name of `data` is used. The data
  in the time column must be of class `POSIXct` or coercible to
  `POSIXct`.

- easting_col:

  a character string or vector of strings specifying the name, or
  possible names, of the easting column in `data`. If a vector is
  provided, the first element matching a column name of `data` is used.
  The data in the easting column must be numeric.

- northing_col:

  a character string or vector of strings specifying the name, or
  possible names, of the northing column in `data`. If a vector is
  provided, the first element matching a column name of `data` is used.
  The data in the northing column must be numeric.

- id_col:

  an optional character string or vector of strings specifying the name,
  or possible names, of the id column (the column containing the
  individual identifier(s) of each track) in `data`. If a vector is
  provided, the first element matching a column name of `data` is used.
  The data in the id column can be numeric, character, or factor.

- sort:

  logical, if true (default), data will be sorted by id_col and then by
  time_col.

- coerce_to:

  the type of dataframe that trackframe is coerced to. `base`,
  `data.table` and `tibble` are supported. The default is `base`, which
  coerces to a `data.frame` without `data.table` or `tbl` classes. If
  NULL, the returned `trackframe` object takes the same dataframe type
  as the input `data` object.

- crs:

  the numeric [EPSG coordinate reference system code](https://epsg.io/)
  for the coordinates. This argument is required for non-sf input. Use
  `NA` to denote non-georeferenced coordinates. The coordinate reference
  system must be Cartesian.

- ...:

  Additional arguments (unused).

## Value

A `trackframe` object with appropriate attributes set.

## Details

When creating a `trackframe` object from another representation of
animal track data, `as.trackframe` identifies the columns representing
time (`time_col`), coordinates (`northing_col` & `easting_col`), and
track id (`id_col`). It obtains this information from 3 potential
sources: 1) information inherited from the input object (see below), 2)
arguments supplied directly to `as.trackframe`, and/or 3) package-level
options(see [tf_options](tf_options.md)).

If the input object is a `data.frame`, `data.table`, or `tibble`, no
information about the (`time_col`), coordinates (`northing_col` &
`easting_col`), and track id (`id_col`) columns is inherited, all are
identified from other sources (i.e., arguments supplied directly to the
function, or package-level options). If the input object is an `sf`
object (but not `sftrack` or `move2`), the `easting_col` and
`northing_col` are identified from the input object's `geometry` column,
and the `time_col` and `id_col` are identified from other sources. If
the input object is an `sftrack` or `move2` object, then all 4 of the
key columns are identified from the input object's attributes. See
vignette `vignette("identifying-columns")` for details and examples.

Trackframe only supports Cartesian coordinates, i.e., coordinates that
define locations on a flat surface. Geographic coordinate systems, i.e.,
those that define locations on a three-dimensional spherical model of
the Earth using latitude and longitude, are not supported by trackframe
or by (most) functions using trackframe. If you try to pass a
non-Cartesian georeferenced coordinate reference system to the `crs`
argument, you will get an error. Data containing latitude-longitude
coordinates must first be projected before the data is converted to a
`trackframe` object. If you set `crs = NA` for non-georeferenced
coordinates, the function will assume that these are Cartesian
coordinates, though it has no way of checking or warning if not.

## See also

[tf_options](tf_options.md), [tf_backtransform](tf_backtransform.md)

## Examples

``` r
library(trackframe)

# Create a dataframe
set.seed(2025)
df <- data.frame(
  x = rnorm(10),
  y = rnorm(10),
  t = 1:10,
  animal_id = c(rep('a', 5), rep('b', 5))
)

# Convert dataframe to trackframe, identify key columns by setting arguments
tf <- trackframe(df,
                 time_col = "t",
                 easting_col = "x",
                 northing_col = "y",
                 id_col = "animal_id",
                 crs = "EPSG:32632")

class(tf)
#> [1] "trackframe" "data.frame"
attributes(tf)
#> $names
#> [1] "x"         "y"         "t"         "animal_id"
#> 
#> $class
#> [1] "trackframe" "data.frame"
#> 
#> $row.names
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $time
#> [1] "t"
#> 
#> $easting
#> [1] "x"
#> 
#> $northing
#> [1] "y"
#> 
#> $id
#> [1] "animal_id"
#> 
#> $crs
#> [1] "EPSG:32632"
#> 
#> $crs_type
#> [1] "projected"
#> 
#> $transformation_info
#> $transformation_info$attributes
#> $transformation_info$attributes$names
#> [1] "x"         "y"         "t"         "animal_id"
#> 
#> $transformation_info$attributes$class
#> [1] "data.frame"
#> 
#> $transformation_info$attributes$row.names
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> 
#> $transformation_info$class
#> [1] "data.frame"
#> 
#> $transformation_info$names
#> [1] "x"         "y"         "t"         "animal_id"
#> 
#> $transformation_info$coord_names
#> [1] "x" "y"
#> 
#> $transformation_info$id_hash_orig
#>                                  1                                  2 
#> "97aa3a7ea569fbbcfb11a2b7c4aae712" "c599b9f1887f552a7b5cfecccda17f24" 
#>                                  3                                  4 
#> "1ef9f76abaca3d0f10c62fddd488ef0c" "5ba2b7922bab34cb907b34542f0dd4bc" 
#>                                  5                                  6 
#> "f8ace80877a38ed1dfa6f82a594cc71c" "abb8d0dd4139ea53c7c8b1328d265d87" 
#>                                  7                                  8 
#> "5c8f965fb0bea3bfa4f1da4037528759" "d372c8e1f76f95da2749b2054cda6d1d" 
#>                                  9                                 10 
#> "991f83496d0c26f9b07c3880c881fefa" "a1d89c0339f709a0d03b35358b920313" 
#> 
#> $transformation_info$id_hash_ordered
#>                                  1                                  2 
#> "97aa3a7ea569fbbcfb11a2b7c4aae712" "c599b9f1887f552a7b5cfecccda17f24" 
#>                                  3                                  4 
#> "1ef9f76abaca3d0f10c62fddd488ef0c" "5ba2b7922bab34cb907b34542f0dd4bc" 
#>                                  5                                  6 
#> "f8ace80877a38ed1dfa6f82a594cc71c" "abb8d0dd4139ea53c7c8b1328d265d87" 
#>                                  7                                  8 
#> "5c8f965fb0bea3bfa4f1da4037528759" "d372c8e1f76f95da2749b2054cda6d1d" 
#>                                  9                                 10 
#> "991f83496d0c26f9b07c3880c881fefa" "a1d89c0339f709a0d03b35358b920313" 
#> 
#> 

tf <- as.trackframe(df,
                    time_col = "t",
                    easting_col = "x",
                    northing_col = "y",
                    id_col = "animal_id",
                    crs = "EPSG:32632")

# Convert dataframe to trackframe, identify key columns by having as.trackframe match column
# names to tf_options
tf <- as.trackframe(df_mini, crs = NA)

# Convert dataframe to trackframe, identify key columns by having as.trackframe match column
# names to multiple options given in arguments
tf <- as.trackframe(df_mini,
                    time_col = c("t", "time", "timestamp"),
                    easting_col = c("x", "easting", "horiz"),
                    northing_col = c("y", "northing", "vert"),
                    id_col = c("animal", "id"),
                    crs = NA)

# Convert dataframe to trackframe, identify key columns by setting arguments, also set
# crs = NA to indicate non georeferenced coordinates
tf <-  as.trackframe(df,
                    time_col = "t",
                    easting_col = "x",
                    northing_col = "y",
                    id_col = "animal_id",
                    crs = NA)

# Convert dataframe to trackframe, identify key columns by having as.trackframe match column
# names to tf_options, set crs = NA to indicate non georeferenced coordinates, coerce output
# to be:
# ... a plain (base R) data.frame
tf_df <- as.trackframe(df, coerce_to = "base", crs = NA)
attributes(tf_df)$class
#> [1] "trackframe" "data.frame"
# ... a data.table
tf_dt <- as.trackframe(df, coerce_to = "data.table", crs = NA)
attributes(tf_dt)$class
#> [1] "trackframe" "data.table" "data.frame"
# ... a tibble.
tf_tib <- as.trackframe(df, coerce_to = "tibble", crs = NA)
attributes(tf_tib)$class
#> [1] "trackframe" "tbl_df"     "tbl"        "data.frame"

# Projected CRS, acceptable
tf_df <- as.trackframe(df, coerce_to = "base", crs = "EPSG:32632")
# Non Cartesian crs, e.g. "EPSG:4326", is not acceptable

# Example for move2 objects

# Key columns and crs are identified from the attributes of the move2 object
tf <- as.trackframe(data = move2_mini)


# Example for sftrack objects

# Key columns and crs are identified from the attributes of the sftrack object
tf <- as.trackframe(data = sftrack_mini)

```
