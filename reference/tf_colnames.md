# Get and set column names of Trackframe Key columns

Key columns meaning: easting, northing, time, and (opt) id column All
3-4 key column names can be accessed or set with `tf_colnames()`. Each
also has a dedicated functions to access or set the names of the key
columns. Setting column names in this way will rename the currently
configured key column in the trackframe. It does not change the values
of the column.

## Usage

``` r
time_col(tf)

time_col(tf) <- value

id_col(tf)

id_col(tf) <- value

easting_col(tf)

easting_col(tf) <- value

northing_col(tf)

northing_col(tf) <- value

tf_colnames(tf)

tf_colnames(tf) <- value
```

## Arguments

- tf:

  a trackframe

- value:

  new column naems. `tf_colnames` takes a named character vector with
  names `easting`, `northing`, `time`, and (opt) `id`. `X_col()`
  functions take a single string

## Value

X_col returns a character object representing the column name.
tf_colnames returns a named character vector of length 4 indicating the
column names of each of the key columns.

## Details

To identify a different (existing) column as a key column, use
`as.trackframe`.

## See also

tf_coords, as.trackframe, tf_id, tf_time

## Examples

``` r
time_col(tf_mini)
#> [1] "time"
time_col(tf_mini) <- as.POSIXct(seq_along(nrow(tf_mini)))
colnames(tf_mini)
#> [1] "1"        "northing" "easting"  "id"      
id_col(tf_mini)
#> [1] "id"
id_col(tf_mini) <- "track_id"
colnames(tf_mini)
#> [1] "1"        "northing" "easting"  "track_id"
easting_col(tf_mini)
#> [1] "easting"
easting_col(tf_mini) <- "x"
northing_col(tf_mini)
#> [1] "northing"
northing_col(tf_mini) <- "y"
tf_colnames(tf_mini)
#>       time   northing    easting         id 
#>        "1"        "y"        "x" "track_id" 
tf_colnames(tf_mini)["id"] <- "track_id"
```
