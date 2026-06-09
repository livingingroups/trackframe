# Merge Two trackframes

Merge two trackframes by key colums ("time", "id", "easting",
"northing"), or do other versions of database join operations.

## Usage

``` r
# S3 method for class 'trackframe'
merge(
  x,
  y,
  by = NULL,
  by.x = NULL,
  by.y = NULL,
  all = FALSE,
  all.x = all,
  all.y = all,
  sort = TRUE,
  suffixes = c("", ".y"),
  ...
)
```

## Arguments

- x:

  an object of class trackframe

- y:

  an object of class trackframe

- by:

  specifications of the columns used for merging. Default is
  tf_colnames(x)

- by.x:

  specifications of the columns used for merging of x.

- by.y:

  specifications of the columns used for merging of y.

- all:

  logical all = TRUE is shorthand for all.x = TRUE and all.y = TRUE

- all.x:

  logical; if TRUE, then extra rows will be added to the output, one for
  each row in x that has no matching row in y. These rows will have NAs
  in those columns that are usually filled with values from y. The
  default is FALSE, so that only rows with data from both x and y are
  included in the output.

- all.y:

  logical; analogous to all.x.

- sort:

  logical, if true (default), data will be sorted by id_col and then by
  time_col.

- suffixes:

  a character vector of length 2 specifying the suffixes to be used for
  making unique the names of columns in the result which are not used
  for merging (appearing in by etc).

- ...:

  other arguments to be passed to appropriate merge methods

## Value

an object of class trackframe

## Examples

``` r
tf1 <- trackframe::tf_mini
tf2 <- tf1
tf2$id2 <- "A"
tf1_tf2 <- merge(tf1, tf2)
tf1_tf2
#>                   time northing  easting      id id2
#> 1  2025-10-14 13:48:46 48.20835 16.37250 track_1   A
#> 4  2025-10-14 13:49:46 48.20891 16.37334 track_1   A
#> 7  2025-10-14 14:15:46 48.20891 16.37334 track_1   A
#> 8  2025-10-14 14:16:46 48.20820 16.37319 track_1   A
#> 9  2025-10-14 14:17:46 48.20852 16.37328 track_1   A
#> 2  2025-10-14 13:48:46 48.20835 16.37250 track_2   A
#> 6  2025-10-14 13:52:46 48.20835 16.37250 track_2   A
#> 10 2025-10-14 14:22:46 48.20835 16.37250 track_2   A
#> 11 2025-10-14 14:23:46 48.20921 16.37231 track_2   A
#> 3  2025-10-14 13:48:46 48.20835 16.37250 track_3   A
#> 5  2025-10-14 13:49:46 48.20935 16.37313 track_3   A
class(tf1_tf2)
#> [1] "trackframe" "data.frame"

tf3 <- tf1
tf3$id <- c(rep("A",5), rep("B", 4), rep("C",2))
merge(tf1, tf3, all = TRUE)
#>                   time northing  easting      id
#> 1  2025-10-14 13:48:46 48.20835 16.37250       A
#> 7  2025-10-14 13:49:46 48.20891 16.37334       A
#> 13 2025-10-14 14:15:46 48.20891 16.37334       A
#> 15 2025-10-14 14:16:46 48.20820 16.37319       A
#> 17 2025-10-14 14:17:46 48.20852 16.37328       A
#> 2  2025-10-14 13:48:46 48.20835 16.37250       B
#> 11 2025-10-14 13:52:46 48.20835 16.37250       B
#> 19 2025-10-14 14:22:46 48.20835 16.37250       B
#> 21 2025-10-14 14:23:46 48.20921 16.37231       B
#> 3  2025-10-14 13:48:46 48.20835 16.37250       C
#> 9  2025-10-14 13:49:46 48.20935 16.37313       C
#> 4  2025-10-14 13:48:46 48.20835 16.37250 track_1
#> 8  2025-10-14 13:49:46 48.20891 16.37334 track_1
#> 14 2025-10-14 14:15:46 48.20891 16.37334 track_1
#> 16 2025-10-14 14:16:46 48.20820 16.37319 track_1
#> 18 2025-10-14 14:17:46 48.20852 16.37328 track_1
#> 5  2025-10-14 13:48:46 48.20835 16.37250 track_2
#> 12 2025-10-14 13:52:46 48.20835 16.37250 track_2
#> 20 2025-10-14 14:22:46 48.20835 16.37250 track_2
#> 22 2025-10-14 14:23:46 48.20921 16.37231 track_2
#> 6  2025-10-14 13:48:46 48.20835 16.37250 track_3
#> 10 2025-10-14 13:49:46 48.20935 16.37313 track_3
```
