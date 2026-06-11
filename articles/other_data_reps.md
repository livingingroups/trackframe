# Converting to and from a cocomo dataset

## Converting to and from a `cocomo` dataset

The [`cocomo` R package](https://livingingroups.github.io/cocomo/) was
developed in the [CoCoMo research group](https://www.ab.mpg.de/cocomo)
at the Max Planck Institute of Animal Behavior. The package has many
functions for the analysis of communication and movement of animal
groups.

`cocomo` functions require that animal track data be stored in 4
separate objects: 2 `matrix` objects, one each representing the x and
the y positions of all individuals, 1 `vector` of timestamps, and 1
`dataframe` containing ids and information about the individuals. We
will refer to this dataset structure as a “cocomo dataset” throughout
this vignette.

### Convert a `trackframe` object to a cocomo dataset

To transform a `trackframe` object into a cocomo dataset, use the
`tf_as_cocomo` function. This function returns a single `list` that
contains the 4 objects that make up a cocomo dataset of animal tracks.

``` r

library(trackframe)

# Create a trackframe object with the locations of 3 individuals,
# each observed over 7 time intervals
tf <- as.trackframe(
  data.frame(
    timestamp = rep(as.POSIXct(Sys.time() + 1:7), times = 3),
    x = runif(21, 0, 10),
    y = runif(21, 0, 10),
    id = rep(1:3, each = 7)),
  crs = NA)

# Convert tf to a cocomo dataset
cocomo_list <- tf_as_cocomo(tf)

# Check out the cocomo dataset
attributes(cocomo_list)
```

    ## $names
    ## [1] "xs"  "ys"  "t"   "ids"

``` r

str(cocomo_list)
```

    ## List of 4
    ##  $ xs : num [1:3, 1:7] 0.808 2.898 4.023 8.343 7.329 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:3] "1" "2" "3"
    ##   .. ..$ : NULL
    ##  $ ys : num [1:3, 1:7] 6.78 6.96 4.32 7.35 6.89 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:3] "1" "2" "3"
    ##   .. ..$ : NULL
    ##  $ t  : POSIXct[1:7], format: "2026-06-11 17:04:41" "2026-06-11 17:04:42" ...
    ##  $ ids:'data.frame': 3 obs. of  1 variable:
    ##   ..$ id_code: int [1:3] 1 2 3

``` r

cocomo_list$xs
```

    ##        [,1]     [,2]     [,3]      [,4]       [,5]      [,6]     [,7]
    ## 1 0.8075014 8.343330 6.007609 1.5720844 0.07399441 4.6639350 4.977774
    ## 2 2.8976724 7.328820 7.725215 8.7460066 1.74940627 0.3424133 3.203857
    ## 3 4.0232824 1.956698 4.035381 0.6366146 3.88701313 9.7554784 2.898923

``` r

cocomo_list$ys
```

    ##       [,1]     [,2]      [,3]     [,4]     [,5]      [,6]     [,7]
    ## 1 6.783804 7.353196 1.9595673 9.805397 7.415215 0.5144628 5.302125
    ## 2 6.958239 6.885560 0.3123033 2.255625 3.008308 6.3646561 4.790245
    ## 3 4.321713 7.064338 9.4857658 1.803388 2.168999 6.8016292 4.988456

``` r

cocomo_list$t
```

    ## [1] "2026-06-11 17:04:41 UTC" "2026-06-11 17:04:42 UTC"
    ## [3] "2026-06-11 17:04:43 UTC" "2026-06-11 17:04:44 UTC"
    ## [5] "2026-06-11 17:04:45 UTC" "2026-06-11 17:04:46 UTC"
    ## [7] "2026-06-11 17:04:47 UTC"

``` r

cocomo_list$ids
```

    ##   id_code
    ## 1       1
    ## 2       2
    ## 3       3

So, this cocomo dataset is a `list` that contains 4 objects: the first
is a `matrix` of the x (easting) positions of every individual (rows)
across all timestamps (columns); the second is a `matrix` of the y
(northing) positions of every individual (rows) across all timestamps
(columns), the third is a `vector` of the 7 timestamps, and the fourth
is a `dataframe` of the 3 individuals’ ids.

We can then use a function from the `cocomo` package on the objects in
this cocomo dataset, for example:

``` r

# Install cocomo package
library(devtools)
install_github('livingingroups/cocomo')
```

``` r

library(cocomo)
```

    ## cocomo package contains both experimental and stable functions. Experimental functions have not been code reviewed and are likely to change in the future.

``` r

# Get the group centroid at each time step
get_group_centroid(xs = cocomo_list$xs,
  ys = cocomo_list$ys)
```

    ## $x_centr
    ## [1] 2.576152 5.876283 5.922735 3.651569 1.903471 4.920609 3.693518
    ## 
    ## $y_centr
    ## [1] 6.021252 7.101031 3.919212 4.621470 4.197507 4.560249 5.026942

### Convert a cocomo dataset to a `trackframe` object

To convert a `cocomo` dataset to a `trackframe` object, we can use the
dedicated `cocomo_as_tf` function, and specify exactly where to find the
information for each of the key columns in the arguments.

``` r

# Convert cocomo dataset to trackframe object
tf2 <- cocomo_as_tf(
  xs = cocomo_list$xs,
  ys = cocomo_list$ys,
  t = cocomo_list$t,
  ids = cocomo_list$ids,
  crs = NA)

tf2
```

    ##                   time    easting  northing id
    ## 1  2026-06-11 17:04:41 0.80750138 6.7838043  1
    ## 2  2026-06-11 17:04:42 8.34333037 7.3531960  1
    ## 3  2026-06-11 17:04:43 6.00760886 1.9595673  1
    ## 4  2026-06-11 17:04:44 1.57208442 9.8053967  1
    ## 5  2026-06-11 17:04:45 0.07399441 7.4152153  1
    ## 6  2026-06-11 17:04:46 4.66393497 0.5144628  1
    ## 7  2026-06-11 17:04:47 4.97777389 5.3021246  1
    ## 8  2026-06-11 17:04:41 2.89767245 6.9582388  2
    ## 9  2026-06-11 17:04:42 7.32881987 6.8855600  2
    ## 10 2026-06-11 17:04:43 7.72521511 0.3123033  2
    ## 11 2026-06-11 17:04:44 8.74600661 2.2556253  2
    ## 12 2026-06-11 17:04:45 1.74940627 3.0083081  2
    ## 13 2026-06-11 17:04:46 0.34241333 6.3646561  2
    ## 14 2026-06-11 17:04:47 3.20385731 4.7902455  2
    ## 15 2026-06-11 17:04:41 4.02328238 4.3217126  3
    ## 16 2026-06-11 17:04:42 1.95669835 7.0643384  3
    ## 17 2026-06-11 17:04:43 4.03538117 9.4857658  3
    ## 18 2026-06-11 17:04:44 0.63661457 1.8033877  3
    ## 19 2026-06-11 17:04:45 3.88701313 2.1689988  3
    ## 20 2026-06-11 17:04:46 9.75547835 6.8016292  3
    ## 21 2026-06-11 17:04:47 2.89892295 4.9884561  3

``` r

class(tf2)
```

    ## [1] "trackframe" "data.frame"

``` r

attributes(tf2)[1:6]
```

    ## $names
    ## [1] "time"     "easting"  "northing" "id"      
    ## 
    ## $row.names
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
    ## 
    ## $class
    ## [1] "trackframe" "data.frame"
    ## 
    ## $time
    ## [1] "time"
    ## 
    ## $easting
    ## [1] "easting"
    ## 
    ## $northing
    ## [1] "northing"
