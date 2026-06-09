# Converts a cocomo object to a trackframe

Converts a cocomo object to a trackframe

## Usage

``` r
cocomo_as_tf(
  xs,
  ys,
  t,
  ids = data.frame(id_code = seq_len(NROW(xs))),
  crs = NA,
  na_omit = TRUE,
  sort = TRUE,
  coerce_to = "base",
  verbose = FALSE
)
```

## Arguments

- xs:

  matrix of x coordinates (UTM eastings) of all individuals in a group
  or population (rows) at every time point (columns) x\[i,t\] gives the
  x / easting position of individual i at time point t

- ys:

  matrix of y coordinates (UTM northings) of all individuals in a group
  or population (rows) at every time point (columns) y\[i,t\] gives the
  y / northing position of individual i at time point t

- t:

  vector of timestamps in posixct corresponding to the columns of x and
  y matrices. Timestamps must be uniformly sampled, though it is
  possible to have gaps (e.g. between different days of recording)

- ids:

  data frame giving information about the tracked individuals, with rows
  correpsonding to the rows of the x and y matrices. There must be one
  column called id_code which contains a unique individual identifier
  for each animal (e.g. for meerkats: 'VCVM001', for hyenas: 'WRTH', for
  coatis: 'Luna') The other columns contained are flexible, and can
  include information on age, sex, dominance, etc

- crs:

  coordinate reference system

- na_omit:

  logical indicator if NAs should be omitted

- sort:

  logical, if data should be sorted according to id_col and time_col

- coerce_to:

  the format trackframe is coerced to. `base`, `data.table` and `tibble`
  are supported. Default is `base` and coerces to a `data.frame`.

- verbose:

  logical, default value is `TRUE`

## Value

an object of class trackframe

## Examples

``` r
cocomo <- tf_as_cocomo(tf_mini)
cocomo_as_tf(cocomo$xs, cocomo$ys, cocomo$t, cocomo$ids)
#>                   time  easting northing      id
#> 1  2025-10-14 13:48:46 16.37250 48.20835 track_1
#> 2  2025-10-14 13:49:46 16.37334 48.20891 track_1
#> 3  2025-10-14 14:15:46 16.37334 48.20891 track_1
#> 4  2025-10-14 14:16:46 16.37319 48.20820 track_1
#> 5  2025-10-14 14:17:46 16.37328 48.20852 track_1
#> 6  2025-10-14 13:48:46 16.37250 48.20835 track_2
#> 7  2025-10-14 13:52:46 16.37250 48.20835 track_2
#> 8  2025-10-14 14:22:46 16.37250 48.20835 track_2
#> 9  2025-10-14 14:23:46 16.37231 48.20921 track_2
#> 10 2025-10-14 13:48:46 16.37250 48.20835 track_3
#> 11 2025-10-14 13:49:46 16.37313 48.20935 track_3
```
