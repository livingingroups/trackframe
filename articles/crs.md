# Compatible Coordinate Systems

## Compatible Coordinate Systems

Trackframe only supports Cartesian (or Euclidean) coordinate systems,
i.e., systems in which coordinates describe locations on a flat plane
using perpendicular northing (y) and easting (x) axes, and units
represent consistent physical distances in all directions.

As long as the coordinates are Cartesian, trackframe supports both:
georeferenced data, wherein the coordinates explicitly represent
locations on the Earth’s surface; and non-georeferenced data, wherein
the coordinates do not explicitly or necessarily represent locations on
the Earth’s surface.

### Load Package

``` r

library(trackframe)
```

### Georeferenced data

For georeferenced coordinates (i.e., coordinates that represent
locations on the Earth’s surface), `trackframe` requires that they be
projected, e.g., UTMs rather than latitute-longitude.

For `sf` objects (`move2` and `sftrack` objects), `trackframe`
identifies the coordinate system using
[`sf::st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html).
For non-sf objects, you have to specify the coordinate reference system
with the `crs` argument in `as.trackframe`. If your data has
non-Cartesian georeferenced coordinates, for example, latitude and
longitude, you will need to first project your data before converting it
to to a trackframe object. For this reason, trackframe may not be
appropriate for studying movement processes that cross large enough
distances that distortion from projected coordinates causes meaningful
measurement inaccuracies (e.g., long-distance migration).

Take this example, using a `move2` object:

``` r

library(move2)

# Load a move2 object using the move2 package
fisher_move2 <- mt_read(mt_example())

# Check the coordinate reference system
sf::st_crs(fisher_move2)
```

    ## Coordinate Reference System:
    ##   User input: EPSG:4326 
    ##   wkt:
    ## GEOGCRS["WGS 84",
    ##     ENSEMBLE["World Geodetic System 1984 ensemble",
    ##         MEMBER["World Geodetic System 1984 (Transit)"],
    ##         MEMBER["World Geodetic System 1984 (G730)"],
    ##         MEMBER["World Geodetic System 1984 (G873)"],
    ##         MEMBER["World Geodetic System 1984 (G1150)"],
    ##         MEMBER["World Geodetic System 1984 (G1674)"],
    ##         MEMBER["World Geodetic System 1984 (G1762)"],
    ##         MEMBER["World Geodetic System 1984 (G2139)"],
    ##         ELLIPSOID["WGS 84",6378137,298.257223563,
    ##             LENGTHUNIT["metre",1]],
    ##         ENSEMBLEACCURACY[2.0]],
    ##     PRIMEM["Greenwich",0,
    ##         ANGLEUNIT["degree",0.0174532925199433]],
    ##     CS[ellipsoidal,2],
    ##         AXIS["geodetic latitude (Lat)",north,
    ##             ORDER[1],
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##         AXIS["geodetic longitude (Lon)",east,
    ##             ORDER[2],
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##     USAGE[
    ##         SCOPE["Horizontal component of 3D system."],
    ##         AREA["World."],
    ##         BBOX[-90,-180,90,180]],
    ##     ID["EPSG",4326]]

The objects coordinate reference system is WGS 84 (i.e., `crs = 4326` or
`EPSG:4326`), i.e., the standard decimal latitude-longitude. These are,
thus, not Cartesian coordinates. If we attempt to directly convert this
object to a `trackframe` object, it will fail…

``` r

# Convert to trackframe (ERROR since non-Cartesian coordinates)
fisher_tf <- as.trackframe(data = fisher_move2)
```

    ## Error in `derive_crs_type()`:
    ## ! Expected projected coordinates, got geographic coordinates. Please project into an appropriate crs.

Note the instruction in the error message: “Please project into an
appropriate crs.”

There are several packages and approaches for projecting georeferenced
data. In the following example, we use the `st_transform` function from
the `sf` package.

``` r

# Project the coordinates into the appropriate UTM zone (in this case,
# UTM zone 38S, or EPSG: 32738), and then convert to trackframe object
fisher_tf <- fisher_move2 |>
  sf::st_transform(crs = 32738) |>
  as.trackframe()

# Extract the input crs EPSG code from the tf object's transformation_info
crs(fisher_tf)
```

    ## [1] "EPSG:32738"

In the following example, we convert a `data.frame` object to a
`trackframe` object. The dataframe doesn’t contain any georeferencing
metadata (i.e., a crs), but let’s say that we know that these data were
collected in southern Germany and that the coordinates are already
projected (i.e., they’re in UTM zone 32N, or ESPG:32632). In this case,
you can simply tell `as.trackframe` what the crs is, using the `crs`
argument or `tf_options("crs")`.

``` r

# Create some example data
df <- data.frame(
  time_col = rep(as.POSIXct(Sys.time() + 1:5), times = 5),
  easting_col = runif(25, 0, 10),
  northing_col = runif(25, 0, 10),
  id = rep(1:5, each = 5)
)

# Specify the crs when converting to trackframe
tf <- as.trackframe(df, crs = "EPSG:32632")

# Extract the input crs EPSG code from the tf object
crs(tf)
```

    ## [1] "EPSG:32632"

``` r

# Set crs globally using `tf_options`
tf_options("crs", "EPSG:32632")
tf <- as.trackframe(df)

# Global crs is applied to tf
crs(tf)
```

    ## [1] "EPSG:32632"

### Non-georeferenced data

For non-georeferenced coordinates, i.e., those that do not represent
locations on the Earth’s surface, the `crs` argument in `as.trackframe`
should be set to `NA`. This may apply to, for example, data pertaining
to simulations (i.e., representing locations on a virtual plane) or
captive animal locations (i.e., representing locations in an enclosure,
measured relative to some reference point in or near the enclosure).
Note that even non-georeferenced coordinates should still be Cartesian,
but `trackframe` has no way of verifying this or alerting if this is not
the case.

Where `as.trackframe` does not inherit metadata specifying the `crs`
from the input object (for example, when the input object is a
dataframe), the `crs` **must** be provided either as an argument or via
`tf_options("crs")`. Otherwise `as.trackframe` will throw an error.

``` r

# Create some example data
df <- data.frame(
  time_col = rep(as.POSIXct(Sys.time() + 1:5), times = 5),
  easting_col = runif(25, 0, 10),
  northing_col = runif(25, 0, 10),
  id = rep(1:5, each = 5)
)

# Convert df to trackframe without specifying crs (ERROR since no crs given)
tf_non_gr <- as.trackframe(df)

# Convert df to trackframe with non-georeferenced crs
tf_non_gr <- as.trackframe(df, crs = NA)

# Extract the crs from the tf object
crs(tf_non_gr)
```

    ## [1] NA

``` r

# Set crs globally using `tf_options`
tf_options("crs", NA)
tf <- as.trackframe(df)

# Global crs is applied to tf
crs(tf)
```

    ## [1] NA
