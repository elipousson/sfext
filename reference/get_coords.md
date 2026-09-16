# Get coordinates for a simple feature or bounding box object

An extended version of
[`sf::st_coordinates()`](https://r-spatial.github.io/sf/reference/st_coordinates.html)
that supports binding coordinates to the object, optionally dropping the
geometry, and returning wkt or a point on surface (geometry = "surface
point") instead of the centroid.

## Usage

``` r
get_coords(
  x,
  coords = NULL,
  geometry = "centroid",
  crs = NULL,
  keep_all = TRUE,
  drop = TRUE,
  call = caller_env()
)

get_minmax(x, crs = NULL, keep_all = TRUE, drop = TRUE)
```

## Arguments

- x:

  A `sf`, `bbox`, or `sfc` object.

- coords:

  Column names to use for coordinates in results, Default: `NULL`; which
  is set to c("lon", "lat") by
  [`check_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md).

- geometry:

  geometry to use for coordinates "centroid", "surface point", or
  alternatively "wkt"; defaults to `NULL` ("centroid").

- crs:

  Coordinate reference system to use for coordinates; defaults to
  `NULL`.

- keep_all:

  If `TRUE`, bind the coordinates columns to the provided object x;
  defaults to `TRUE`.

- drop:

  If `TRUE` and x is an sf object, drop the geometry Default: `TRUE`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Details

`get_minmax()` get a bounding box for each feature (or group of
features) appends the xmin, ymin, xmax, and ymax values for each feature
to the simple feature object.
