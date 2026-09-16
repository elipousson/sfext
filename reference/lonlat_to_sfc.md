# Convert a lon/lat or lat/lon coordinate pair to a sfc object

**\[experimental\]**

## Usage

``` r
lonlat_to_sfc(
  x,
  range = getOption("sfext.coord_range", c(xmin = -180, ymin = -50, xmax = 180, ymax =
    60)),
  quiet = FALSE,
  call = parent.frame(),
  ...
)
```

## Arguments

- x:

  A length 2 numeric vector with geodetic coordinates in a
  [EPSG:4326](https://epsg.org/crs_4326/WGS-84.html) coordinate
  reference system.

- range:

  For `lonlat_to_sfc()`, an object that is coercible to a `bbox` object
  or a length 4 vector with names xmin, xmax, ymin, and ymax. If a
  coordinate pair falls outside the latitude/longitude range defined by
  the vector but inside the range if reversed, the coordinates are
  assumed to be in lat/lon order and are switched to lon/lat order
  before being converted to a point. Defaults to
  `c("xmin" = -180, "ymin" = -50, "xmax" = 180, "ymax" = 60)`. Note that
  this default setting will reverse valid coordinates north of
  Anchorage, Alaska or south of New Zealand.

- quiet:

  If `TRUE`, suppress alert messages when converting a lat/lon
  coordinate pair to a lon/lat pair. Defaults to `FALSE`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- ...:

  Arguments passed on to
  [`sf::st_sfc`](https://r-spatial.github.io/sf/reference/sfc.html)

  `precision`

  :   numeric; see
      [st_as_binary](https://r-spatial.github.io/sf/reference/st_as_binary.html)

  `check_ring_dir`

  :   see
      [st_read](https://r-spatial.github.io/sf/reference/st_read.html)

  `dim`

  :   character; if this function is called without valid geometries,
      this argument may carry the right dimension to set empty
      geometries

  `recompute_bbox`

  :   logical; use `TRUE` to force recomputation of the bounding box

  `oriented`

  :   logical; if `TRUE`, the ring is oriented such that left of the
      edges is inside the polygon; this is needed for convering polygons
      larger than half the globe to s2

  `fall_back_class`

  :   character; class for return object when no geometries are provided
      as input

  `i`

  :   record selection. Might also be an `sfc`/`sf` object to work with
      the `op` argument

  `j`

  :   ignored if `op` is specified

  `op`

  :   function, geometrical binary predicate function to apply when `i`
      is a `sf`/`sfc` object. Additional arguments can be specified
      using `...`, see examples.

## See also

[`is_geo_coords()`](https://elipousson.github.io/sfext/reference/is_sf.md)
