# Buffer a simple feature or bounding box object

Return an sf object with a buffer based on `dist` or a proportion of the
diagonal distance defined by `diag_ratio`. If x uses geographic
coordinates, the coordinate reference system is transformed into
EPSG:3857 and then transformed back into the original CRS after the
buffer has been applied.

## Usage

``` r
st_buffer_ext(
  x,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  dist_limits = NULL,
  end_style = NULL,
  join_style = NULL,
  single_side = FALSE,
  allow_null = TRUE,
  allow_list = TRUE,
  ...
)

# Default S3 method
st_buffer_ext(
  x,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  dist_limits = NULL,
  end_style = NULL,
  join_style = NULL,
  single_side = FALSE,
  allow_null = TRUE,
  ...
)

# S3 method for class 'bbox'
st_buffer_ext(
  x,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  dist_limits = NULL,
  end_style = NULL,
  join_style = NULL,
  single_side = FALSE,
  ...
)

# S3 method for class 'list'
st_buffer_ext(
  x,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  dist_limits = NULL,
  end_style = NULL,
  join_style = NULL,
  single_side = FALSE,
  allow_null = TRUE,
  allow_list = TRUE,
  ...
)

# S3 method for class 'sf_list'
st_buffer_ext(
  x,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  dist_limits = NULL,
  end_style = NULL,
  join_style = NULL,
  single_side = FALSE,
  allow_null = TRUE,
  allow_list = TRUE,
  ...
)

st_edge(x, dist = NULL, diag_ratio = NULL, unit = "meter", ...)
```

## Arguments

- x:

  A `sf`, `sfc`, or `bbox` object or a list of `sf` objects.

- dist:

  buffer distance in units. Optional.

- diag_ratio:

  ratio of diagonal distance of area's bounding box used as buffer
  distance. e.g. if the diagonal distance is 3000 meters and the
  "diag_ratio = 0.1" a 300 meter will be used. Ignored when `dist` is
  provided.

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

- dist_limits:

  Numeric vector of any length (minimum and maximum values used as lower
  and upper limits on distance buffer). Units must match the provided
  units; defaults to `NULL`.

- end_style:

  "round" (default), "flat", or "square" passed to the endCapStyle
  parameter of
  [`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- join_style:

  "round" (default), "mitre", or "bevel" passed to the joinStyle
  parameter of
  [`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- single_side:

  If `TRUE`, single-sided buffers are returned for linear geometries, in
  which case negative dist values give buffers on the right-hand side,
  positive on the left.

- allow_null:

  If `TRUE` (default) and x is `NULL`, a `NULL` value is returned with
  no error.

- allow_list:

  If `TRUE` (default), allow sf list objects as an input and use
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) to
  apply the provided parameters to each object within the list to return
  as a new sf list object.

- ...:

  Additional parameters passed to
  [`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html)

## Details

`st_edge()` is a variation on `st_buffer_ext()` where dist or diag_ratio
is used to define the width of the edge to return either outside the
existing geometry (for positive dist values) or inside the existing
geometry (for negative dist values).
