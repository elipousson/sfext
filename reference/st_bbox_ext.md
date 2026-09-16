# Get a bounding box buffered a set distance or to match an aspect ratio

`st_bbox_ext()` converts the input to a bounding box with
[`as_bbox()`](https://elipousson.github.io/sfext/reference/as_sf.md),
applies a buffer (based on a specified distance or proportion of the
diagonal distance across the bounding box) and adjusts the bounding box
aspect ratio before returning a bounding box (or another class specified
by the class parameter). If the input object is a list or `sf_list`
object, the function always returns a `list` or `sf_list`.

## Usage

``` r
st_bbox_ext(
  x,
  dist = NULL,
  diag_ratio = NULL,
  asp = NULL,
  unit = NULL,
  crs = NULL,
  class = "bbox",
  nudge = NULL,
  allow_null = TRUE,
  allow_list = TRUE
)

st_bbox_asp(
  x,
  asp = NULL,
  class = "bbox",
  allow_null = TRUE,
  allow_list = TRUE
)
```

## Arguments

- x:

  A `sf`, `sfc`, `bbox`, `sfg`, `Raster`, `Spatial`, `Extent`,
  `numeric`, or `character` object (a place name passed to
  [`osmdata::getbb()`](https://docs.ropensci.org/osmdata/reference/getbb.html)).
  See
  [`as_bbox()`](https://elipousson.github.io/sfext/reference/as_sf.md)
  for more details.

- dist:

  buffer distance in units. Optional.

- diag_ratio:

  ratio of diagonal distance of area's bounding box used as buffer
  distance. e.g. if the diagonal distance is 3000 meters and the
  "diag_ratio = 0.1" a 300 meter will be used. Ignored when `dist` is
  provided.

- asp:

  Aspect ratio of width to height as a numeric value (e.g. 0.33) or
  character (e.g. "1:3"). If numeric,
  [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.md)
  returns the same value without modification.

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

- crs:

  Coordinate reference system of bounding box to return; defaults to
  `NULL` which maintains the crs of the input object.

- class:

  Class of object to return passed to
  [`as_sf_class()`](https://elipousson.github.io/sfext/reference/as_sf.md);
  defaults to "bbox".

- nudge:

  Passed as to parameter
  [`st_nudge()`](https://elipousson.github.io/sfext/reference/st_nudge.md)
  when not `NULL`. A numeric vector, a `sf` object, or any other object
  that can be converted to a simple feature collection with
  [`as_sfc()`](https://elipousson.github.io/sfext/reference/as_sf.md)..

- allow_null:

  If `TRUE` (default) and x is `NULL`, return `NULL` or, if `FALSE`,
  abort function.

- allow_list:

  If `TRUE` (default), allow sf list objects as an input and use
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) to
  apply the provided parameters to each object within the list to return
  as a new sf list object.

## Value

A `bbox` object converted to match the class of the class parameter.

## Details

`st_bbox_asp()` supports aspect ratio adjustments without applying a
buffer. asp can be supplied as a number or a string matching the format
of "width:height". Common aspect ratios include "1:1" (1), "4:6"
(0.666), "8.5:11", "16:9" (1.777).
