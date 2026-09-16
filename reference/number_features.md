# Sort and number features by coordinates or distance

Used by
[layer_numbers()](https://elipousson.github.io/maplayer/reference/layer_markers.html)
function from maplayer package. Supports multiple types of sorting
including sorting:

- by centroid coordinates ("lon", "lat") appended with
  [`get_coords()`](https://elipousson.github.io/sfext/reference/get_coords.md)

- by one or more bounding box min or max values ("xmin", "ymin", "xmax",
  "ymax") appended with
  [`get_minmax()`](https://elipousson.github.io/sfext/reference/get_coords.md)

- by distance from the corner, side midpoint, or center of a bounding
  box ("dist_xmin_ymin", "dist_xmax_ymax", "dist_xmin_ymax",
  "dist_xmax_ymin", "dist_xmin_ymid", "dist_xmax_ymid",
  "dist_xmid_ymin", "dist_xmid_ymax", "dist_xmid_ymid")

- by distance to a point (or `sf`, `sfc`, or `bbox` object) passed to
  the to parameter

For example, in the eastern United States, you can sort and number
features from the top-left corner of the map to the bottom right by
setting sort to "dist_xmin_ymax" (default).

number_features also supports a range of different numbering styles
designed to match the standard enumeration options available in LaTeX.

## Usage

``` r
number_features(
  x,
  col = NULL,
  sort = "dist_xmin_ymax",
  to = NULL,
  desc = FALSE,
  crs = NULL,
  num_style = "arabic",
  num_start = 1,
  suffix = NULL,
  .id = "number"
)

number_sf(
  x,
  col = NULL,
  sort = "dist_xmin_ymax",
  to = NULL,
  desc = FALSE,
  crs = NULL,
  num_style = "arabic",
  num_start = 1,
  suffix = NULL,
  .id = "number"
)

sort_features(
  x,
  col = NULL,
  sort = c("lon", "lat"),
  to = NULL,
  desc = FALSE,
  crs = NULL
)

sort_sf(
  x,
  col = NULL,
  sort = c("lon", "lat"),
  to = NULL,
  desc = FALSE,
  crs = NULL
)
```

## Arguments

- x:

  A `sf` or `sfc` object.

- col:

  Group column name, Default: `NULL`

- sort:

  Sort column name, Default: "dist_xmin_ymax".

- to:

  A `sf` object used to determine sort order based on distance from a
  feature to the center of the "to" object.

- desc:

  If `TRUE`, sort descending; default `FALSE`.

- crs:

  Coordinate reference to use with
  [`get_coords()`](https://elipousson.github.io/sfext/reference/get_coords.md)
  or
  [`get_minmax()`](https://elipousson.github.io/sfext/reference/get_coords.md)
  if "sort" any of the following: "lon", "lat", "longitude", "latitude",
  "xmin", "ymin", "xmax", "ymax"

- num_style:

  Style of enumeration, either "arabic", "alph", "Alph", "roman",
  "Roman".

- num_start:

  Starting number; defaults to 1.

- suffix:

  Character to appended to "number" column. (e.g. "." for "1." or ":"
  for "1:"). Can also be a character vector with the same length as the
  number column.

- .id:

  Name of the column to use for the feature numbers; defaults to
  "number".

## Value

A `sf` object with a number column ordered by sort values.
