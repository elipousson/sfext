# Measure a bounding box using x, y, or diagonal distance

- Measure distances (`sf_bbox_dist()`, `sf_bbox_xdist()`,
  `sf_bbox_ydist()`, and `sf_bbox_diagdist()`)

- Convert a diag_ratio value to a distance value if a bbox and
  diag_ratio are provided (`sf_bbox_diag_ratio_to_dist()`); return
  `NULL` if bbox and diag_ratio are `NULL`

- Get an aspect ratio or orientation (`sf_bbox_asp()`) (counts asp
  between 0.9 and 1.1 as "square" when tolerance is 0.1) or
  (`sf_bbox_orientation()`)

## Usage

``` r
sf_bbox_dist(
  bbox,
  from,
  to,
  units = NULL,
  drop = TRUE,
  by_element = TRUE,
  call = caller_env(),
  ...
)

sf_bbox_xdist(bbox, units = NULL, drop = TRUE)

sf_bbox_ydist(bbox, units = NULL, drop = TRUE)

sf_bbox_diagdist(bbox, units = NULL, drop = TRUE)

sf_bbox_diag_ratio_to_dist(bbox, diag_ratio, units = NULL, drop = TRUE)

sf_bbox_asp(bbox)

sf_bbox_orientation(bbox, tolerance = 0.1, call = caller_env())

sf_bbox_check_fit(bbox, dist)
```

## Arguments

- bbox:

  A `bbox` object.

- from, to:

  Length 2 character vectors with xy pairs (e.g. c("xmax", "ymax)
  defining points to measure distance from and to.

- units:

  The units to return for sf_bbox_dist. Defaults to NULL.

- drop:

  If `FALSE`, distance functions return with units. If `FALSE`
  (default), distance functions return numeric values.

- by_element:

  logical; if `TRUE`, return a vector with distance between the first
  elements of `x` and `y`, the second, etc; an error is raised if `x`
  and `y` are not the same length. If `FALSE`, return the dense matrix
  with all pairwise distances.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- ...:

  passed on to
  [s2_distance](https://r-spatial.github.io/s2/reference/s2_is_collection.html),
  [s2_distance_matrix](https://r-spatial.github.io/s2/reference/s2_closest_feature.html),
  or
  [s2_perimeter](https://r-spatial.github.io/s2/reference/s2_is_collection.html)

- diag_ratio:

  Proportion of the diagonal distance (ratio of distance to the full
  diagonal distance) across the bounding box.

- tolerance:

  Numeric value above or below 1 used by `sf_bbox_orientation()` to
  describe an aspect ratio as landscape or portrait; defaults to 0.1.

- dist:

  Distance to compare with bounding box for `sf_bbox_check_fit()`. If
  dist is length 1 compare to bounding box diagonal distance. If dist is
  length 2, compare to bounding box x and y distances. Return TRUE if
  dist fits within bounding box or FALSE if dist exceeds bounding box
  limits.

## See also

Other dist:
[`compare_dist()`](https://elipousson.github.io/sfext/reference/compare_dist.md),
[`convert_dist_scale()`](https://elipousson.github.io/sfext/reference/convert_dist_scale.md),
[`convert_dist_units()`](https://elipousson.github.io/sfext/reference/convert_dist_units.md),
[`get_measurements`](https://elipousson.github.io/sfext/reference/get_measurements.md),
[`is_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
