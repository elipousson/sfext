# Compare a distance to the dimension of a bounding box or another distance value

Compare dist to a distance based on x. If to is "xdist" (default),
"ydist", "diagdist", or any length 2 character vector supported by
[`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
dist is compared to the resulting distance. If to is an sf or sfc
object, the comparison distance is the results from
[`sf::st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
with x as the first parameter and to as the second. x can also be a
numeric value with or without a units class. Use the "how" argument to
determine the type of comparison: "ratio" (dist divided by comparison
distance) "fit" (how many lengths of dist can fit wholly within the
comparison distance) "longer", "shorter", "same" (uses tolerance
parameter).

## Usage

``` r
compare_dist(
  dist,
  x,
  units = NULL,
  to = "xdist",
  how = "ratio",
  tolerance = 1.5e-08,
  ...
)
```

## Arguments

- dist:

  Distance to check as a units class or numeric vector

- x:

  A `sf`, `sfc`, or `bbox` object or a numeric vector.

- units:

  Units to compare. If dist or x are numeric they are assumed to be in
  the provided units or the units of the coordinate reference system
  (CRS) for x (if x is a `sf`, `sfc`, or `bbox` object).

- to:

  Distance to compare dist with. Standard options include "xdist"
  (default), "ydist", or "diagdist" to compare to
  [`sf_bbox_xdist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md),
  [`sf_bbox_ydist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md),
  or
  [`sf_bbox_diagdist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md).
  If to is a length 2 character vector, x, to, and any ... parameters
  are passed to
  [`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md).
  If to is an sf or sfc object, to and any additional ... parameters are
  passed to
  [`sf::st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
  using x as the first argument and to as the second. If x is a numeric
  vector the value of to is ignored.

- how:

  If "ratio", dist divided by the comparison distance. If what is "fit",
  return the number of times dist fits in the comparison distance
  without remainder. If "longer", "shorter", "same", return a logical
  vector.

- tolerance:

  passed to [`all.equal()`](https://rdrr.io/r/base/all.equal.html) if
  `how = "same"`

- ...:

  Additional parameters passed to
  [`sf::st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
  if to is an sf or sfc object or
  [`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  if to is a length 2 character vector.

## Value

A logical or numeric vector.

## See also

Other dist:
[`convert_dist_scale()`](https://elipousson.github.io/sfext/reference/convert_dist_scale.md),
[`convert_dist_units()`](https://elipousson.github.io/sfext/reference/convert_dist_units.md),
[`get_measurements`](https://elipousson.github.io/sfext/reference/get_measurements.md),
[`is_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md),
[`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
