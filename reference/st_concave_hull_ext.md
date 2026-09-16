# Make a concave hull around simple feature object by attribute

Make a concave hull around simple feature object by attribute

## Usage

``` r
st_concave_hull_ext(
  x,
  by = NULL,
  centroid = FALSE,
  ratio = 0.5,
  allow_holes = FALSE
)
```

## Arguments

- x:

  A sf object.

- by:

  Column name to use for grouping and combining geometry, Default:
  `NULL`

- centroid:

  If `TRUE`, use centroids for geometry of x, Default: `FALSE`

- ratio:

  numeric; fraction convex: 1 returns the convex hulls, 0 maximally
  concave hulls

- allow_holes:

  logical; if `TRUE`, the resulting concave hull may have holes
