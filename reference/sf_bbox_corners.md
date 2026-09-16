# Get bounding box corner points from a bbox, sfc, or sf object

If x is an sf object, the results include a column with a name matching
.id containing the values "SW", "SE", "NE", and "NW" corresponding to
the cardinal position of each corner.

## Usage

``` r
sf_bbox_corners(x, .id = "corner", class = "sf")
```

## Arguments

- x:

  A `bbox`, length 1 `sfc` object, or a `sf` object with a single
  feature

- .id:

  A name to use for added column with cardinal position of points,
  Default: 'corner'

- class:

  Class of object to return. Must be "sf" (default) or "sfc".

## Value

A `sf` with four features (duplicating input feature with new geometry
derived from bounding box) or a length 4 `sfc` object with POINT
geometry.
