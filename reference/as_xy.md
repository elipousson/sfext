# Convert data to a data frame with X/Y coordinate pairs

Wraps
[`as_points()`](https://elipousson.github.io/sfext/reference/as_point.md),
[`as_sfc()`](https://elipousson.github.io/sfext/reference/as_sf.md), and
[`sf_bbox_point()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
to allow the conversion of sf, sfc, or sfg objects into a simple data
frame with X and Y columns matching the provided nm parameter.

## Usage

``` r
as_xy(x, bbox = NULL, crs = NULL, nm = c("x", "y"), ...)
```

## Arguments

- x:

  A length 2 character string or numeric coordinate pair or a `sf`,
  `sfc`, or a `bbox` object. If x is a character string (e.g. c("xmin",
  "ymax")), a sf, sfc, or bbox object must be provided to data argument.

- bbox:

  A bbox object or object that can be converted with
  [`as_bbox()`](https://elipousson.github.io/sfext/reference/as_sf.md)
  that is passed as bbox to
  [`sf_bbox_point()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
  when x is passed to the point parameter.

- crs:

  A character or numeric reference to a coordinate reference system
  supported by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  or another `sf`, `sfc`, or `bbox` object that is used to provide crs.

- nm:

  Column names to use for X and Y columns.

- ...:

  Additional parameters passed to
  [`sf_bbox_point()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md),
  [`as_points()`](https://elipousson.github.io/sfext/reference/as_point.md),
  or
  [`as_sfc()`](https://elipousson.github.io/sfext/reference/as_sf.md).
