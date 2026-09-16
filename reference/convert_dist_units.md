# Convert distance (and area) values between different units

Convert distance (and area) values between different units

## Usage

``` r
convert_dist_units(
  dist,
  from = NULL,
  to = "meter",
  drop = FALSE,
  digits = NULL
)
```

## Arguments

- dist:

  Numeric or units object

- from:

  Existing unit for dist, Default: `NULL`. If dist is a units object,
  the numerator is used as "from"

- to:

  Unit to convert distance to, Default: 'meter'

- drop:

  If `TRUE`, return numeric. If `FALSE`, return class units object.

- digits:

  Number of digits to include in result; defaults to `NULL`.

## Value

Object created by
[`units::set_units()`](https://r-quantities.github.io/units/reference/units.html)

## See also

Other dist:
[`compare_dist()`](https://elipousson.github.io/sfext/reference/compare_dist.md),
[`convert_dist_scale()`](https://elipousson.github.io/sfext/reference/convert_dist_scale.md),
[`get_measurements`](https://elipousson.github.io/sfext/reference/get_measurements.md),
[`is_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md),
[`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
