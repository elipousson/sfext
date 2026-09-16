# General utility functions for working with distance units objects

- `is_dist_units()`: Is x a distance unit object?

- `is_diff_dist()`: What is the difference between x and y distance?

- `is_same_dist()`: Is x the same distance as y? or does the bbox of x
  and bbox of y have the same x, y, or diagonal distance?

- `is_shorter()`, `is_longer()`: Is x shorter or longer than y?

- `is_same_area()`: do x and y have the same area?

- `is_same_units()`: are x and y character strings that represent the
  same units or objects that use the same units?

## Usage

``` r
is_dist_units(x)

is_diff_dist(x, y, units = NULL)

is_same_dist(x, y, dist = NULL, diff = FALSE, call = caller_env(), ...)

is_longer(x, y)

is_shorter(x, y)

get_dist_units(x, allow_null = TRUE, multiple = TRUE, quiet = FALSE)

as_dist_units(x, units = NULL, allow_null = FALSE, call = caller_env())

is_diff_area(x, y, units = NULL, combine = TRUE)

is_same_area(x, y, units = NULL, combine = TRUE, diff = FALSE, ...)

is_same_units(x, y = NULL)
```

## Arguments

- x, y:

  objects to check

- units:

  For is_diff_dist, if x and y are both not units objects, use units;
  default to `NULL`.

- dist:

  type of distance to compare if x and y are `sf`, `sfc`, or `bbox`
  objects; "diagdist", "xdist", "ydist". defaults to `NULL`.

- diff:

  If `TRUE`, return results from is_diff_dist or is_diff_area; if
  `FALSE`, return logical indicator; defaults to `FALSE`

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- ...:

  Additional parameters passed to
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html)

- allow_null:

  If allow_null is `TRUE`, allow x to return a `NULL` value; if `FALSE`,
  error on `NULL` values.

- multiple:

  If `TRUE` and x is a character vector with distance/area units,
  get_dist_units may return multiple units. Passed to
  [rlang::arg_match](https://rlang.r-lib.org/reference/arg_match.html).

- quiet:

  If `TRUE`, suppress warning messages.

- combine:

  If `TRUE`, combine objects with
  [`sf::st_combine()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  before comparing area with `is_diff_area()` or `is_same_area()`,
  defaults to `TRUE`.

## Details

There are two additional functions that support these utility functions:

- `get_dist_units()`: Get the distance units from x (if x is a sf or
  units objects or a character string from
  [dist_unit_options](https://elipousson.github.io/sfext/reference/dist_unit_options.md))

- `as_dist_units()`: Convert x to units using
  [units::as_units](https://r-quantities.github.io/units/reference/units.html)

## See also

Other dist:
[`compare_dist()`](https://elipousson.github.io/sfext/reference/compare_dist.md),
[`convert_dist_scale()`](https://elipousson.github.io/sfext/reference/convert_dist_scale.md),
[`convert_dist_units()`](https://elipousson.github.io/sfext/reference/convert_dist_units.md),
[`get_measurements`](https://elipousson.github.io/sfext/reference/get_measurements.md),
[`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
