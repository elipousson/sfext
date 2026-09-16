# Bind units column to data frame

Utility function supporting
[get_area](https://elipousson.github.io/sfext/reference/get_measurements.md),
[get_dist](https://elipousson.github.io/sfext/reference/get_measurements.md),
and
[get_bearing](https://elipousson.github.io/sfext/reference/get_measurements.md).

## Usage

``` r
bind_units_col(
  x,
  y,
  units = NULL,
  drop = FALSE,
  keep_all = TRUE,
  .id = NULL,
  call = caller_env()
)
```

## Arguments

- x:

  Data frame or sf object.

- y:

  Vector of numeric or units values to bind to x.

- units:

  Units to use for y (if numeric) or convert to (if y is units class);
  defaults to `NULL`.

- drop:

  If `TRUE`, apply the
  [units::drop_units](https://r-quantities.github.io/units/reference/drop_units.html)
  function to the column with units class values and return numeric
  values instead; defaults to `FALSE`.

- keep_all:

  If `FALSE`, keep all columns. If `FALSE`, return only the named .id
  column.

- .id:

  Name to use for vector of units provided to "y" parameter, when "y" is
  bound to the "x" data frame or tibble as a new column.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
