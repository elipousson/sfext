# Row bind coverage for a feature based on a coverage feature

**\[experimental\]**

## Usage

``` r
bind_sf_coverage(
  x,
  coverage,
  coverage_nm = NULL,
  .id = "name",
  x_arg = caller_arg(x),
  coverage_arg = caller_arg(coverage),
  id_arg = caller_arg(.id),
  combine = FALSE,
  error_call = caller_env()
)

st_make_valid_coverage(
  x,
  y,
  combine = FALSE,
  is_coverage = TRUE,
  .fn = sf::st_difference
)

st_make_valid_union(x, combine = FALSE, is_coverage = TRUE)
```

## Arguments

- x:

  A input sf object

- coverage:

  A sf, sfc, or bbox object that covers x.

- coverage_nm:

  Name to use for coverage feature assigned as value for .id column,
  Default: `NULL`

- .id:

  .id column name, must be present in x Default: 'name'

- x_arg, coverage_arg, id_arg:

  Argument names for error messages if `bind_sf_coverage()` is used as a
  wrapper function.

- combine:

  If `TRUE`, use
  [`sf::st_combine()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  before passing x to
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- y:

  Second parameter for `st_make_valid_coverage()`

- is_coverage:

  logical; if `TRUE`, use an optimized algorithm for features that form
  a polygonal coverage (have no overlaps)

- .fn:

  Defaults to sf::st_difference

## Value

A sf object or a sfc object

## Details

`bind_sf_coverage()` and the related helper functions
`st_make_valid_coverage()` and `st_make_valid_union()` take a sf object
and create a non-intersecting coverage area of a surrounding geography
defined by a second sf, sfc, or bbox object. This was originally
developed for the
[make_area_xwalk()](https://elipousson.github.io/getACS/reference/make_area_xwalk.html)
function in the getACS package.

## See also

[`vctrs::vec_bind()`](https://vctrs.r-lib.org/reference/vec_bind.html)
[`sf::valid()`](https://r-spatial.github.io/sf/reference/valid.html),
[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
