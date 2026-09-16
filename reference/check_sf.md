# Check if x is an sf object

If x is an `sf` object invisibly return TRUE. If not, return an error
with [cli::cli_abort](https://cli.r-lib.org/reference/cli_abort.html)

## Usage

``` r
check_sf(
  x,
  ...,
  ext = FALSE,
  allow_list = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  An `sf`, `sfc`, or `bbox` object.

- ...:

  Additional parameters passed to
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html).

- ext:

  If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or not;
  defaults to `FALSE`. (used by
  [is_sf](https://elipousson.github.io/sfext/reference/is_sf.md))

- allow_list:

  If `TRUE`, return `TRUE` if x is an sf list or, if ext is also `TRUE`,
  a list of sf, sfc, or bbox objects. Defaults to `FALSE`.

- allow_null:

  If `TRUE` and x is `NULL`, return `TRUE`; defaults to `FALSE`.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
