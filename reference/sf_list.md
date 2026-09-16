# Creating and checking sf lists

Functions for coercing an object to an sf_list class objects created by
[`vctrs::new_list_of()`](https://vctrs.r-lib.org/reference/new_list_of.html)
and checking lists of sf objects. Any function with the `allow_list`
parameter supports this type of input.

## Usage

``` r
as_sf_list(
  x,
  nm = "data",
  col = NULL,
  crs = NULL,
  clean_names = TRUE,
  .name_repair = "check_unique",
  call = caller_env()
)

new_sf_list(
  x,
  nm = "data",
  col = NULL,
  clean_names = TRUE,
  .name_repair = "check_unique",
  call = caller_env()
)

is_sf_list(x, ext = TRUE, allow_null = FALSE)

sf_list_rbind(x, ...)

map_as_sf_list(x, .f, ...)

map_as_sf(x, .f, ...)
```

## Arguments

- x:

  An `sf`, `sfc`, or `bbox` object.

- nm:

  For as_sf_list, name(s) for sf list; defaults to "data". If col is
  provided, the values of the grouping column are used as names.

- col:

  For as_sf_list, the name of the column used to group data if x is a sf
  object or used to group and nest data before passing to x.

- crs:

  A character or numeric reference to a coordinate reference system
  supported by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  or another `sf`, `sfc`, or `bbox` object that is used to provide crs.

- clean_names:

  If `TRUE`, clean names provided to nm or created based on value of col
  using
  [janitor::clean_names](https://sfirke.github.io/janitor/reference/clean_names.html).
  If `FALSE`, use names as provided.

- .name_repair:

  One of "unique", "universal", or "check_unique". See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for the meaning of these options.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- ext:

  If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or not;
  defaults to `FALSE`. (used by
  [is_sf](https://elipousson.github.io/sfext/reference/is_sf.md))

- allow_null:

  If `TRUE` and x is `NULL`, return `TRUE`; defaults to `FALSE`.

- ...:

  For `sf_list_rbind()`, additional parameters passed to
  [`purrr::list_rbind()`](https://purrr.tidyverse.org/reference/list_c.html).
  For `map_as_sf()`, additional parameters passed to map.

- .f:

  A function, specified in one of the following ways:

  - A named function, e.g. `mean`.

  - An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.

  - A formula, e.g. `~ .x + 1`. Use `.x` to refer to the first argument.
    No longer recommended.

  - A string, integer, or list, e.g. `"idx"`, `1`, or `list("idx", 1)`
    which are shorthand for `\(x) pluck(x, "idx")`, `\(x) pluck(x, 1)`,
    and `\(x) pluck(x, "idx", 1)` respectively. Optionally supply
    `.default` to set a default value if the indexed element is `NULL`
    or does not exist.

  **\[experimental\]**

  Wrap a function with
  [`in_parallel()`](https://purrr.tidyverse.org/reference/in_parallel.html)
  to declare that it should be performed in parallel. See
  [`in_parallel()`](https://purrr.tidyverse.org/reference/in_parallel.html)
  for more details. Use of `...` is not permitted in this context.

## See also

[`is_sf()`](https://elipousson.github.io/sfext/reference/is_sf.md)
