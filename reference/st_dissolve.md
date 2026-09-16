# Dissolve geometry preserving existing or supplied grouping variables

`st_dissolve()` dissolves sf and sfc objects. If the input object is an
sf object, any existing grouping variables or added grouping variables
passed to the `.by` parameter are included in the output sf data frame.

## Usage

``` r
st_dissolve(
  x,
  ...,
  .by = NULL,
  .keep = "nest",
  do_union = TRUE,
  .data_key = "data",
  .dissolve_key = "group.comp.id",
  call = caller_env()
)
```

## Arguments

- x:

  A sf or sfc object. If sf object, a grouped data frame is allowed.

- ...:

  Arguments passed on to
  [`spdep::poly2nb`](https://r-spatial.github.io/spdep/reference/poly2nb.html)

  `pl`

  :   list of polygons of class extending `SpatialPolygons`, or an `sf`
      or `sfc` object containing non-empty (multi-)polygon objects

  `row.names`

  :   character vector of region ids to be added to the neighbours list
      as attribute `region.id`, default `seq(1, nrow(x))`; if `pl` has
      `row.names`, they are used instead of the default sequence.

  `snap`

  :   boundary points less than `snap` distance apart are considered to
      indicate contiguity; used both to find candidate and actual
      neighbours for planar geometries, but only actual neighbours for
      spherical geometries, as spherical spatial indexing itself injects
      some fuzzyness. If not set, for all `SpatialPolygons` objects, the
      default is as before `sqrt(.Machine$double.eps)`, with this value
      also used for `sf` objects with no coordinate reference system.
      For `sf` objects with a defined coordinate reference system, the
      default value is `1e-7` for geographical coordinates
      (approximately 10mm), is 10mm where projected coordinates are in
      metre units, and is converted from 10mm to the same distance in
      the units of the coordinates. Should the conversion fail, `snap`
      reverts to `sqrt(.Machine$double.eps)`.

  `queen`

  :   if TRUE, a single shared boundary point meets the contiguity
      condition, if FALSE, more than one shared point is required; note
      that more than one shared boundary point does not necessarily mean
      a shared boundary line

  `useC`

  :   default TRUE, doing the work loop in C, may be set to false to
      revert to R code calling two C functions in an `n*k` work loop,
      where `k` is the average number of candidate neighbours

  `foundInBox`

  :   default NULL using R code or
      [`st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
      to generate candidate neighbours (using `snap=` if the geometries
      are not spherical); if not NULL (for legacy purposes) a list of
      length `(n-1)` with integer vectors of candidate neighbours
      `(j > i)` (as created by the `poly_findInBoxGEOS` function in
      rgeos for clean polygons)

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .keep:

  Method for handling attributes for input data. By default ("nest"),
  input data is converted to a nested list column with the name from
  `.data_key`. Any other values

- do_union:

  Use
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  to dissolve geometry if `TRUE` (default). Use
  [`sf::st_combine()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  if `FALSE`.

- .data_key:

  Passed to `.data` argument of
  [`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html)

- .dissolve_key:

  Used as the column name for the dissolve grouping variable. x can't
  have any existing columns with this name.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
