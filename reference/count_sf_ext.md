# Count extended for working with sf objects

An extended version of
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
that makes it easier to count the occurrences of features from data that
intersect with features from a second sf object (set by y) or created by
passing x or data to
[`st_make_grid_ext()`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.md).
Similar to
[`count_features()`](https://elipousson.github.io/sfext/reference/count_features.md)
and the two functions may be combined in the future.

## Usage

``` r
count_sf_ext(
  data,
  x = NULL,
  y = NULL,
  join = sf::st_intersects,
  largest = TRUE,
  wt = NULL,
  sort = FALSE,
  replace_na = FALSE,
  keep_na = FALSE,
  lims = NULL,
  geometry = TRUE,
  .id = "id",
  name = NULL,
  ...
)
```

## Arguments

- data:

  Data to count in relationship to y

- x:

  Optional sf object passed to
  [`st_make_grid_ext()`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.md).
  Defaults to `NULL`.

- y:

  If `NULL` (default), y defaults to an `sf` object created by
  [`st_make_grid_ext()`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.md)
  using x or data (if x is `NULL`) as the x parameter for
  [`st_make_grid_ext()`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.md).
  If not `NULL`, y must be an `sf` object that has a column with the
  same name as .id (defaults to "id").

- join:

  geometry predicate function with the same profile as
  [st_intersects](https://r-spatial.github.io/sf/reference/geos_binary_pred.html);
  see details

- largest:

  logical; if `TRUE`, return `x` features augmented with the fields of
  `y` that have the largest overlap with each of the features of `x`;
  see https://github.com/r-spatial/sf/issues/578

- wt:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Frequency weights. Can be `NULL` or a variable:

  - If `NULL` (the default), counts the number of rows in each group.

  - If a variable, computes `sum(wt)` for each group.

- sort:

  If `TRUE`, will show the largest groups at the top.

- replace_na:

  If `TRUE`, replace NA values from count with 0.

- keep_na:

  If `TRUE`, filter NA values from count. Ignored if replace_na is
  `TRUE`.

- lims:

  Optional numeric vector with minimum or both minimum and maximum count
  values. If provided, any values below the minimum are set to that
  minimum and any values above the maximum as set to the maximum. If
  only one value is provided, it is assumed to be a minimum limit.

- geometry:

  If `TRUE` (default) return a `sf` object. If `FALSE`, return a data
  frame.

- .id:

  A name to use for the cell id column. Defaults to "id".

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

- ...:

  Arguments passed on to
  [`st_make_grid_ext`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.md)

  `ncol,nrow`

  :   Used to set n if either are not `NULL`; defaults to `NULL`. row
      and id are added as columns to the grid if they are provided.

  `gutter`

  :   Distance in units between each column cell; gutter effectively
      serves as a margin as the negative buffer is applied to all cells
      (including those at the edges of the grid).

  `desc`

  :   If TRUE, reverse standard order of cell id numbering; defaults
      `FALSE`

  `n`

  :   If n is NULL and square is `TRUE`, the grid is set automatically
      to be 10 cells wide, Default: `NULL`

  `what`

  :   "polygons", "corners", "centers"; set to centers automatically if
      style is "circle", "circle_offset" but a buffer is applied to
      return circular polygons.

  `style`

  :   Style of cell to return with options including "rect", "square",
      "hex", "flat_top_hex", "circle", "circle_offset"

  `filter`

  :   If `TRUE` (or if trim is `TRUE`) filter grid geometry by x using
      [st_filter_ext](https://elipousson.github.io/sfext/reference/st_filter_ext.md)

  `crs`

  :   Coordinate reference system of bounding box to return; defaults to
      `NULL` which maintains the crs of the input object.

  `unit`

  :   Units for buffer. Supported options include "meter", "foot",
      "kilometer", and "mile", "nautical mile" Common abbreviations
      (e.g. "km" instead of "kilometer") are also supported. Distance in
      units is converted to units matching GDAL units for x; defaults to
      "meter"

  `cellsize`

  :   numeric of length 1 or 2 with target cellsize: for square or
      rectangular cells the width and height, for hexagonal cells the
      distance between opposite edges (edge length is cellsize/sqrt(3)).
      A length units object can be passed, or an area unit object with
      area size of the square or hexagonal cell.

  `trim`

  :   If `TRUE`, x is trimmed to y with
      [`st_trim()`](https://elipousson.github.io/sfext/reference/st_erase.md).

## Value

A sf object or a tibble (if `geometry = FALSE`) with a column counting
occurrences of features from data.

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
data <- sf::st_sample(nc, size = 75)

# Count data based on nc
count <- count_sf_ext(data = data, y = nc, .id = "FIPS")
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
plot(count[, "n"], reset = FALSE)
plot(data, col = "gray60", pch = 3, add = TRUE)


# Count data based grid created by passing nc to st_make_grid_ext
count_grid <- count_sf_ext(data = data, x = nc, .id = "FIPS")
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
plot(count_grid[, "n"], reset = FALSE)
plot(data, col = "gray60", pch = 3, add = TRUE)

```
