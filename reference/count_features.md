# Count simple features based on relationship with a second simple feature object

Use
[`st_join_ext()`](https://elipousson.github.io/sfext/reference/st_join_ext.md)
and [`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
to count features in x based on their spatial relationship with y. Very
similar to
[`count_sf_ext()`](https://elipousson.github.io/sfext/reference/count_sf_ext.md)
so they may be merged in the future.

## Usage

``` r
count_features(
  x = NULL,
  y = NULL,
  nm = "data",
  join = NULL,
  .id = "name",
  by = NULL,
  count = NULL,
  sort = FALSE,
  name = NULL,
  geometry = "y",
  ...
)
```

## Arguments

- x:

  Data frame or `sf` object, Default: `NULL`

- y:

  Length 1 named `sf` list (name of y is used as count if count is
  `NULL`) or a `sf` object if nm is not `NULL`. y must include a column
  name matching the value of .id. If y is `NULL`, count is required.
  Default: `NULL`

- nm:

  Vector of names to use with
  [`as_sf_list()`](https://elipousson.github.io/sfext/reference/sf_list.md)
  to convert y to an sf list (if y is not already an sf list). Defaults
  to "data".

- join:

  geometry predicate function; defaults to `NULL`, set to
  [`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
  if y contains only POLYGON or MULTIPOLYGON objects or
  [`sf::st_nearest_feature()`](https://r-spatial.github.io/sf/reference/st_nearest_feature.html)
  if y contains other types.

- .id:

  Column name with the values that should be added as a column to the
  input `sf` object.

- by:

  A character vector of variables to join by passed to
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html).

- count:

  Name of column to count. If `NULL`, count is set to `names(y)`
  (assuming y is a length 1 named `sf` list).

- sort:

  If `TRUE`, will show the largest groups at the top.

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

- geometry:

  If "y", replace x geometry with y geometry joining based on by. If by
  is `NULL`, by is set to the same value as count.

- ...:

  Additional parameters passed to
  [`st_join_ext()`](https://elipousson.github.io/sfext/reference/st_join_ext.md)
