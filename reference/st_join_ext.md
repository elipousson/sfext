# Complete a spatial join using a simple feature objects or an object and list

Wrapper for
[`sf::st_join()`](https://r-spatial.github.io/sf/reference/st_join.html)
that works with sf lists.

## Usage

``` r
st_join_ext(x, y, col = NULL, .id = "name", join = NULL, ...)
```

## Arguments

- x:

  A `sf`, `sfc`, or `bbox` object.

- y:

  An `sf` object with a column named "name" or a list of sf objects
  where all items in the list have a "name" column.

- col:

  Column name used to convert y into a `sf` list if it is not already.

- .id:

  Column name with the values that should be added as a column to the
  input `sf` object.

- join:

  geometry predicate function; defaults to `NULL`, set to
  [`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
  if y contains only POLYGON or MULTIPOLYGON objects or
  [`sf::st_nearest_feature()`](https://r-spatial.github.io/sf/reference/st_nearest_feature.html)
  if y contains other types.

- ...:

  Additional parameters passed to
  [`sf::st_join()`](https://r-spatial.github.io/sf/reference/st_join.html)
