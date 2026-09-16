# What geometry type is this feature?

`is_geom_type()` extends
[`sf::st_geometry_type()`](https://r-spatial.github.io/sf/reference/st_geometry_type.html)
to return a list with the standard output ("TYPES") and additional list
values with logical vectors from
[`sf::st_is()`](https://r-spatial.github.io/sf/reference/st_is.html) for
"POINTS" (passing "POINT" and "MULTIPOINT" as type), "POLYGONS" (passing
"POLYGON", "MULTIPOLYGON"), "LINESTRINGS" ("LINESTRING" and
"MULTILINESTRING"), "GEOMETRYCOLLECTION" and "OTHER".

`st_is_ext()` adds a by_geometry argument that passes the results of
[`sf::st_is()`](https://r-spatial.github.io/sf/reference/st_is.html) to
[`all()`](https://rdrr.io/r/base/all.html) if by_geometry is `FALSE`.

## Usage

``` r
is_geom_type(x, type = NULL, by_geometry = FALSE, ext = TRUE)

is_point(x, by_geometry = FALSE)

is_multipoint(x, by_geometry = FALSE)

is_line(x, by_geometry = FALSE)

is_multiline(x, by_geometry = FALSE)

is_polygon(x, by_geometry = FALSE)

is_multipolygon(x, by_geometry = FALSE)

st_is_ext(x, type = NULL, by_geometry = FALSE)
```

## Arguments

- x:

  A `sf` or `sfc` object passed to
  [`sf::st_geometry_type()`](https://r-spatial.github.io/sf/reference/st_geometry_type.html)

- type:

  If "POINT", check if geometry type is POINT. Same for all available
  geometry types; not case sensitive; Default: `NULL`

- by_geometry:

  Passed to
  [`sf::st_geometry_type()`](https://r-spatial.github.io/sf/reference/st_geometry_type.html);
  defaults to `FALSE`

- ext:

  For `is_geom_type()`, if ext `TRUE` and check is `NULL`, return a list
  with checks for POINTS, POLYGONS, LINESTRING, and the returned types.

## Value

- If ext is `FALSE` and type is `NULL`, returns vector with geometry
  types identical to
  [`sf::st_geometry_type()`](https://r-spatial.github.io/sf/reference/st_geometry_type.html).

- If ext is `TRUE`, returns a list and, if type is not `NULL`, returns a
  logical vector.
