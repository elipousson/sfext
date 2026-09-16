# Cast geometry of a simple feature object or simple feature collection to another type

Wrapper for
[`sf::st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.html)
that currently supports casting MULTIPOLYGON to POLYGON or MULTIPOLYGON
or POLYGON to POINT or, if simplify = FALSE, can cast MULTIPOINT to
LINESTRING. This is not very different than the basic functionality of
st_cast but further development may improve the utility of this
function.

## Usage

``` r
st_cast_ext(x, to = "POINT", simplify = TRUE, ...)
```

## Arguments

- x:

  A `sf` or `sfc` object to cast to another type.

- to:

  character; target type, if missing, simplification is tried; when `x`
  is of type `sfg` (i.e., a single geometry) then `to` needs to be
  specified.

- simplify:

  If `TRUE`, simplify geometry type; defaults to `TRUE`.

- ...:

  Arguments passed on to
  [`sf::st_cast`](https://r-spatial.github.io/sf/reference/st_cast.html)

  `ids`

  :   integer vector, denoting how geometries should be grouped
      (default: no grouping)

  `group_or_split`

  :   logical; if TRUE, group or split geometries; if FALSE, carry out a
      1-1 per-geometry conversion.

  `warn`

  :   logical; if `TRUE`, warn if attributes are assigned to
      sub-geometries

  `do_split`

  :   logical; if `TRUE`, allow splitting of geometries in
      sub-geometries
