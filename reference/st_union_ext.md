# Union simple feature objects and combine name column values

Wrapper for
[`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
supporting the additional feature of a combined name column collapsed
into a single vector with
[`cli::pluralize()`](https://cli.r-lib.org/reference/pluralize.html).
`st_union_by()` wraps
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
and
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
to allow quick unioning geometry by the passed parameters.

## Usage

``` r
st_union_ext(
  x,
  y = NULL,
  name_col = "name",
  .sf_col = NULL,
  label = NULL,
  ext = TRUE,
  ...
)

st_union_by(x, ..., .sf_col = NULL)
```

## Arguments

- x:

  A `sf`, `sfc`, or `bbox` object or `sf` only for `st_union_by()`
  Required.

- y:

  A `sf` or `sfc` object, defaults to `NULL`.

- name_col:

  Column name to collapse into new name_col value, Default: 'name'

- .sf_col:

  Geometry column name to use if x is an `sf` object, Default: `NULL`.

- label:

  Length 1 character vector used if name_col is `NULL`.

- ext:

  If `FALSE`, `st_union_ext()` functions the same as
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html),
  Default: `TRUE`

- ...:

  Additional parameters passed to
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  or
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Value

A `sfc` object if y is `NULL` and ext is `FALSE`. A tibble sf if x is a
sf object and y is `NULL`. If y is provided, `st_union_ext()` is
identical to
[`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)

## Examples

``` r
nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
nc_union <- st_union_ext(nc[10:15, ], name_col = "NAME")
nc_union
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -80.45301 ymin: 36.01413 xmax: -78.28293 ymax: 36.55104
#> Geodetic CRS:  NAD27
#> # A tibble: 1 × 2
#>   NAME                                                                  geometry
#>   <chr>                                                            <POLYGON [°]>
#> 1 Stokes, Caswell, Rockingham, Granville, Person, and… ((-79.2585 36.23569, -79…
plot(nc_union)
```
