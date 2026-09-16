# What is the class or spatial attributes of this feature?

What is the class or spatial attributes of this feature?

## Usage

``` r
is_sf(x, ext = FALSE, allow_null = FALSE, allow_list = FALSE)

is_sfg(x, allow_null = FALSE)

is_sfc(x, allow_null = FALSE)

is_bbox(x, allow_null = FALSE, allow_na = FALSE)

is_raster(x, allow_null = FALSE)

is_sp(x, allow_null = FALSE)

is_geo_coords(x, allow_null = FALSE)
```

## Arguments

- x:

  An `sf`, `sfc`, or `bbox` object.

- ext:

  If `TRUE`, check if x is a `sf`, `sfc`, or `bbox` class object or not;
  defaults to `FALSE`. (used by is_sf)

- allow_null:

  If `TRUE` and x is `NULL`, return `TRUE`; defaults to `FALSE`.

- allow_list:

  If `TRUE`, is_sf will return TRUE if x is a list of sf objects.

- allow_na:

  If `TRUE`, `is_bbox()` ignores any `NA` values. Defaults to `FALSE`.

## Details

- is_sf: is x a `sf` class object?

- is_sfc: is x is a `sfc` class object?

- is_bbox: is x is a `bbox` class object?

- [is_sf_list](https://elipousson.github.io/sfext/reference/sf_list.md):
  is x is a list of `sf` class objects (with or without names)?

- is_raster: is x a `Raster` class object?

- is_sp: is x a `Spatial` class object of any type?

- is_geo_coords: is x likely a geodetic coordinate pair (a length 2
  numeric vector, with a max absolute value less than or equal to 180)?

- [is_wgs84](https://elipousson.github.io/sfext/reference/as_crs.md): is
  x using the
  [WSG84](https://en.wikipedia.org/wiki/World_Geodetic_System)
  coordinate reference system?

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

is_sf(nc)
#> [1] TRUE

is_sfc(nc$geometry)
#> [1] TRUE

is_sf_list(list(nc[1, ], nc[2, ]))
#> [1] TRUE

is_bbox(sf::st_bbox(nc))
#> [1] TRUE

is_wgs84(sf::st_transform(nc, 4326))
#> [1] TRUE

is_same_crs(nc, 4267)
#> [1] TRUE
```
