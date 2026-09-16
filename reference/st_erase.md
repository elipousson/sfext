# Erase or trim geometry of a sf or sfc object

`st_erase()` extends
[`sf::st_difference()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
by unioning the second parameter by default, checking validity of
inputs, and optionally (when `flip = TRUE`) using
[`sf::st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
instead of
[sf::st_difference](https://r-spatial.github.io/sf/reference/geos_binary_ops.html).
`st_trim()` is equivalent to `st_erase()` with flip set to `TRUE`.

## Usage

``` r
st_erase(x, y, flip = FALSE, union = TRUE, combine = FALSE, ...)

st_trim(x, y, union = TRUE, combine = FALSE, ...)
```

## Arguments

- x:

  A `sf`, `sfc`, or `bbox` object to erase or trim.

- y:

  A `sf`, `sfc`, or `bbox` object to use to erase or trim.

- flip:

  If `TRUE`, use
  [`sf::st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
  to "erase" geometry of x that intersects y; if `FALSE` use
  [`sf::st_difference()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
  to trim x to y geometry, Default: `FALSE`.

- union:

  If `TRUE`, union `y` with
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  before applying difference/intersection; defaults to `TRUE`.

- combine:

  If `TRUE` and `union = TRUE`, combine `y` with
  [`sf::st_combine()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  before unioning. Defaults to `FALSE`.

- ...:

  arguments passed on to
  [s2_options](https://r-spatial.github.io/s2/reference/s2_options.html)

## Examples

``` r
nc <- read_sf_ext(system.file("shape/nc.shp", package = "sf"))

nc <- st_transform_ext(nc, 3657)

plot(
  st_erase(
    sf::st_buffer(nc[1, ], 1000),
    nc[1, ]
  ),
  max.plot = 1
)


plot(
  st_trim(
    nc,
    sf::st_buffer(nc[1, ], 2000)
  ),
  max.plot = 1
)
```
