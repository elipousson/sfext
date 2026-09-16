# Transform or convert coordinates of a simple feature or bounding box object

This function wraps
[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)
but supports a wider range of input objects and, using the
[`as_sf_class()`](https://elipousson.github.io/sfext/reference/as_sf.md)
function, returns a wider range of objects. Typically, takes a `sf`,
`sfc`, or `bbox` object and transform to coordinate reference system to
match the value of crs or the object provided to crs. If `x` is a
data.frame or if `x` is `NULL` and `allow_null` is `TRUE` (defaults to
`FALSE`) it is returned as is.

## Usage

``` r
st_transform_ext(
  x,
  crs = NULL,
  class = NULL,
  rotate = 0,
  allow_null = FALSE,
  allow_list = TRUE
)

st_omerc(x, rotate = 0)

st_wgs84(x)
```

## Arguments

- x:

  An `sf`, `sfc`, or `bbox` object, a list of sf objects, or a
  `data.frame` object (always returned as is).

- crs:

  A character or numeric reference to a coordinate reference system
  supported by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  or another `sf`, `sfc`, or `bbox` object that is used to provide crs.

- class:

  Class of object to return (`sf` or `bbox`). If x is an sf list, the
  returned object remains a list but may be converted to `bbox` if
  `class = "sf"`.

- rotate:

  If rotate is greater or less than 0, `st_transform_ext()` calls
  `st_omerc()` and returns an object with the Oblique Mercator
  projection passing the value of rotate to the gamma parameter of the
  projection. rotate must be between -45 and 45 degrees.

- allow_null:

  If `TRUE` and x is `NULL` return x without an error. Defaults to
  `FALSE`.

- allow_list:

  If `TRUE`, x can be a list of `sf`, `sfc`, or `bbox` objects. If
  `FALSE`, only `sf`, `sfc`, or `bbox` objects are supported. Defaults
  to `TRUE`.

## Value

An `sf`, `sfc`, or `bbox` object transformed to a new coordinate
reference system.

## See also

[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html),[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

nc_bbox <- sf::st_bbox(nc)

nc_3857 <- st_transform_ext(nc, 3857)

st_transform_ext(nc_bbox, crs = 4326)
#>      xmin      ymin      xmax      ymax 
#> -84.32377  33.88212 -75.45662  36.58973 

sf::st_crs(st_transform_ext(nc_3857, crs = nc))$input
#> [1] "NAD27"

sf::st_crs(st_wgs84(nc_3857))$input
#> [1] "EPSG:4326"
```
