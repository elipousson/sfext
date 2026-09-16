# Scale and rotate a simple feature object, simple feature collection, or bounding box

Scale or rotate a simple feature or bounding box object using affine
transformations.

## Usage

``` r
st_scale_rotate(x, scale = 1, rotate = 0, call = caller_env())
```

## Arguments

- x:

  A `sf`, `sfc`, or `bbox` object or another object coercible to a
  simple feature collection with
  [`as_sfc()`](https://elipousson.github.io/sfext/reference/as_sf.md).

- scale:

  numeric; scale factor, Default: 1

- rotate:

  numeric; degrees to rotate (-360 to 360), Default: 0

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
nc <- sf::st_transform(nc, crs = 3857)

plot(st_scale_rotate(nc, scale = 0.75, rotate = 15), max.plot = 1)

```
