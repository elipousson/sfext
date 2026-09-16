# Convert an sf, numeric, or other object to a POINT (sfg) or POINT, MULTIPOINT, LINESTRING, or MULTILINESTRING (sfc) object

Works with sf, sfc, and bbox objects using
[`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html).
Works with
[`sf_bbox_point()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)

## Usage

``` r
as_point(..., to = "POINT")

as_points(..., to = "POINT", call = caller_env())

as_startpoint(x)

as_endpoint(x)

as_line(..., to = "LINESTRING", call = caller_env())

as_lines(..., to = "LINESTRING")

as_polygons(..., to = "POLYGON")

as_centroid(x, ...)
```

## Arguments

- ...:

  See details.

- to:

  The geometry type to return, either POINT or MULTIPOINT or LINESTRING
  or MULTILINESTRING.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- x:

  A `sf`, `sfc`, or `bbox` object.

## Details

Using as_point:

`as_point()` always returns a single point sfg object. The ... parameter
is passed to
[`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html)
if ... is a sf, sfc, or bbox object,
[`sf_bbox_point()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
includes a bbox object and a string indicating the requested point
position, or
[`sf::st_point()`](https://r-spatial.github.io/sf/reference/st.html) if
... includes a numeric vector.

Using as_points:

`as_points()` always returns an sfc object. The parameters are passed to
as_point using
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) and
then converted to sfc using
[`sf::st_as_sfc()`](https://r-spatial.github.io/sf/reference/st_as_sfc.html).
The ... parameters must include a crs, otherwise the crs will be NA for
the resulting sfc object.

Using as_startpoint and as_endpoint:

`as_startpoint()` and `as_endpoint()` require a LINESTRING OR
MULTILINESTRING geometry type sf or sfc object that is passed to
[`lwgeom::st_startpoint()`](https://r-spatial.github.io/lwgeom/reference/st_startpoint.html)
or
[`lwgeom::st_endpoint()`](https://r-spatial.github.io/lwgeom/reference/st_startpoint.html)
respectively. Both functions always return a sfc object matching the CRS
of the input geometry.

Using as_lines:

If params do not have POINT or MULTIPOINT geometry, they are passed to
`as_points()` to convert to an `sfc` object. If the parameters have
POINT geometry, they are combined to create a MULTIPOINT geometry.

For `as_lines()` the ... parameters are passed to `as_points()` and/or
[`sf::st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.html).

Both as_line and as_lines do not consistently retain the coordinate
reference system of the original object but this should be improved in
the future.

Using `as_centroid()`

`as_centroid()` always returns a sfc object with the same length and crs
as the input object.

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

as_point(nc)
#> POINT (-79.40065 35.55937)

as_point(c("xmax", "ymax"), bbox = as_bbox(nc))
#> POINT (-75.45698 36.58965)

as_points(nc)
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -79.40065 ymin: 35.55937 xmax: -79.40065 ymax: 35.55937
#> Geodetic CRS:  NAD27
#> POINT (-79.40065 35.55937)

as_points(nc[1, ], nc[2, ])
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -81.49823 ymin: 36.4314 xmax: -81.12513 ymax: 36.49111
#> Geodetic CRS:  NAD27
#> POINT (-81.49823 36.4314)
#> POINT (-81.12513 36.49111)

nc_line <- as_line(c(as_points(nc[1, ]), as_points(nc[10, ])))

as_startpoint(nc_line)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -81.49823 ymin: 36.4314 xmax: -81.49823 ymax: 36.4314
#> Geodetic CRS:  NAD27
#> POINT (-81.49823 36.4314)

as_endpoint(nc_line)
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -80.23429 ymin: 36.40042 xmax: -80.23429 ymax: 36.40042
#> Geodetic CRS:  NAD27
#> POINT (-80.23429 36.40042)

as_centroid(nc)
#> Geometry set for 100 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -84.05986 ymin: 34.07671 xmax: -75.8095 ymax: 36.49111
#> Geodetic CRS:  NAD27
#> First 5 geometries:
#> POINT (-81.49823 36.4314)
#> POINT (-81.12513 36.49111)
#> POINT (-80.68573 36.41252)
#> POINT (-76.02719 36.40714)
#> POINT (-77.41046 36.42236)
```
