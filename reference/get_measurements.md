# Get measurements for simple feature objects

Get measurements for simple feature objects

## Usage

``` r
get_area(x, units = NULL, keep_all = TRUE, drop = FALSE, .id = "area")

st_area_ext(x, units = NULL, keep_all = TRUE, drop = FALSE, .id = "area")

get_length(x, units = NULL, keep_all = TRUE, drop = FALSE, .id = "length")

st_length_ext(x, units = NULL, keep_all = TRUE, drop = FALSE, .id = "length")

get_dist(
  x,
  to,
  by_element = TRUE,
  units = NULL,
  drop = FALSE,
  keep_all = TRUE,
  .id = "dist",
  ...
)

st_distance_ext(
  x,
  to,
  by_element = TRUE,
  units = NULL,
  drop = FALSE,
  keep_all = TRUE,
  .id = "dist",
  ...
)

get_bearing(x, to = NULL, dir = FALSE, keep_all = TRUE, .id = "bearing")

st_bearing(x, to = NULL, dir = FALSE, keep_all = TRUE, .id = "bearing")
```

## Arguments

- x:

  A `sf` or `sfc` object to measure.

- units:

  Units to return for area, length, perimeter, or distance; Default:
  `NULL`

- keep_all:

  If `TRUE`, return all columns from the original object, Default:
  `TRUE`

- drop:

  If `TRUE`, drop units from the line lengths, Default: `FALSE`

- .id:

  Column name to use for area, line length/perimeter, distance, or
  bearing.

- to:

  A `sf`, `sfc`, or `bbox` object or a length 2 character vector. If
  "to" is an `sf` or `sfc` object, it must have either a single feature
  or the same number of features as x (if by_element is `TRUE`). If "to"
  is a character vector it must represent a valid xy pair using the
  following options: "xmin", "ymin", "xmax", "ymax", "xmid", "ymid".

- by_element:

  logical; if `TRUE`, return a vector with distance between the first
  elements of `x` and `y`, the second, etc; an error is raised if `x`
  and `y` are not the same length. If `FALSE`, return the dense matrix
  with all pairwise distances.

- ...:

  passed on to
  [s2_distance](https://r-spatial.github.io/s2/reference/s2_is_collection.html),
  [s2_distance_matrix](https://r-spatial.github.io/s2/reference/s2_closest_feature.html),
  or
  [s2_perimeter](https://r-spatial.github.io/s2/reference/s2_is_collection.html)

- dir:

  Logical indicator whether to include direction in bearing; If `FALSE`,
  return the absolute (positive) bearing value. If `TRUE`, return
  negative and positive bearing values. Default: `FALSE`.

## Details

Wrapper functions for
[sf::geos_measures](https://r-spatial.github.io/sf/reference/geos_measures.html):

- `get_area()`: Wraps on
  [`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
  but MULTIPOINT or MULTILINESTRING geometry is converted to a polygon
  using
  [`sf::st_polygonize()`](https://r-spatial.github.io/sf/reference/geos_unary.html)
  which is used to determine the coverage area.

- `get_length()`: Wraps to
  [`sf::st_length()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
  but POINT and MULTIPOINT geometry is converted to LINESTRING using
  [`as_lines()`](https://elipousson.github.io/sfext/reference/as_point.md).
  If x has POLYGON geometry, `lwgeom::st_perimeter()` is used to return
  the perimeter instead of the length.

- `get_dist()`: Wraps
  [`sf::st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
  but x is converted to a POINT using
  [`st_center()`](https://elipousson.github.io/sfext/reference/st_misc.md)
  and "to" can be a POINT, a sf object that can be converted to a POINT,
  or a character vector indicating a point on the overall bounding box
  for x.

Additional measurement functions:

- `get_bearing()`: Wraps
  [`geosphere::bearing()`](https://rdrr.io/pkg/geosphere/man/bearing.html).

## See also

Other dist:
[`compare_dist()`](https://elipousson.github.io/sfext/reference/compare_dist.md),
[`convert_dist_scale()`](https://elipousson.github.io/sfext/reference/convert_dist_scale.md),
[`convert_dist_units()`](https://elipousson.github.io/sfext/reference/convert_dist_units.md),
[`is_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md),
[`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)

## Examples

``` r
nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

# Get area for North Caroline counties
get_area(nc[1:2, ])$area
#> Units: [m^2]
#> [1] 1137107793  610916077
get_area(nc[1:2, ], units = "acres")$area
#> Units: [acres]
#> [1] 280984.3 150960.0
get_area(nc[1:2, ], units = "acres", .id = "acreage")$acreage
#> Units: [acres]
#> [1] 280984.3 150960.0

# Get distances for North Caroline counties
get_dist(nc[1, ], to = c("xmax", "ymax"), units = "mile")$dist
#> Units: [mile]
#>          [,1]
#> [1,] 18.03826
get_dist(nc[1, ], to = nc[30, ], units = "km")$dist
#> 239.2365 [km]

# Create a line between two counties
nc_line <- as_line(
  c(as_point(nc[1, ]), as_point(nc[30, ])),
  crs = sf::st_crs(nc)
)

# Get length and bearing of the line
get_length(nc_line)
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -81.49823 ymin: 36.02862 xmax: -78.87809 ymax: 36.4314
#> Geodetic CRS:  NAD27
#> # A tibble: 1 × 2
#>    length                                geometry
#>       [m]                        <LINESTRING [°]>
#> 1 239236. (-81.49823 36.4314, -78.87809 36.02862)

if (rlang::is_installed("geosphere")) {
  get_bearing(nc_line)
}
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -81.49823 ymin: 36.02862 xmax: -78.87809 ymax: 36.4314
#> Geodetic CRS:  NAD27
#> # A tibble: 1 × 2
#>   bearing                                geometry
#>     <dbl>                        <LINESTRING [°]>
#> 1   100.0 (-81.49823 36.4314, -78.87809 36.02862)
```
