# Convert between simple feature and data frame objects

`sf_to_df()` converts a simple feature object to a data.frame by
dropping geometry or, using
[`get_coords()`](https://elipousson.github.io/sfext/reference/get_coords.md),
converting geometry to well known text, or (if the geometry type is not
POINT) getting coordinates for a centroid or point on surface. If an sfc
object is provided,the "drop" geometry option is not supported.

`df_to_sf()` converts a data.frame to a simple feature object using a
geometry column (named "geometry"), address column (name matching the
address parameter), well known text column (named "wkt"), or coords
(must have columns matching coords values). Related helper functions
include
[`check_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
to suggest the appropriate coordinate column names based on the column
names in the provided data frame;
[`has_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
to check if a data frame has the specified coordinates.

## Usage

``` r
sf_to_df(
  x,
  crs = 4326,
  coords = c("lon", "lat"),
  geometry = "centroid",
  keep_all = TRUE
)

df_to_sf(
  x,
  crs = NULL,
  coords = c("lon", "lat"),
  from_crs = 4326,
  into = NULL,
  sep = ",",
  rev = TRUE,
  remove_coords = FALSE,
  geo = FALSE,
  address = "address",
  y = NULL,
  by = NULL,
  ...,
  as_tibble = TRUE,
  .name_repair = "unique",
  call = caller_env()
)
```

## Arguments

- x:

  A `sf` or `sfc` object or a data frame with lat/lon coordinates in a
  single column or two separated columns.

- crs:

  Cordinate reference system to return, Default: 4326 for `sf_to_df()`
  and `NULL` for `df_to_sf()`.

- coords:

  Coordinate columns for input data.frame or output sf object (if
  geometry is 'centroid' or 'point') Default: c("lon", "lat").

- geometry:

  Type of geometry to include in data frame. options include "drop",
  "wkt", "centroid", "point", Default: 'centroid'.

- keep_all:

  If `FALSE`, drop all columns other than those named in coords,
  Default: `TRUE`.

- from_crs:

  For `df_to_sf()`, coordinate reference system used by coordinates or
  well known text in data frame.

- into:

  If coords is a single column name with both longitude and latitude,
  `into` is used as the names of the new columns that coords is
  separated into. Passed to
  [`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html).

- sep:

  If coords is a single column name with both longitude and latitude,
  `sep` is used as the separator between coordinate values. Passed to
  [`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html).

- rev:

  If `TRUE`, reverse `c("lat", "lon")` coords to `c("lon", "lat")`.
  [`check_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
  only.

- remove_coords:

  For `df_to_sf()`, if `TRUE`, remove the coordinate columns after
  converting a data frame to simple feature object; defaults to `FALSE`.

- geo:

  If `TRUE`, use
  [`address_to_sf()`](https://elipousson.github.io/sfext/reference/address_to_sf.md)
  to geocode address column; defaults to `FALSE`.

- address:

  Address column name passed to
  [`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
  or
  [tidygeocoder::geo](https://jessecambon.github.io/tidygeocoder/reference/geo.html)

- y:

  A sf object passed as y argument to
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html).

- by:

  A character vector of variables to join by passed to
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html).

- ...:

  Other parameters passed onto methods.

- as_tibble:

  If `TRUE`, use
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  to make sure a sf tibble is returned.

- .name_repair:

  Passed to
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

`sf_to_df()` returns a data frame with geometry dropped or converted to
wkt or coordinates for the centroid or point on surface; `df_to_sf()`
returns a simple feature object with POINT geometry.

## See also

[`sf::st_coordinates()`](https://r-spatial.github.io/sf/reference/st_coordinates.html)

[df_spatial()](https://paleolimbot.github.io/ggspatial/reference/df_spatial.html)
in the ggspatial package
[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)

## Examples

``` r
nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

# Convert a sf object to a data frame
nc_df <- sf_to_df(nc)

# Convert a data frame to a sf object
df_to_sf(nc_df, coords = c("lon", "lat"), remove_coords = TRUE)
#> Simple feature collection with 100 features and 14 fields
#> Attribute-geometry relationships: constant (14)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -84.05976 ymin: 34.07684 xmax: -75.80916 ymax: 36.49119
#> Geodetic CRS:  WGS 84
#> # A tibble: 100 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>  * <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # ℹ 90 more rows
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <POINT [°]>

# If lon and lat values are present in a single column, use the into parameter
# to split the values back into separate columns
nc_df$xy <- paste(nc_df$lon, nc_df$lat, sep = ",")

df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
#> Simple feature collection with 100 features and 16 fields
#> Attribute-geometry relationships: constant (16)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -84.05976 ymin: 34.07684 xmax: -75.80916 ymax: 36.49119
#> Geodetic CRS:  WGS 84
#> # A tibble: 100 × 17
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>  * <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # ℹ 90 more rows
#> # ℹ 6 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>, lon <dbl>,
#> #   lat <dbl>, geometry <POINT [°]>
```
