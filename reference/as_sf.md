# Convert an object to a simple feature or bounding box object

Both functions will pass a `NULL` value without returning an error. If a
POINT or MULTIPOINT object is passed to `as_bbox()` a 0.00000001 meter
buffer is applied. If a character object is passed to `as_bbox()` it is
passed to
[`osmdata::getbb()`](https://docs.ropensci.org/osmdata/reference/getbb.html)
using `format_out = "matrix"` which is converted into a bounding box.

## Usage

``` r
as_sf(
  x,
  crs = NULL,
  sf_col = "geometry",
  ext = TRUE,
  ...,
  as_tibble = TRUE,
  call = caller_env()
)

as_bbox(x, crs = NULL, ext = TRUE, ..., call = caller_env())

as_sfc(x, crs = NULL, ext = TRUE, ..., call = caller_env())

as_sf_class(x, class = NULL, allow_null = TRUE, ..., call = caller_env())
```

## Arguments

- x:

  Object to convert to an sf, sfc, bbox or a sf list object.

- crs:

  Coordinate reference system for `sf`, `bbox`, `sfc` or `sf` list
  object to return.

- sf_col:

  A column name to use for the geometry column created by as_sf;
  defaults to "geometry".

- ext:

  If `TRUE`, as_sf will convert a data frame or character vector of
  addresses to an sf object using
  [df_to_sf](https://elipousson.github.io/sfext/reference/sf_to_df.md)
  or
  [address_to_sf](https://elipousson.github.io/sfext/reference/address_to_sf.md).
  If `FALSE`, only spatial objects (bbox, sfg, sfc, sf list, raster, or
  sp objects) can be converted. Defaults to `TRUE`.

- ...:

  Additional parameters passed to as_sf, as_sfc, as_bbox, or
  [as_sf_list](https://elipousson.github.io/sfext/reference/sf_list.md)

- as_tibble:

  If `TRUE`, always return sf object as a tibble. If `FALSE`, some
  conversions may still return a tibble object.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- class:

  A class to convert data to; defaults to NULL (which returns "sf")

- allow_null:

  For as_sf_class, if class is `NULL` and allow_null is `TRUE`, return x
  without any class conversion or checks. Defaults to `TRUE`.

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
nc[["category"]] <- sample(c("A", "B", "C"), nrow(nc), replace = TRUE)

as_sf(nc$geometry)
#> Simple feature collection with 100 features and 0 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> # A tibble: 100 × 1
#>                                                                         geometry
#>                                                               <MULTIPOLYGON [°]>
#>  1 (((-81.47276 36.23436, -81.54084 36.27251, -81.56198 36.27359, -81.63306 36.…
#>  2 (((-81.23989 36.36536, -81.24069 36.37942, -81.26284 36.40504, -81.26624 36.…
#>  3 (((-80.45634 36.24256, -80.47639 36.25473, -80.53688 36.25674, -80.54501 36.…
#>  4 (((-76.00897 36.3196, -76.01735 36.33773, -76.03288 36.33598, -76.04395 36.3…
#>  5 (((-77.21767 36.24098, -77.23461 36.2146, -77.29861 36.21153, -77.29351 36.1…
#>  6 (((-76.74506 36.23392, -76.98069 36.23024, -76.99475 36.23558, -77.13007 36.…
#>  7 (((-76.00897 36.3196, -75.95718 36.19377, -75.98134 36.16973, -76.18317 36.3…
#>  8 (((-76.56251 36.34057, -76.60424 36.31498, -76.64822 36.31532, -76.68874 36.…
#>  9 (((-78.30876 36.26004, -78.28293 36.29188, -78.32125 36.54553, -78.05167 36.…
#> 10 (((-80.02567 36.25023, -80.45301 36.25709, -80.43531 36.55104, -80.0481 36.5…
#> # ℹ 90 more rows

nc_bbox <- as_bbox(nc)

nc_bbox
#>      xmin      ymin      xmax      ymax 
#> -84.32385  33.88199 -75.45698  36.58965 

as_sfc(nc_bbox)
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> POLYGON ((-84.32385 33.88199, -75.45698 33.8819...

as_xy(nc[1, ])
#>           x       y
#> 1 -81.49823 36.4314

as_sf_list(nc, col = "category")
#> <sf_list[3]>
#> $a
#> Simple feature collection with 37 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.03086 ymin: 33.88199 xmax: -75.7637 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> # A tibble: 37 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>    <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  4 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  5 0.109      1.32  1841    1841 Pers… 37145  37145       73  1556     4     613
#>  6 0.053      1.17  1848    1848 Pasq… 37139  37139       70  1638     3     622
#>  7 0.199      1.98  1874    1874 Wilk… 37193  37193       97  3146     4     200
#>  8 0.086      1.27  1893    1893 Yadk… 37197  37197       99  1269     1      65
#>  9 0.128      1.55  1897    1897 Fran… 37069  37069       35  1399     2     736
#> 10 0.18       2.15  1905    1905 Bert… 37015  37015        8  1324     6     921
#> # ℹ 27 more rows
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <MULTIPOLYGON [°]>
#> 
#> $b
#> Simple feature collection with 27 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -83.98855 ymin: 34.30505 xmax: -75.45698 ymax: 36.56521
#> Geodetic CRS:  NAD27
#> # A tibble: 27 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>    <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  2 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#>  3 0.19       2.20  1846    1846 Hali… 37083  37083       42  3608    18    2365
#>  4 0.081      1.29  1880    1880 Wata… 37189  37189       95  1323     1      17
#>  5 0.044      1.16  1887    1887 Chow… 37041  37041       21   751     1     368
#>  6 0.064      1.21  1892    1892 Avery 37011  37011        6   781     0       4
#>  7 0.17       1.68  1903    1903 Guil… 37081  37081       41 16184    23    5483
#>  8 0.122      1.52  1932    1932 Cald… 37027  37027       14  3609     6     309
#>  9 0.118      1.60  1946    1946 Madi… 37115  37115       58   765     2       5
#> 10 0.155      1.78  1947    1947 Ired… 37097  37097       49  4139     4    1144
#> # ℹ 17 more rows
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <MULTIPOLYGON [°]>
#> 
#> $c
#> Simple feature collection with 36 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.94867 xmax: -75.77316 ymax: 36.55716
#> Geodetic CRS:  NAD27
#> # A tibble: 36 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>    <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  2 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  3 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  4 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#>  5 0.114      1.35  1838    1838 Casw… 37033  37033       17  1035     2     550
#>  6 0.153      1.62  1839    1839 Rock… 37157  37157       79  4449    16    1243
#>  7 0.143      1.66  1840    1840 Gran… 37077  37077       39  1671     4     930
#>  8 0.072      1.08  1842    1842 Vance 37181  37181       91  2180     4    1179
#>  9 0.063      1     1881    1881 Perq… 37143  37143       72   484     1     230
#> 10 0.108      1.48  1900    1900 Fors… 37067  37067       34 11858    10    3919
#> # ℹ 26 more rows
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <MULTIPOLYGON [°]>
#> 
```
