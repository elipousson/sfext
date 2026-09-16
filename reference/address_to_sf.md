# Use tidygeocoder to convert an address or data frame with an address column to an sf object

Wraps
[`tidygeocoder::geo()`](https://jessecambon.github.io/tidygeocoder/reference/geo.html)
and
[`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
to convert a character string or a data frame with an address column.
Additional parameters passed to
[`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
which passes `...` parameters to
[`tidygeocoder::geo()`](https://jessecambon.github.io/tidygeocoder/reference/geo.html).

## Usage

``` r
address_to_sf(
  x,
  address = "address",
  method = "osm",
  coords = c("lon", "lat"),
  remove_coords = FALSE,
  crs = NULL,
  full_results = FALSE,
  ...,
  call = caller_env()
)
```

## Arguments

- x:

  Data frame with an address column. Multiple address columns are not
  currently supported.

- address:

  Address column name, Default: 'address'

- method:

  Geocoding service to use, Default: 'osm'. Passed to
  [`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
  which passes it on to
  [`tidygeocoder::geo()`](https://jessecambon.github.io/tidygeocoder/reference/geo.html).
  See
  [`tidygeocoder::geo()`](https://jessecambon.github.io/tidygeocoder/reference/geo.html)
  for supported services (e.g. "osm", "census", "arcgis", "google").

- coords:

  Coordinate columns for input data.frame or output sf object (if
  geometry is 'centroid' or 'point') Default: c("lon", "lat").

- remove_coords:

  For
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.md),
  if `TRUE`, remove the coordinate columns after converting a data frame
  to simple feature object; defaults to `FALSE`.

- crs:

  Cordinate reference system to return, Default: 4326 for
  [`sf_to_df()`](https://elipousson.github.io/sfext/reference/sf_to_df.md)
  and `NULL` for
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.md).

- full_results:

  If `TRUE`, return all columns from the geocoding service in addition
  to the latitude and longitude columns, Default: `FALSE`.

- ...:

  Other parameters passed onto methods.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A `sf` object with POINT geometry for all geocoded addresses with valid
coordinates.

## See also

[`tidygeocoder::geo()`](https://jessecambon.github.io/tidygeocoder/reference/geo.html),
[`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
