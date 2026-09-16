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

  Arguments passed on to
  [`tidygeocoder::geocode`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)

  `.tbl`

  :   dataframe containing addresses

  `street`

  :   street address column name

  `city`

  :   city column name

  `county`

  :   county column name

  `state`

  :   state column name

  `postalcode`

  :   postal code column name (zip code if in the United States)

  `country`

  :   country column name

  `lat`

  :   latitude column name. Can be quoted or unquoted (ie. `lat` or
      `"lat"`).

  `long`

  :   longitude column name. Can be quoted or unquoted (ie. `long` or
      `"long"`).

  `return_input`

  :   if TRUE then the input dataset will be combined with the geocoder
      query results and returned. If FALSE only the geocoder results
      will be returned.

  `limit`

  :   maximum number of results to return per input address. For many
      geocoding services the maximum value of the limit parameter
      is 100. Pass `limit = NULL` to use the default `limit` value of
      the selected geocoding service. For batch geocoding, limit must be
      set to 1 (default) if `return_addresses = TRUE`.To use `limit > 1`
      or `limit = NULL` set return_input to FALSE. Refer to
      [api_parameter_reference](https://jessecambon.github.io/tidygeocoder/reference/api_parameter_reference.html)
      for more details.

  `return_addresses`

  :   if TRUE return input addresses. Defaults to TRUE if `return_input`
      is FALSE and FALSE if `return_input` is TRUE. This argument is
      passed to the `geo()` function.

  `unique_only`

  :   if TRUE then only unique results will be returned and return_input
      will be set to FALSE.

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
