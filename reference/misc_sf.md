# Additional utility functions for sf objects

- `transform_sf()` is similar to
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)
  but supports sf, sfc, or bbox objects as the crs parameter, supports
  sfg objects (transformed to sfc), and uses
  [`sf::st_set_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  if the CRS for the provided object is `NA`. This function does not
  support bounding box transformations.

- `relocate_sf_col()` relocates the sf geometry column after specified
  column (by default after everything).

- `rename_sf_col()` a wrapper for
  [`sf::st_set_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.html)
  that renames the sf column.

- `get_sf_col()` returns the "sf_column" attribute.

- `get_sf_colnames()` returns the column names of a file that can be
  read with
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html)
  to allow you to use column names to build a query (or provide a value
  for name_col) without reading the whole file.

## Usage

``` r
transform_sf(x, crs = NULL, allow_null = TRUE, ...)

relocate_sf_col(x, .after = dplyr::everything())

rename_sf_col(x, sf_col = "geometry")

get_sf_col(x = NULL)

get_sf_colnames(x = NULL, dsn = NULL, layer = NULL, ...)
```

## Arguments

- x:

  A `sf` or `sfc` object. If x has a missing crs, the crs is set to the
  provided value.

- crs:

  A coordinate reference system identifier (numeric or character) or a
  `sf`, `sfc`, `bbox`, or `crs` class object supported by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).

- allow_null:

  If `TRUE` and crs is `NULL`, return x.

- ...:

  Additional parameters passed to
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)
  by `transform_sf()` or to
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html)
  by `get_sf_colnames()` if x is not `NULL`.

- .after:

  The location to place sf column after; defaults to
  [`dplyr::everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- sf_col:

  Name to use for the sf column after renaming; defaults to "geometry".

- dsn:

  data source name (interpretation varies by driver - for some drivers,
  `dsn` is a file name, but may also be a folder, or contain the name
  and access credentials of a database); in case of GeoJSON, `dsn` may
  be the character string holding the geojson data. It can also be an
  open database connection.

- layer:

  layer name (varies by driver, may be a file name without extension);
  in case `layer` is missing, `st_read` will read the first layer of
  `dsn`, give a warning and (unless `quiet = TRUE`) print a message when
  there are multiple layers, or give an error if there are no layers in
  `dsn`. If `dsn` is a database connection, then `layer` can be a table
  name or a database identifier (see
  [`Id`](https://dbi.r-dbi.org/reference/Id.html)). It is also possible
  to omit `layer` and rather use the `query` argument.
