# Read spatial data in a bounding box to a simple feature object from multiple sources

An extended version of
[`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html)
that support reading spatial data based on a file path, URL, or the data
name and associated package. A RDS, RDA, or RData file Optionally
provide a bounding box to filter data (data is filtered before download
or reading into memory where possible).

## Usage

``` r
read_sf_ext(...)

read_sf_pkg(
  data,
  bbox = NULL,
  package = NULL,
  pkg = NULL,
  fileext = "gpkg",
  filetype = NULL,
  ...
)

read_sf_path(path, bbox = NULL, ...)

read_sf_zip(
  path,
  bbox = NULL,
  exdir = NULL,
  overwrite = TRUE,
  unzip = "internal",
  ...
)

read_sf_rdata(
  path,
  file = NULL,
  refhook = NULL,
  bbox = NULL,
  .name_repair = "check_unique",
  ...
)

read_sf_query(
  path,
  dsn = NULL,
  bbox = NULL,
  query = NULL,
  table = NULL,
  name = NULL,
  name_col = NULL,
  wkt_filter = NULL,
  zm_drop = FALSE,
  .name_repair = "check_unique",
  ...
)

read_sf_excel(
  path,
  sheet = NULL,
  combine_sheets = FALSE,
  bbox = NULL,
  coords = c("lon", "lat"),
  from_crs = 4326,
  geo = FALSE,
  address = "address",
  .name_repair = "check_unique",
  ...
)

read_sf_csv(
  path,
  url = NULL,
  bbox = NULL,
  coords = c("lon", "lat"),
  from_crs = 4326,
  geo = FALSE,
  address = "address",
  wkt = NULL,
  .name_repair = "check_unique",
  show_col_types = FALSE,
  ...
)

read_sf_url(url, bbox = NULL, coords = c("lon", "lat"), ...)

read_sf_esri(
  url,
  bbox = NULL,
  where = NULL,
  name = NULL,
  name_col = NULL,
  coords = c("lon", "lat"),
  from_crs = 4326,
  .name_repair = "check_unique",
  ...
)

read_sf_gist(url, id = NULL, bbox = NULL, nth = 1, ...)

read_sf_gmap(
  url,
  bbox = NULL,
  layer = NULL,
  combine_layers = FALSE,
  zm_drop = TRUE,
  .name_repair = "check_unique"
)

read_sf_download(
  url,
  filename,
  bbox = NULL,
  path = NULL,
  filetype = "geojson",
  prefix = "date",
  method = "auto",
  unzip = FALSE,
  .name_repair = "check_unique",
  ...
)

read_sf_gsheet(
  url,
  sheet = NULL,
  ss = NULL,
  bbox = NULL,
  ask = FALSE,
  coords = c("lon", "lat"),
  from_crs = 4326,
  geo = FALSE,
  address = "address",
  .name_repair = "check_unique",
  ...
)
```

## Arguments

- ...:

  Additional parameters passed to multiple functions; see details.

- data:

  Name of a package dataset; used by `read_sf_pkg()` only.

- bbox:

  A bounding box object; defaults to `NULL`. If `"bbox"` is provided,
  only returns features intersecting the bounding box.

- package, pkg:

  Package name; used by `read_sf_pkg()` only. pkg is used if package is
  `NULL`.

- fileext:

  File extension. If supplied to
  [`list_path_filenames()`](https://elipousson.github.io/filenamr/reference/get_data_dir.html)
  and pattern is `NULL`, only return file names matching this extension.

- filetype:

  File type supported by
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html);
  Default: 'gpkg'; used by `read_sf_pkg()` only and required only if the
  data is in the package cache directory or extdata system files.

- path:

  A file path.

- exdir:

  The directory to extract files to (the equivalent of `unzip -d`). It
  will be created if necessary.

- overwrite:

  If `TRUE`, overwrite existing files (the equivalent of `unzip -o`),
  otherwise ignore such files (the equivalent of `unzip -n`).

- unzip:

  If `TRUE`, url must be a zip file that is downloaded to a cache
  folder, unzipped into a temporary directory (created with
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)), and then read to
  a file using the specified file type.

- file:

  The file path to read from/write to.

- refhook:

  A function to handle reference objects.

- .name_repair:

  Passed to repair parameter of
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)

- dsn:

  data source name (interpretation varies by driver - for some drivers,
  `dsn` is a file name, but may also be a folder, or contain the name
  and access credentials of a database); in case of GeoJSON, `dsn` may
  be the character string holding the geojson data. It can also be an
  open database connection.

- query:

  SQL query to select records; see details

- table:

  table can usually be inferred from basename of the data source. table
  is used to generate a custom query if both name and name_col are
  provided. Use `sf::st_layers(dsn = dsn)[["name"]]` to see a list of
  available table names.

- name, name_col:

  Name value and name column to use in generated a query for sources
  read with `read_sf_query()` or `read_sf_esri()`.

- wkt_filter:

  character; WKT representation of a spatial filter (may be used as
  bounding box, selecting overlapping geometries); see examples

- zm_drop:

  If `TRUE`, drop Z and/or M dimensions using
  [sf::st_zm](https://r-spatial.github.io/sf/reference/st_zm.html)

- sheet:

  Sheet to read. Either a string (the name of a sheet), or an integer
  (the position of the sheet). Ignored if the sheet is specified via
  `range`. If neither argument specifies the sheet, defaults to the
  first sheet.

- coords:

  Character vector with coordinate values. Coordinates must use the same
  crs as the `from_crs` parameter.

- from_crs:

  For
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.md),
  coordinate reference system used by coordinates or well known text in
  data frame.

- geo:

  If `TRUE`, use
  [`address_to_sf()`](https://elipousson.github.io/sfext/reference/address_to_sf.md)
  to geocode address column; defaults to `FALSE`.

- address:

  Address column name, Default: 'address'

- url:

  A url for a spatial data file, tabular data with coordinates, or a
  ArcGIS FeatureServer or MapServer to access with
  [`esri2sf::esri2sf()`](https://rdrr.io/pkg/esri2sf/man/esri2sf.html)

- wkt:

  Name of column with well-known text for geometry. Used by
  `read_sf_csv()`.

- show_col_types:

  If `FALSE`, do not show the guessed column types. If `TRUE` always
  show the column types, even if they are supplied. If `NULL` (the
  default) only show the column types if they are not explicitly
  supplied by the `col_types` argument.

- id:

  The name of a column in which to store the file path. This is useful
  when reading multiple input files and there is data in the file paths,
  such as the data collection date. If `NULL` (the default) no extra
  column is created.

- nth:

  For `read_sf_gist()`, the file to return from the gist, e.g. 1 for
  first, 2 for second. Defaults to 1.

- layer:

  layer name (varies by driver, may be a file name without extension);
  in case `layer` is missing, `st_read` will read the first layer of
  `dsn`, give a warning and (unless `quiet = TRUE`) print a message when
  there are multiple layers, or give an error if there are no layers in
  `dsn`. If `dsn` is a database connection, then `layer` can be a table
  name or a database identifier (see
  [`Id`](https://dbi.r-dbi.org/reference/Id.html)). It is also possible
  to omit `layer` and rather use the `query` argument.

- combine_layers, combine_sheets:

  If `FALSE` (default), return a list with a sf object for each layer or
  sheet as a separate item. If `TRUE`, use
  [`purrr::map_dfr()`](https://purrr.tidyverse.org/reference/map_dfr.html)
  to combine layers or sheets into a single sf object using the layer or
  sheet name as an additional column.

- filename:

  File name; if filename is `NULL` and path does not include a file
  extension, name and file extension are both required.

- prefix:

  File name prefix. "date" adds a date prefix, "time" adds a date/time
  prefix; defaults to `NULL`.

- method:

  Method to be used for downloading files. Current download methods are
  `"internal"`, `"libcurl"`, `"wget"`, `"curl"` and `"wininet"` (Windows
  only), and there is a value `"auto"`: see ‘Details’ and ‘Note’.

  The method can also be set through the option
  `"download.file.method"`: see
  [`options()`](https://rdrr.io/r/base/options.html).

- ss:

  Something that identifies a Google Sheet:

  - its file id as a string or
    [`drive_id`](https://googledrive.tidyverse.org/reference/drive_id.html)

  - a URL from which we can recover the id

  - a one-row
    [`dribble`](https://googledrive.tidyverse.org/reference/dribble.html),
    which is how googledrive represents Drive files

  - an instance of `googlesheets4_spreadsheet`, which is what
    [`gs4_get()`](https://googlesheets4.tidyverse.org/reference/gs4_get.html)
    returns

  Processed through
  [`as_sheets_id()`](https://googlesheets4.tidyverse.org/reference/sheets_id.html).

- ask:

  If `TRUE`, ask for the name of the Google Sheet to read if ss is not
  provided to read_sf_gsheet.

## Details

Reading data from a url:

`read_sf_url()` supports multiple types of urls:

- A MapServer or FeatureServer URL

- A URL for a GitHub gist with a single spatial data file (first file
  used if gist contains multiple)

- A URL for a spatial data file, CSV file, Excel file, or RDS file (RDA
  and RData files supported by `read_sf_path()`)

- A Google Sheets URL

- A public Google Maps URL

## Reading data from a package

`read_sf_pkg()` looks for three types of package data:

- Data loaded with the package

- External data in the `extdata` system files folder.

- Cached data in the cache directory returned by
  [`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)

## Additional ... parameters

`read_sf_ext()` is a flexible function where ... are passed to one of
the other read functions depending on the provided parameters. If you
are using more than one parameter, all parameters *must* be named.

`read_sf_pkg()` and `read_sf_download()` both pass additional parameters
to `read_sf_path()` which supports query, name_col, name, and table.
name and name_col are ignored if a query parameter is provided. If table
is not provided, a expected layer name is created based on the file
path.

`read_sf_url()` pass the where, name_col, and name for any ArcGIS
FeatureServer or MapServer url (passed to `read_sf_esri()`) or sheet if
the url is for a Google Sheet (passed to
[`googlesheets4::read_sheet()`](https://googlesheets4.tidyverse.org/reference/range_read.html)),
or a query or wkt filter parameter if the url is some other type (passed
to
[`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html)).

## See also

Other read_write:
[`read_sf_exif()`](https://elipousson.github.io/sfext/reference/read_sf_exif.md)
