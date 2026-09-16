# Write or cache a simple feature object to a file

The write_sf_ext and write_sf_cache helper functions wrap the
[`sf::write_sf()`](https://r-spatial.github.io/sf/reference/st_write.html)
function to provide some additional options including consistent file
naming with
[`make_filename()`](https://elipousson.github.io/filenamr/reference/make_filename.html)
and features including:

## Usage

``` r
write_sf_ext(
  data,
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  filename = NULL,
  fileext = NULL,
  filetype = NULL,
  description = NULL,
  path = NULL,
  cache = FALSE,
  pkg = "sfext",
  overwrite = FALSE,
  onefile = FALSE,
  ...
)

write_sf_list(
  data,
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  filename = NULL,
  fileext = NULL,
  filetype = NULL,
  path = NULL,
  overwrite = FALSE,
  onefile = FALSE,
  cache = FALSE,
  ...
)

write_sf_cache(
  data,
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  filename = NULL,
  fileext = NULL,
  filetype = NULL,
  data_dir = NULL,
  pkg = "sfext",
  overwrite = FALSE,
  create = TRUE,
  ...
)

write_sf_gist(
  data,
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  filename = NULL,
  fileext = "geojson",
  filetype = NULL,
  description = NULL,
  public = TRUE,
  browse = FALSE,
  token = Sys.getenv("GITHUB_PAT")
)

write_sf_gsheet(
  data,
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  filename = NULL,
  sheet = 1,
  ask = FALSE,
  key = NULL,
  ...
)
```

## Arguments

- data:

  A `sf` object, data frame, or other object to write.

- name:

  Name to make file name converted to snake case with
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html),
  e.g. "Residential zoning map" becomes "residential_zoning_map". If the
  name includes a file extension it is assumed that the filename has
  been provided as the name parameter.

- label:

  Label to combine with name converted to snake case with
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html).
  The label is designed to identify the area or other shared
  characteristics across multiple data files, maps, or plots. label is
  ignored if name is NULL or if name includes a file extension.

- prefix:

  File name prefix. "date" adds a date prefix, "time" adds a date/time
  prefix; defaults to `NULL`.

- postfix:

  File name postfix; defaults to `NULL`.

- filename, fileext, filetype:

  File name and/or file extension to write. filetype is superseded in
  favor of fileext. Both are optional if path includes filename and
  type, e.g. "~/Documents/data.geojson". fileext can be provided as part
  of the filename, e.g. "data.geojson". If a filename includes a file
  extensions and a separate fileext is also provided, the separate
  fileext parameter is used. Supported file extensions include "csv",
  "xlsx", "gsheet" (writes a Google Sheet), "rda", or any fileext
  supported by the available drivers (use
  [`sf::st_drivers()`](https://r-spatial.github.io/sf/reference/st_drivers.html)
  to list drivers).

- description:

  Optional description for the written file. For `write_sf_ext()`, used
  as the "DESCRIPTION" layer creation option when fileext is "gpkg". For
  `write_sf_gist()`, used as the gist description passed to
  [`gistr::gist_create()`](https://rdrr.io/pkg/gistr/man/gist_create.html).
  Defaults to `NULL`.

- path:

  Path to file or data directory. Optional. If path includes a file
  extension and filename and fileext are both `NULL`, the filename and
  extension included with path will be used instead. If multiple file
  extensions are provided to filename, path, or fileext,
  [`make_filename()`](https://elipousson.github.io/filenamr/reference/make_filename.html)
  will abort.

- cache:

  If `TRUE`, write `sf` object to file in cache directory; defaults to
  `FALSE`.

- pkg:

  The name of the package cache directory to use for write_sf_cache or
  write_sf_ext if `cache = TRUE`.

- overwrite:

  Logical. Default `FALSE`. If `TRUE`, overwrite any existing cached
  files that use the same file name.

- onefile:

  If `TRUE` and the fileext if "gpkg" (directly or from filename), save
  a sf list as a multilayer GeoPackage file where names for list items
  are used as layer names.

- ...:

  If data is an sf object and the fileext is "csv" or "xlsx", the ...
  parameters are passed to
  [`sf_to_df()`](https://elipousson.github.io/sfext/reference/sf_to_df.md)
  or to
  [`sf::write_sf()`](https://r-spatial.github.io/sf/reference/st_write.html)
  otherwise. If fileext is "rda" ... parameters are passed to
  [`readr::write_rds()`](https://readr.tidyverse.org/reference/read_rds.html).

- data_dir:

  cache data directory, defaults to
  [`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)
  when data_dir is `NULL`. (only used for `write_sf_cache()`; default is
  used when `cache = TRUE` for `write_sf_ext()`)

- create:

  If `FALSE` and path does not exist, return path with a warning. If
  `TRUE` and
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)
  is `TRUE`, ask user if directory should be created. If the session not
  interactive and create is `TRUE`, a new directory will be created.

- public:

  If `TRUE` (default), create a public gist. Passed to
  [`gistr::gist_create()`](https://rdrr.io/pkg/gistr/man/gist_create.html).

- browse:

  If `TRUE`, open the created gist in a browser after creation. Passed
  to
  [`gistr::gist_create()`](https://rdrr.io/pkg/gistr/man/gist_create.html).
  Defaults to `FALSE`.

- token:

  A personal access token on GitHub with permission to create gists;
  defaults to Sys.getenv("GITHUB_PAT")

- sheet:

  Sheet to write into, in the sense of "worksheet" or "tab". You can
  identify a sheet by name, with a string, or by position, with a
  number.

- ask:

  If `TRUE`, the user is prompted to make revisions to the created
  Google Sheet. When user responds to the prompt, the date is read back
  into the environment using
  [read_sf_gsheet](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  and joined to the provided data with the column name provided to key.
  Defaults to `FALSE`.

- key:

  If ask is `TRUE`, a key is required to join the sheet data to the
  provided data.

## Details

- If fileext is "csv", "xlsx", or "gsheet" the file is converted to a
  dataframe using
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.md)

- If the data is not an `sf` object and none of these filenames are
  provided, the user is prompted to save the file as an rda file with
  [`readr::write_rds()`](https://readr.tidyverse.org/reference/read_rds.html).

- If cache is `TRUE` use `write_sf_cache()` to cache file after writing
  a copy to the path provided.

- If data is a named sf list, pass the name of each sf object in the
  list to the name parameter and keep all other parameters consistent to
  write a file for each object in the list. No ... parameters are passed
  if data is an sf list.

## See also

[`sf::st_write()`](https://r-spatial.github.io/sf/reference/st_write.html)
