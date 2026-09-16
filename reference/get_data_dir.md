# Check if data directory exists and create a new directory if needed

**\[superseded\]** Get the path for a package-specific cache directory
with
[`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html),
check for the existence of a data directory, optionally create a new
directory at the provided path location.

## Usage

``` r
get_data_dir(
  path = NULL,
  cache = FALSE,
  create = TRUE,
  pkg = "sfext",
  allow_null = TRUE,
  recursive = TRUE
)

list_data_files(
  path = NULL,
  pkg = "sfext",
  cache = FALSE,
  fileext = NULL,
  pattern = NULL,
  full.names = TRUE,
  ignore.case = TRUE,
  ...
)
```

## Arguments

- path:

  Path to directory for use as data directory.

- cache:

  If `TRUE`, and path is `NULL` set path to
  [rappdirs::user_cache_dir](https://rappdirs.r-lib.org/reference/user_cache_dir.html)
  (using value of pkg as appname). If path is not `NULL`, the path is
  returned even if cache is `TRUE`.

- create:

  If `FALSE` and path does not exist, return path with a warning. If
  `TRUE` and
  [rlang::is_interactive](https://rlang.r-lib.org/reference/is_interactive.html)
  is `TRUE`, ask user if directory should be created. If the session not
  interactive and create is `TRUE`, a new directory will be created.

- pkg:

  Package name; defaults to "sfext"

- allow_null:

  If `TRUE`, path is `NULL`, cache is `FALSE`, return the `NULL` path
  value; defaults to `TRUE`.

- recursive:

  logical. Should elements of the path other than the last be created?
  If true, like the Unix command `mkdir -p`.

- fileext:

  If pattern is NULL, fileext is used to set the pattern and filter
  listed files to those matching the file extension.

- pattern:

  an optional [regular expression](https://rdrr.io/r/base/regex.html).
  Only file names which match the regular expression will be returned.

- full.names:

  a logical value. If `TRUE`, the directory path is prepended to the
  file names to give a relative file path. If `FALSE`, the file names
  (rather than paths) are returned.

- ignore.case:

  logical. Should pattern-matching be case-insensitive?

- ...:

  Additional parameters passed to
  [`list.files()`](https://rdrr.io/r/base/list.files.html)
