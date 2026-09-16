# Convert a data.frame with one or more coordinate columns to an sf object

- `coords_to_sf()`: Convert a data frame with coordinates into a simple
  feature object

- `check_coords()`: Check if a provided vector with coordinate column
  names are valid for the provided data frame

- `separate_coords()`: Separate coordinates from a single combined
  column into two columns

- `format_coords()`: Format coordinates as numeric values and remove
  missing coordinates from a data.frame

- `has_coords()`: Suggests a coordinate pair by comparing common values
  to the column names for a provided data.frame

- `rev_coords()`: Reverse a vector of coordinate names if the text "lat"
  or "y" appears in the first position

## Usage

``` r
coords_to_sf(
  x,
  coords = c("lon", "lat"),
  into = NULL,
  sep = ",",
  rev = FALSE,
  remove_coords = FALSE,
  crs = 4326,
  call = caller_env()
)

check_coords(
  x = NULL,
  coords = NULL,
  default = c("lon", "lat"),
  rev = FALSE,
  call = caller_env()
)

rev_coords(coords, pattern = c("lat", "^y"), ignore.case = TRUE)

has_coords(x, coords = NULL, value = TRUE)

format_coords(
  x,
  coords = c("lon", "lat"),
  keep_missing = FALSE,
  call = caller_env()
)

separate_coords(x, coords, into = c("lon", "lat"), sep = ",")
```

## Arguments

- x:

  A data.frame with one or more coordinate columns. If coordinates are
  contained within a single column, coord must be length 1 and a length
  2 into parameter must be provided.

- coords:

  Coordinate columns for input data.frame or output sf object (if
  geometry is 'centroid' or 'point') Default: c("lon", "lat").

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
  `check_coords()` only.

- remove_coords:

  For
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.md),
  if `TRUE`, remove the coordinate columns after converting a data frame
  to simple feature object; defaults to `FALSE`.

- crs:

  Coordinate reference system used by the coordinates in the provided
  data frame.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- default:

  Default coordinate values; defaults to `c("lon", "lat")`.

- pattern:

  Pattern passed by `rev_coords()` to
  [`grepl()`](https://rdrr.io/r/base/grep.html) used to match vectors
  that are reversed. Defaults to `c("lat", "^y")`.

- ignore.case:

  If `TRUE`, pattern matching is not case sensitive.

- value:

  If `TRUE`, return the value of the coordinate column names. Used by
  `has_coords()`.

- keep_missing:

  If `TRUE`, keep rows with missing coordinate values. Defaults to
  `FALSE` which filters out rows with missing coordinates.

## See also

[`is_geo_coords()`](https://elipousson.github.io/sfext/reference/is_sf.md)
