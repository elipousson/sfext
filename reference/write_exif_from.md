# Write EXIF data for photos on spatial join with a sf object or list of sf objects

Extends
[`read_sf_exif()`](https://elipousson.github.io/sfext/reference/read_sf_exif.md)
and
[`filenamr::write_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.html)

## Usage

``` r
write_exif_from(
  path,
  fileext = NULL,
  filetype = NULL,
  from,
  .id = "name",
  tag = "keywords",
  join = NULL,
  overwrite = TRUE
)
```

## Arguments

- path:

  A path to folder or file.

- fileext, filetype:

  File extension or file type. filetype is used if fileext is `NULL`.

- from:

  A sf object or list of sf objects where each object has a column with
  a name matching the .id parameter. The attribute value in this column
  are used to assign the tag parameter to the file at the provided path
  based on the spatial relationship set by join. For example, from may
  be boundary data used to assign keywords based on photo locations.

- .id:

  Column name in from with the values to use for tag values.

- tag:

  EXIF tag to update, supported options include "keywords", "title", or
  "description".

- join:

  geometry predicate function; defaults to `NULL`, set to
  [sf::st_intersects](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
  if from contains only POLYGON or MULTIPOLYGON objects or
  [sf::st_nearest_feature](https://r-spatial.github.io/sf/reference/st_nearest_feature.html)
  if from contains other types.

- overwrite:

  If `TRUE`, overwrite any existing EXIF metadata present in the
  provided fields; defaults to `TRUE`
