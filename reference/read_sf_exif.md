# Read EXIF metadata to create a simple feature object or write EXIF metadata to image files

`read_sf_exif()` read EXIF data from folder of files and, geometry is
`TRUE` and coordinate metadata is available, convert the data to a sf
object. This function also assigns a cardinal direction based on the
direction metadata and recodes the orientation metadata.

For `write_exif()` the parameters are used to multiple tags with the
same values:

- title: Title, IPTC:Headline, IPTC:ObjectName, XMP-dc:Title

- description: ImageDescription, XMP-dc:Description, and
  IPTC:Caption-Abstract

- keywords: Keywords, IPTC:Keywords, XMP-dc:Subject

## Usage

``` r
read_sf_exif(
  path = NULL,
  fileext = NULL,
  filetype = NULL,
  bbox = NULL,
  sort = NULL,
  tags = NULL,
  geometry = TRUE,
  quiet = TRUE,
  ...
)
```

## Arguments

- path:

  A path to folder or file.

- fileext, filetype:

  File extension or file type. filetype is used if fileext is `NULL`.

- bbox:

  Bounding box to filter by.

- sort:

  Column name for variable to sort by passed to
  [`sort_features()`](https://elipousson.github.io/sfext/reference/number_features.md).
  Currently supports "lon", "lat", or "filename". Defaults to `NULL`.

- tags:

  Optional list of EXIF tags to read from files. Must include GPS tags
  to create an `sf` object.

- geometry:

  If `TRUE` (default), return a simple feature object. If `FALSE`,
  return a data.frame.

- quiet:

  If `TRUE` (default), suppress function messages.

- ...:

  Additional parameters to pass to
  [`exiftoolr::exif_read()`](https://joshobrien.github.io/exiftoolr/reference/exif_read.html)

## See also

Other read_write:
[`read_sf_ext()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  read_sf_exif(
    path = system.file("extdata/photos", package = "overedge"),
    filetype = "jpeg"
  )
} # }
```
