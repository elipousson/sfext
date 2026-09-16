# Get aspect ratio from string or based on specific paper and margins

Get aspect ratio from string or based on specific paper and margins

## Usage

``` r
get_asp(
  asp = NULL,
  paper = NULL,
  orientation = NULL,
  bbox = NULL,
  margin = NULL,
  block_asp = FALSE,
  allow_null = TRUE,
  ...
)
```

## Arguments

- asp:

  Aspect ratio of width to height as a numeric value (e.g. 0.33) or
  character (e.g. "1:3"). If numeric, `get_asp()` returns the same value
  without modification.

- paper:

  Paper, Default: 'letter'.

- orientation:

  Orientation "portrait", "landscape", or "square", Default: 'portrait'.

- bbox:

  A bounding box to use to get orientation using
  [`sf_bbox_asp()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  with orientation = TRUE.

- margin:

  A numeric vector or ggplot2 margin object.

- block_asp:

  If `TRUE`, and margin is not `NULL`, return the aspect ratio of the
  text or content block inside the page margins.

- allow_null:

  If `TRUE` and asp and paper are both `NULL`, return `NULL` without an
  error.

- ...:

  Arguments passed on to
  [`get_paper`](https://elipousson.github.io/sfext/reference/get_paper.md)

  `standard`

  :   Size standard, "ANSI", "ISO", "British Imperial", "JIS", "USPS",
      "Facebook", "Instagram", or "Twitter".

  `series`

  :   Size series (e.g. A), Default: `NULL`

  `size`

  :   Size number (only used for "ISO" and "JIS" series). Standard,
      series, and size may all be required to return a single paper when
      using these parameters.

  `width,height`

  :   Width and height in units, Default: `NULL`.

  `units`

  :   Paper size units, either "in", "mm", or "px"; defaults to `NULL`
      (using "in" if width or height are provided).

  `ncol,nrow`

  :   Number of expected columns and rows in paper; used to determine
      row_height and section_asp in paper data frame returned by
      get_paper if nrow or ncol is greater than 1; defaults to `NULL`.

  `gutter`

  :   Gutter distance in units. Gutter is used as the spacing between
      nrow and columns (variable spacing is not currently supported);
      defaults to 0.

## Value

A numeric aspect ratio.
