# Convert distance from scale to actual units

This function converts scale distances to actual units based on named
[standard_scales](https://elipousson.github.io/sfext/reference/standard_scales.md).

## Usage

``` r
convert_dist_scale(
  dist = NULL,
  scale = NULL,
  scale_standard = NULL,
  scale_series = NULL,
  scale_unit = "in",
  scale_factor = NULL,
  actual_unit = NULL,
  dpi = 120,
  paper = NULL,
  orientation = NULL,
  ...
)
```

## Arguments

- dist:

  distance to convert. If paper is provided, dist is optional and paper
  width and height are used as dist.

- scale:

  Scale name from `standard_scales[["scale"]]`.

- scale_standard, scale_series:

  Passed to standard and scale parameters of
  [`get_scale()`](https://elipousson.github.io/sfext/reference/get_scale.md).

- scale_unit:

  "mm" (converted to cm by dividing by 10), "cm", "px" (converted to
  inches by dividing by dpi), or "in".

- scale_factor:

  factor for converting from scale_unit to actual_unit, e.g. if 1" = 1',
  the scale factor is 12. optional if scale if provided; defaults to
  `NULL`.

- actual_unit:

  any unit supported by
  [`convert_dist_units()`](https://elipousson.github.io/sfext/reference/convert_dist_units.md)

- dpi:

  dots per square inch (used as conversion factor for "px" to "in")

- paper:

  Paper, Default: 'letter'.

- orientation:

  Orientation "portrait", "landscape", or "square", Default: 'portrait'.

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

  `margin`

  :   A numeric vector or ggplot2 margin object.

  `bbox`

  :   A bounding box to use to get orientation using
      [`sf_bbox_asp()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
      with orientation = TRUE.

## Value

- If paper is not provided, return a vector of dist values converted
  from scale_unit to actual_unit based on scale_factor or information
  from
  [standard_scales](https://elipousson.github.io/sfext/reference/standard_scales.md)
  data.

- If paper is provided, return a data.frame with converted distances
  appends as columns named actual_width and actual_height.

## See also

Other dist:
[`compare_dist()`](https://elipousson.github.io/sfext/reference/compare_dist.md),
[`convert_dist_units()`](https://elipousson.github.io/sfext/reference/convert_dist_units.md),
[`get_measurements`](https://elipousson.github.io/sfext/reference/get_measurements.md),
[`is_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md),
[`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
