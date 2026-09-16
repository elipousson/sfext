# Clip the side or corner of a simple feature or bounding box object

Clip based on the corner of the object bounding box.

## Usage

``` r
st_clip(
  x,
  clip = NULL,
  keep = NULL,
  flip = FALSE,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter"
)
```

## Arguments

- x:

  `sf` or `bbox` object to clip

- clip:

  Character string describing the part of the area to clip or remove.
  Options include c("top", "right", "bottom", "left", "topright",
  "bottomright", "bottomleft", "topleft"). If NULL, the area is not
  clipped and a full edge can be returned.

- keep:

  Alternate way of defining clip (by naming the section to keep).

- flip:

  Logical. Default FALSE. If TRUE, than the clip area is kept instead of
  removed. If keep is provided, flip is automatically set to TRUE.

- dist:

  Numeric. Distance to use for the edge. Default NULL meters. Use
  negative values for an inside edge or positive numbers for an outside
  edge.

- diag_ratio:

  Alternate way to define edge distance.

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

## Value

`sf` object clipped based on parameters
