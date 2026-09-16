# Get standard scales and convert to scale distances

This function returns a scale from
[standard_scales](https://elipousson.github.io/sfext/reference/standard_scales.md)
based on a provided name, standard, and/or series.

## Usage

``` r
get_scale(scale = NULL, standard = NULL, series = NULL)
```

## Arguments

- scale:

  Scale name from `standard_scales[["scale"]]`.

- standard:

  Scale standard. Options include "USGS", "Engineering", or
  "Architectural".

- series:

  Map series from `standard_scales[["series"]]`. Series is only
  available for USGS scales.

## Value

A tibble based on
[standard_scales](https://elipousson.github.io/sfext/reference/standard_scales.md)
with rows filtered to values that match parameters.
