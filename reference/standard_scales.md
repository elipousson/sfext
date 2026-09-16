# Standard map, architectural, and engineering scales

Standard map scales derived from USGS 2002 report on map scales
<https://pubs.usgs.gov/fs/2002/0015/report.pdf>

## Usage

``` r
standard_scales
```

## Format

A data frame with 36 rows and 16 variables:

- `scale`:

  Scale name

- `standard`:

  Standard (USGS, architectural, or engineering)

- `series`:

  Series name (USGS map scales only)

- `actual_ft`:

  Scale distance for 1 ft actual.

- `actual_ft_unit`:

  Unit of scale for 1 ft actual.

- `scale_in`:

  Actual distance for 1 in scale.

- `scale_in_unit`:

  Unit of actual distance for 1 in scale.

- `scale_in_accuracy`:

  Accuracy of 1 in scale (approximate or exact)

- `scale_cm`:

  Actual distance for 1 cm scale.

- `scale_cm_unit`:

  Unit of actual distance for 1 cm scale.

- `scale_cm_accuracy`:

  Accuracy of 1 cm scale (approximate or exact)

- `size_latlon`:

  Standard size in latitude/longitude

- `size_latlon_unit`:

  Unit of latitude/longitude size (minutes or degrees)

- `area_approx`:

  Approximate actual area

- `area_approx_unit`:

  Approximate area unit

- `series_status`:

  Series status (select USGS map series are "abandoned")

## Details

Common architectural and engineering scales derived from FEMA guide to
using scales
<https://www.usfa.fema.gov/downloads/pdf/nfa/engineer-architect-scales.pdf>
