# Get margins for a ggplot2 plot or map based on style or distance

This function works in combination with the
[`get_paper()`](https://elipousson.github.io/sfext/reference/get_paper.md)
function to make it easier to position a map on a page before saving to
file. This is primarily useful when using a map or plot created with
ggplot2 as part of a print document format that is composed outside of R
using a page layout application such as Adobe InDesign.

## Usage

``` r
get_margin(
  margin = NULL,
  paper = NULL,
  orientation = NULL,
  dist = NULL,
  unit = "in",
  block_width = NULL,
  header = 0,
  footer = 0
)
```

## Arguments

- margin:

  Margin style (options include "extrawide", "wide", "standard",
  "narrow", "none"), Additional "auto" option to generate margin based
  on line length is planned but not yet implemented. Default: `NULL`
  (equivalent to "none").

- paper:

  Paper, Default: 'letter'.

- orientation:

  Orientation "portrait", "landscape", or "square", Default: 'portrait'.

- dist:

  Margin distance (single value used to all sides), Default: `NULL`

- unit:

  Unit for margin distance, Default: 'in'.

- block_width:

  Plot or map width in units. If `paper` and `block_width` are provided,
  margins are half the distance between the two evenly distributed. This
  sets the margin distance for height as well as width so does not work
  well with header and footers and should be improved in the future.

- header, footer:

  Header and footer height in units; defaults to 0. Please note: headers
  and footers are not currently supported for "px" units.

## Value

A
[`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html)
element intended for use with
[`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
and the `plot.background` theme element.

## See also

[`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html)

## Examples

``` r
if (rlang::is_installed("ggplot2")) {
  get_margin("standard")

  get_margin("none")

  get_margin(dist = 25, unit = "mm")

  get_margin(paper = "letter", block_width = 5.5)
}
#> [1] 1.5inches 1.5inches 1.5inches 1.5inches
```
