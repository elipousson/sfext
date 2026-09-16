# Get standard paper and image sizes

**\[superseded\]**

## Usage

``` r
get_paper(
  paper = "letter",
  orientation = "portrait",
  standard = NULL,
  series = NULL,
  size = NULL,
  width = NULL,
  height = NULL,
  units = NULL,
  ncol = 1,
  nrow = 1,
  gutter = 0,
  bbox = NULL,
  margin = NULL,
  ...
)
```

## Arguments

- paper:

  Paper, Default: 'letter'.

- orientation:

  Orientation "portrait", "landscape", or "square", Default: 'portrait'.

- standard:

  Size standard, "ANSI", "ISO", "British Imperial", "JIS", "USPS",
  "Facebook", "Instagram", or "Twitter".

- series:

  Size series (e.g. A), Default: `NULL`

- size:

  Size number (only used for "ISO" and "JIS" series). Standard, series,
  and size may all be required to return a single paper when using these
  parameters.

- width, height:

  Width and height in units, Default: `NULL`.

- units:

  Paper size units, either "in", "mm", or "px"; defaults to `NULL`
  (using "in" if width or height are provided).

- ncol, nrow:

  Number of expected columns and rows in paper; used to determine
  row_height and section_asp in paper data frame returned by get_paper
  if nrow or ncol is greater than 1; defaults to `NULL`.

- gutter:

  Gutter distance in units. Gutter is used as the spacing between nrow
  and columns (variable spacing is not currently supported); defaults to
  0.

- bbox:

  A bounding box to use to get orientation using
  [`sf_bbox_asp()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  with orientation = TRUE.

- margin:

  A numeric vector or ggplot2 margin object.

- ...:

  Additional parameters passed to get_margin. plot_width can only be
  passed in these parameters if paper has only a single row. margin is
  returned as a list column.

## Value

Data frame with one or more paper/image sizes.

## Details

Use the "paper" parameter (matching name from
[paper_sizes](https://elipousson.github.io/sfext/reference/paper_sizes.md)),
standard (optionally including series and size) parameter, or width,
height and units. May return multiple paper sizes depending on
parameters.

If margin is provided, a block_width, block_height, and block_asp are
calculated and included as columns in the returned data frame.

Paper can also be a data frame with "width", "height", "orientation",
and "units" columns.

## Examples

``` r
get_paper("letter")
#> # A tibble: 1 × 16
#>   name   series size   standard units width height   asp  ncol  nrow col_width
#>   <chr>  <chr>  <list> <chr>    <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>     <dbl>
#> 1 Letter NA     <NULL> ANSI     in      8.5     11 0.773     1     1       8.5
#> # ℹ 5 more variables: row_height <dbl>, gutter <dbl>, section_asp <dbl>,
#> #   orientation <chr>, type <chr>

get_paper(paper = NULL, standard = "ISO", series = "A", size = 4)
#> # A tibble: 1 × 16
#>   name  series size      standard units width height   asp  ncol  nrow col_width
#>   <chr> <chr>  <list>    <chr>    <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>     <dbl>
#> 1 A4    A      <dbl [1]> ISO      mm      210    297 0.707     1     1       210
#> # ℹ 5 more variables: row_height <dbl>, gutter <dbl>, section_asp <dbl>,
#> #   orientation <chr>, type <chr>

get_paper(width = 11, height = 17)
#> # A tibble: 1 × 16
#>   name   series size   standard units width height   asp  ncol  nrow col_width
#>   <chr>  <chr>  <list> <chr>    <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>     <dbl>
#> 1 Letter NA     <NULL> ANSI     in      8.5     11 0.773     1     1       8.5
#> # ℹ 5 more variables: row_height <dbl>, gutter <dbl>, section_asp <dbl>,
#> #   orientation <chr>, type <chr>
```
