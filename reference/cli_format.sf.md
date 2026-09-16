# cli_format style for sf objects

cli_format style for sf objects

## Usage

``` r
# S3 method for class 'sf'
cli_format(x, style = NULL, ...)
```

## Arguments

- x:

  A sf object to format with the cli_format.sf method.

- style:

  Not in use for the 'sf' class method.

- ...:

  Not in use for the 'sf' class method.

## See also

[`cliExtras::register_cli_format()`](https://elipousson.github.io/cliExtras/reference/register_cli_format.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
  library(cli)
  library(sf)
  cliExtras::register_cli_format("sf", cli_format.sf)
  nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
  cli_text("`nc` is a {.val {nc}}")
}
} # }
```
