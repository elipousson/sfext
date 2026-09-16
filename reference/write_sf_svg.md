# Write an sf object to an svg file

`write_sf_svg()` uses
[`plot()`](https://r-spatial.github.io/sf/reference/plot.html) and
[`svg()`](https://rdrr.io/r/grDevices/cairo.html) to create a simple
plot of an sf object geometry. This function is convenient for working
with designers or other collaborators interested in using spatial data
outside of R or a desktop GIS application.

## Usage

``` r
write_sf_svg(data, filename = NULL, path = NULL, ..., width = 10, height = 10)
```

## Arguments

- data:

  A sf object to save as a svg file.

- filename:

  the file path of the output file(s). The page number is substituted if
  a C integer format is included in the character string, as in the
  default. (Depending on the platform, the result must be less than
  `PATH_MAX` characters long, and may be truncated if not. See
  [`pdf`](https://rdrr.io/r/grDevices/pdf.html) for further details.)
  Tilde expansion is performed where supported by the platform.

- path:

  File path to combine with filename. Optional if filename is provided.
  filename is optional if path includes a svg file extension.

- ...:

  Arguments passed on to
  [`grDevices::svg`](https://rdrr.io/r/grDevices/cairo.html)

  `pointsize`

  :   the default pointsize of plotted text (in big points).

  `onefile`

  :   should all plots appear in one file or in separate files?

  `family`

  :   one of the device-independent font families, `"sans"`, `"serif"`
      and `"mono"`, or a character string specify a font family to be
      searched for in a system-dependent way.

      On unix-alikes (incl.\\ macOS), see the ‘Cairo fonts’ section in
      the help for [`X11`](https://rdrr.io/r/grDevices/x11.html).

  `bg`

  :   the initial background colour: can be overridden by setting
      `par("bg")`.

  `antialias`

  :   string, the type of anti-aliasing (if any) to be used; defaults to
      `"default"`.

  `fallback_resolution`

  :   numeric: the resolution in dpi used when falling back to bitmap
      output.

  `symbolfamily`

  :   a length-one character string that specifies the font family to be
      used as the "symbol" font (e.g., for
      [plotmath](https://rdrr.io/r/grDevices/plotmath.html) output).

- width:

  the width of the device in inches.

- height:

  the height of the device in inches.
