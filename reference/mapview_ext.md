# Use mapview to interactively explore spatial data

A wrapper for
[`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html)
that drops list columns and makes it easier to quickly specify a zcol
value.

## Usage

``` r
mapview_ext(x, zcol = NULL, remove_na = FALSE, ...)

mapview_exif(
  path = NULL,
  fileext = "jpeg",
  popup = TRUE,
  tooltip = FALSE,
  images = NULL,
  width = 320,
  ...
)

mapview_popup_img(
  images,
  popup = TRUE,
  tooltip = FALSE,
  map = NULL,
  width = 320,
  ...,
  call = caller_env()
)
```

## Arguments

- x:

  a `Raster*` or `Spatial*` or `Satellite` or `sf` or `stars` object or
  a list of any combination of those. Furthermore, this can also be a
  `data.frame`, a `numeric vector` or a `character string` pointing to a
  tile image folder or file on disk. If missing, a blank map will be
  drawn. A value of NULL will return NULL.

- zcol:

  attribute name(s) or column number(s) in attribute table of the
  column(s) to be rendered. See also Details.

- remove_na:

  If `TRUE` and `zcol` is not `NULL`, filter `NA` values from the data
  frame column defined by `zcol` before passing to
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html)

- ...:

  Arguments passed on to
  [`mapview::mapview`](https://r-spatial.github.io/mapview/reference/mapView.html)

  `maxpixels`

  :   integer \> 0. Maximum number of cells to use for the plot. If
      maxpixels \< `ncell(x)`, sampleRegular is used before plotting.

  `col.regions`

  :   color (palette) pixels. See
      [`levelplot`](https://rdrr.io/pkg/lattice/man/levelplot.html) for
      details.

  `at`

  :   the breakpoints used for the visualisation. See
      [`levelplot`](https://rdrr.io/pkg/lattice/man/levelplot.html) for
      details.

  `na.color`

  :   color for missing values

  `use.layer.names`

  :   should layer names of the Raster\* object be used?

  `map.types`

  :   character specifications for the base maps. see
      <https://leaflet-extras.github.io/leaflet-providers/preview/> for
      available options.

  `alpha.regions`

  :   opacity of the fills of points, polygons or raster layer(s)

  `legend`

  :   should a legend be plotted

  `legend.opacity`

  :   opacity of the legend

  `trim`

  :   should the raster be trimmed in case there are NAs on the edges

  `verbose`

  :   should some details be printed during the process

  `layer.name`

  :   the name of the layer to be shown on the map. By default this is
      the character version of whatever is passed to `x`. NOTE: This is
      being passed to underlying leaflet functions as the group
      argument. So if you use mapview to set up a map and want to refer
      to a certain layer later on, this is what you should refer to in
      `group`.

  `homebutton`

  :   logical, whether to add a zoom-to-layer button to the map.
      Defaults to TRUE

  `native.crs`

  :   logical whether to reproject to web map coordinate reference
      system (web mercator - epsg:3857) or render using native CRS of
      the supplied data (can also be NA). Default is FALSE which will
      render in web mercator. If set to TRUE now background maps will be
      drawn (but rendering may be much quicker as no reprojecting is
      necessary). Currently only works for simple features.

  `method`

  :   for raster data only (raster/stars). Method used to compute values
      for the resampled layer that is passed on to leaflet. mapview does
      projection on-the-fly to ensure correct display and therefore
      needs to know how to do this projection. The default is 'bilinear'
      (bilinear interpolation), which is appropriate for continuous
      variables. The other option, 'ngb' (nearest neighbor), is useful
      for categorical variables. Ignored if the raster layer is of class
      `factor` in which case "ngb" is used.

  `label`

  :   For vector data (sf/sp) a character vector of labels to be shown
      on mouseover. See
      [`addControl`](https://rstudio.github.io/leaflet/reference/map-layers.html)
      for details. For raster data (Raster\*/stars) a logical indicating
      whether to add image query.

  `query.type`

  :   for raster methods only. Whether to show raster value query on
      `'mousemove'` or `'click'`. Ignored if `label = FALSE`.

  `query.digits`

  :   for raster methods only. The amount of digits to be shown by
      raster value query. Ignored if `label = FALSE`.

  `query.position`

  :   for raster methods only. The position of the raster value query
      info box. See `position` argument of
      [`addLegend`](https://rstudio.github.io/leaflet/reference/addLegend.html)
      for possible values. Ignored if `label = FALSE`.

  `query.prefix`

  :   for raster methods only. a character string to be shown as prefix
      for the layerId. Ignored if `label = FALSE`.

  `viewer.suppress`

  :   deprecated. Use `mapviewOptions(viewer.suppress = TRUE/FALSE)`
      instead.

  `hide`

  :   either a logical, a vector of layer names or a vector of layer
      indices. See Details for more information on what exactly it does
      for different raster types.

  `band`

  :   for stars layers, the band number to be plotted.

  `pane`

  :   name of the map pane in which to render features. See
      [`addMapPane`](https://rstudio.github.io/leaflet/reference/addMapPane.html)
      for details. Currently only supported for vector layers. Ignored
      if `canvas = TRUE`. The default `"auto"` will create different
      panes for points, lines and polygons such that points overlay
      lines overlay polygons. Set to `NULL` to get default leaflet
      behaviour where allfeatures are rendered in the same pane and
      layer order is determined automatically/sequentially.

  `canvas`

  :   whether to use canvas rendering rather than svg. May help
      performance with larger data. See
      <https://leafletjs.com/index.html#canvas> for more information.
      Only applicable for vector data. The default setting will decide
      automatically, based on feature complexity.

  `burst`

  :   whether to show all (TRUE) or only one (FALSE) layer(s). See also
      Details.

  `color`

  :   color (palette) for points/polygons/lines

  `cex`

  :   attribute name(s) or column number(s) in attribute table of the
      column(s) to be used for defining the size of circles

  `lwd`

  :   line width

  `alpha`

  :   opacity of lines

  `na.alpha`

  :   opacity of missing values

  `highlight`

  :   either `FALSE`, `NULL` or a list of styling options for feature
      highlighting on mouse hover. See
      [`highlightOptions`](https://rstudio.github.io/leaflet/reference/map-layers.html)
      for details.

  `maxpoints`

  :   the maximum number of points making up the geometry. In case of
      lines and polygons this refers to the number of vertices. See
      Details for more information.

  `tms`

  :   whether the tiles are served as TMS tiles.

  `y`

  :   numeric vector.

  `type`

  :   whether to render the numeric vector `x` as a point `"p"` or line
      `"l"` plot.

  `grid`

  :   whether to plot a (scatter plot) xy-grid to aid interpretation of
      the visualisation. Only relevant for the data.frame method.

  `xcol`

  :   the column to be mapped to the x-axis. Only relevant for the
      data.frame method.

  `ycol`

  :   the column to be mapped to the y-axis. Only relevant for the
      data.frame method.

  `aspect`

  :   the ratio of x/y axis coordinates to adjust the plotting space to
      fit the screen. Only relevant for the data.frame method.

  `crs`

  :   an optional crs specification for the provided data to enable
      rendering on a basemap. See argument description in
      [`st_sf`](https://r-spatial.github.io/sf/reference/sf.html) for
      details.

- path:

  A path to folder or file.

- fileext:

  File extension. Defaults to "jpeg".

- popup:

  If `TRUE`, add a popup image to a leaflet map; defaults `TRUE`.

- tooltip:

  logical, whether to show image(s) as popup(s) (on click) or tooltip(s)
  (on hover).

- images:

  A simple feature object with columns for the image path/url, image
  width, and image height.

- width:

  the width of the image(s) in pixels.

- map:

  an optional existing map to be updated/added to.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## See also

[`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html)
