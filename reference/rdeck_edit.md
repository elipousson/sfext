# rdeck editor

A wrapper for
[`rdeck::editor_options()`](https://qfes.github.io/rdeck/reference/editor_options.html)
that automatically converts features to WGS84.

## Usage

``` r
rdeck_edit(features, mode = rdeck::cur_value(), initial_bounds = NULL, ...)

rdeck_select(features, ..., mode = "select")

editor_options(mode = rdeck::cur_value(), features = rdeck::cur_value())
```

## Arguments

- features:

  \<[`wk-geometry`](https://qfes.github.io/rdeck/reference/wk-geometry.html)\>
  Features with which to initialise the editor. Requires CRS
  [EPSG:4326](http://epsg.io/4326).

- mode:

  \<`editor-mode`\> The polygon editor mode. One of:

  - `view`: editor is in readonly mode

  - `select`: select/unselect features

  - `modify`: add/move/delete vertices

  - `transform`: move/scale/rotate selected features

  - `point`: draw points

  - `linestring`: draw linestrings by clicking each vertex

  - `polygon`: draw polygons by clicking each vertex

  - `lasso`: freehand polygon draw by click-dragging

- initial_bounds:

  \<[`rct`](https://paleolimbot.github.io/wk/reference/rct.html)/[`st_bbox`](https://r-spatial.github.io/sf/reference/st_bbox.html)/[`wk-geometry`](https://qfes.github.io/rdeck/reference/wk-geometry.html)\>
  Sets the initial bounds of the map if not `NULL`. Takes priority over
  `initial_view_state`. Accepts a bounding box, or a geometry from which
  a bounding box can be computed. Requires CRS
  [EPSG:4326](http://epsg.io/4326).

- ...:

  Arguments passed on to
  [`rdeck::rdeck`](https://qfes.github.io/rdeck/reference/rdeck.html)

  `map_style`

  :   \<`string`\> The mapbox basemap style url. See
      <https://docs.mapbox.com/api/maps/#mapbox-styles>

  `theme`

  :   \<`"kepler"` \| `"light"`\> The widget theme which alters the
      style of the legend and tooltips.

  `initial_view_state`

  :   \<[`view_state`](https://qfes.github.io/rdeck/reference/view_state.html)\>
      Defines the map position, zoom, bearing and pitch.

  `controller`

  :   \<`logical`\> If `NULL` or `FALSE`, the map is not interactive.

  `picking_radius`

  :   \<`number`\> Extra pixels around the pointer to include while
      picking; useful when rendering objects that are difficult to
      hover, e.g. thin lines, small points, etc.

  `use_device_pixels`

  :   \<`logical` \| `number`\> Controls the resolution of drawing
      buffer used for rendering.

      - `TRUE`: Resolution is defined by `window.devicePixelRatio`. On
        Retina/HD displays, this resolution is usually twice as big as
        `CSS pixels` resolution.

      - `FALSE`: `CSS pixels` resolution is used for rendering.

      - `number`: Custom ratio (drawing buffer resolution to
        `CSS pixel`) to determine drawing buffer size. A value less than
        `1` uses resolution smaller than `CSS pixels`, improving
        rendering performance at the expense of image quality; a value
        greater than `1` improves image quality at the expense of
        rendering performance.

  `blending_mode`

  :   \<`"normal"` \| `"additive"` \| `"subtractive"`\> Sets the
      blending mode. Blending modes:

      - `normal`: Normal blending doesn't alter colours of overlapping
        objects.

      - `additive`: Additive blending adds colours of overlapping
        objects. Useful for highlighting dot density on dark maps.

      - `subtractive`: Subtractive blending darkens overlapping objects.
        Useful for highlighting dot density on light maps.

  `layer_selector`

  :   \<`boolean`\> If `TRUE`, the layer selector control will be
      enabled and layers with `visibility_toggle = TRUE` may be toggled.
      If `FALSE`, the layer selector control won't be rendered.

  `editor`

  :   \<`boolean`\|[`editor_options`](https://qfes.github.io/rdeck/reference/editor_options.html)\>
      Whether to render the polygon editor. If `TRUE`, renders with the
      default
      [`editor_options()`](https://qfes.github.io/rdeck/reference/editor_options.html).
      If `FALSE`, the polygon editor is not rendered.

  `lazy_load`

  :   **\[deprecated\]**. Maps are always eagerly rendered.

  `width`

  :   \<`number`\> The width of the map canvas.

  `height`

  :   \<`number`\> The height of the map canvas.

  `id`

  :   \<`string`\> The map element id. Not used in shiny applications.
