# Shift sides, contract, or expand a bounding box

- Shift, expand, or contract a bounding box (`sf_bbox_shift()`,
  `sf_bbox_expand()`, `sf_bbox_contract()`)

## Usage

``` r
sf_bbox_shift(
  bbox,
  nudge_x = 0,
  nudge_y = 0,
  side = c("all", "top", "bottom", "left", "right"),
  dir = NULL,
  call = caller_env()
)

sf_bbox_contract(bbox, nudge_x = 0, nudge_y = 0)

sf_bbox_expand(bbox, nudge_x = 0, nudge_y = 0)
```

## Arguments

- bbox:

  A `bbox` object.

- nudge_x, nudge_y:

  Length 1 or 2 numeric vector; unitless. Used by or passed to
  `sf_bbox_shift()`. Required for `sf_bbox_shift()`.

- side:

  one or more sides to shift: "top", "bottom", "left", "right", or
  "all". Required for `sf_bbox_shift()`.

- dir:

  If "in", contract the `bbox` by nudge_x and nudge_y. If "out", expand
  the bbox by nudge_x and nudge_y. If dir is not `NULL`; absolute values
  are used for nudge_x and nudge_y. Defaults to `NULL`. Optional
  `sf_bbox_shift()`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
