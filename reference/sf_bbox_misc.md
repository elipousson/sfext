# Convert and tranform bounding boxes

Simple bounding box functions that you can use to:

- Convert a bounding box to a simple feature object (`sf_bbox_to_sf()`)
  or simple feature collection (`sf_bbox_to_sfc()`)

- Transform the coordinate reference system (`sf_bbox_transform()`)

- Return a point from any of the corners, center, or midpoints
  (`sf_bbox_point()`)

- Convert a bounding box to a SQL style query
  (`sf_bbox_to_lonlat_query()`), well known text (`sf_bbox_to_wkt()`)

- Convert a point and a corresponding bounding box into into a npc
  (normalised parent coordinates) value with `sf_bbox_to_npc()`

If sf (\>=1.0-17) is installed, `sf_bbox_transform()` uses
[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)
*without* converting to a sfc object and back to bbox.

## Usage

``` r
sf_bbox_to_sf(bbox, sf_col = "geometry")

sf_bbox_to_sfc(bbox)

sf_bbox_transform(bbox, crs = NULL)

sf_bbox_point(bbox, point = NULL, crs = NULL, call = caller_env())

sf_bbox_to_wkt(bbox, crs = NULL)

sf_bbox_to_lonlat_query(bbox, coords = c("longitude", "latitude"), crs = 4326)

sf_bbox_to_npc(bbox, point)
```

## Arguments

- bbox:

  A bbox object.

- sf_col:

  name to use for geometry column after converting to simple feature
  object; defaults to "geometry".

- crs:

  A coordinate reference system to use for `sf_bbox_to_lonlat_query()`
  (defaults to 4326) or for resulting bounding box
  (`sf_bbox_transform()`) or sfc object (`sf_bbox_point()`) (defaults to
  `NULL`).

- point:

  point to find npc coords for center

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- coords:

  Column names with coordinates for query. e.g. `c("X", "Y")` or
  `c("longitude", "latitude")` (default). Used by
  `sf_bbox_to_lonlat_query()` only.

## Value

sf object

## See also

- [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html),[`sf::st_as_sfc()`](https://r-spatial.github.io/sf/reference/st_as_sfc.html)

- [st_extent()](https://seasmith.github.io/packages/sfx/reference/st_extent.html)
  in sfx package
