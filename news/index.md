# Changelog

## sfext (development version)

### Fixes

- Correct several small typos in standalone sf utilities.
- Correct typo for `id_arg` default value in
  [`bind_sf_coverage()`](https://elipousson.github.io/sfext/reference/bind_sf_coverage.md)
- Correct typos for internal `cli_abort_ifnot()` and `cli_warn_ifnot()`
  functions.
- Fix
  [`st_filter_pct()`](https://elipousson.github.io/sfext/reference/st_filter_pct.md)
  always erroring due to eager evaluation of
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  branches, and forward the `pct` argument to
  [`st_filter_pct_area()`](https://elipousson.github.io/sfext/reference/st_filter_pct.md)/[`st_filter_pct_length()`](https://elipousson.github.io/sfext/reference/st_filter_pct.md)
  (it was previously dropped).
- Fix
  [`st_filter_pct_area()`](https://elipousson.github.io/sfext/reference/st_filter_pct.md)
  computing trim area with
  [`get_length()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  instead of
  [`get_area()`](https://elipousson.github.io/sfext/reference/get_measurements.md).
- Fix
  [`st_dissolve()`](https://elipousson.github.io/sfext/reference/st_dissolve.md)
  erroring on its own argument validation due to a missing `()` on the
  `call` default.
- Fix
  [`st_join_ext()`](https://elipousson.github.io/sfext/reference/st_join_ext.md)
  erroring when `x` is a `bbox` object by converting it with
  [`as_sf()`](https://elipousson.github.io/sfext/reference/as_sf.md)
  instead of
  [`sf_bbox_to_sfc()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md),
  which
  [`sf::st_join()`](https://r-spatial.github.io/sf/reference/st_join.html)
  cannot dispatch on.
- Fix
  [`read_sf_csv()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  passing the base [`options()`](https://rdrr.io/r/base/options.html)
  function instead of the local `options` variable to
  `make_sf_options()`.
- Remove dangling references to `read_sf_felt()`/`is_felt_url()` in
  [`read_sf_url()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  left over from removing the `feltr` dependency.
- Fix
  [`is_diff_area()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  misusing [`diff()`](https://rdrr.io/r/base/diff.html)’s `lag` argument
  instead of differencing a combined vector.
- Fix
  [`is_same_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  only comparing unit denominators, which incorrectly returned `TRUE`
  for different units (e.g. `"mi"` vs `"km"`).
- Fix
  [`get_margin()`](https://elipousson.github.io/sfext/reference/get_margin.md)
  erroring when called with no arguments.
- Fix
  [`has_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
  erroring when auto-detecting coordinate columns (`coords = NULL`) due
  to a `NULL` branch in
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html).
- Fix
  [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.md)
  always returning `NULL` when `bbox` was supplied.
- Fix `is_lonlat_in_range()` (used by
  [`lonlat_to_sfc()`](https://elipousson.github.io/sfext/reference/lonlat_to_sfc.md))
  checking longitude twice instead of checking latitude.
- Fix
  [`as_bbox()`](https://elipousson.github.io/sfext/reference/as_sf.md)
  producing invalid duplicated names when building a bbox from an
  already-named numeric vector.
- Fix
  [`get_length()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  and
  [`get_bearing()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  erroring on geometry type conversion (e.g. POINT input) due to a
  reference to an undefined variable in an internal alert helper.
- Fix examples for
  [`as_startpoint()`](https://elipousson.github.io/sfext/reference/as_point.md)/[`as_endpoint()`](https://elipousson.github.io/sfext/reference/as_point.md),
  [`get_bearing()`](https://elipousson.github.io/sfext/reference/get_measurements.md),
  and
  [`get_margin()`](https://elipousson.github.io/sfext/reference/get_margin.md)
  erroring when the `lwgeom`, `geosphere`, and `ggplot2` packages
  (respectively) are not installed, by guarding them with
  [`rlang::is_installed()`](https://rlang.r-lib.org/reference/is_installed.html).
- Document previously undocumented arguments in
  [`address_to_sf()`](https://elipousson.github.io/sfext/reference/address_to_sf.md)
  (`method`, `full_results`),
  [`read_sf_esri()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  (`where`), and
  [`write_sf_ext()`](https://elipousson.github.io/sfext/reference/write_sf_ext.md)/[`write_sf_gist()`](https://elipousson.github.io/sfext/reference/write_sf_ext.md)
  (`description`, `public`, `browse`), which relied on `@inheritParams`
  from Suggests packages that silently failed to resolve.

### Tests

- Substantially expand unit test coverage across the package, including
  previously untested functions
  (e.g. [`bind_sf_coverage()`](https://elipousson.github.io/sfext/reference/bind_sf_coverage.md),
  [`count_features()`](https://elipousson.github.io/sfext/reference/count_features.md),
  [`count_sf_ext()`](https://elipousson.github.io/sfext/reference/count_sf_ext.md),
  [`st_filter_pct()`](https://elipousson.github.io/sfext/reference/st_filter_pct.md),
  [`st_dissolve()`](https://elipousson.github.io/sfext/reference/st_dissolve.md),
  [`mapview_ext()`](https://elipousson.github.io/sfext/reference/mapview_ext.md),
  [`rdeck_edit()`](https://elipousson.github.io/sfext/reference/rdeck_edit.md)),
  and review and improve existing tests (more robust
  `skip_if_not_installed()` guards, removing brittle assertions).

### Changes

- Replace `st_concave_hull` with
  [`st_concave_hull_ext()`](https://elipousson.github.io/sfext/reference/st_concave_hull_ext.md)
- Avoid using .onLoad to load packaged data (use namespacing per
  [guidance in R Packages
  book](https://r-pkgs.org/data.html#sec-data-data))

### New features

- Add rdeck helper functions
  [`rdeck_edit()`](https://elipousson.github.io/sfext/reference/rdeck_edit.md)
  and
  [`rdeck_select()`](https://elipousson.github.io/sfext/reference/rdeck_edit.md)
- Add mapview helper functions
  [`mapview_ext()`](https://elipousson.github.io/sfext/reference/mapview_ext.md)
  and
  [`mapview_exif()`](https://elipousson.github.io/sfext/reference/mapview_ext.md)
- Add new `read_sf_felt()` function
  ([\#5](https://github.com/elipousson/sfext/issues/5))
- Add new
  [`st_dissolve()`](https://elipousson.github.io/sfext/reference/st_dissolve.md)
  function.

## sfext 0.1.1 (2023-03-28)

- Add
  [`is_wgs84()`](https://elipousson.github.io/sfext/reference/as_crs.md) +
  [`as_wgs84()`](https://elipousson.github.io/sfext/reference/st_transform_ext.md)
  functions.
- Export
  [`st_edge()`](https://elipousson.github.io/sfext/reference/st_buffer_ext.md)
  function.
- Deprecate
  [`get_data_dir()`](https://elipousson.github.io/sfext/reference/get_data_dir.md)
  function (superseded by)
  [`filenamr::get_data_dir()`](https://elipousson.github.io/filenamr/reference/get_data_dir.html).
- Removed `write_exif()` function (moved to
  [`filenamr::write_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.html)).
- Replace null.ok parameter with allow_null and list.ok parameter with
  allow_list.
- Stop importing
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) and
  [`purrr::map_lgl()`](https://purrr.tidyverse.org/reference/map.html)
  (replaced with `standalone-purrr.R`).
- Add new pkg parameter to
  [`read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  as alternative to package parameter.

## sfext 0.1.0.9000 (2023-03-15)

- Initial release!
