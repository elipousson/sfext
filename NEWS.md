# sfext (development version)

## Fixes

* Correct several small typos in standalone sf utilities.
* Correct typo for `id_arg` default value in `bind_sf_coverage()`
* Correct typos for internal `cli_abort_ifnot()` and `cli_warn_ifnot()` functions.
* Fix `st_filter_pct()` always erroring due to eager evaluation of `dplyr::case_when()` branches, and forward the `pct` argument to `st_filter_pct_area()`/`st_filter_pct_length()` (it was previously dropped).
* Fix `st_filter_pct_area()` computing trim area with `get_length()` instead of `get_area()`.
* Fix `st_dissolve()` erroring on its own argument validation due to a missing `()` on the `call` default.
* Fix `st_join_ext()` erroring when `x` is a `bbox` object by converting it with `as_sf()` instead of `sf_bbox_to_sfc()`, which `sf::st_join()` cannot dispatch on.
* Fix `read_sf_csv()` passing the base `options()` function instead of the local `options` variable to `make_sf_options()`.
* Remove dangling references to `read_sf_felt()`/`is_felt_url()` in `read_sf_url()` left over from removing the `feltr` dependency.
* Fix `is_diff_area()` misusing `diff()`'s `lag` argument instead of differencing a combined vector.
* Fix `is_same_units()` only comparing unit denominators, which incorrectly returned `TRUE` for different units (e.g. `"mi"` vs `"km"`).
* Fix `get_margin()` erroring when called with no arguments.
* Fix `has_coords()` erroring when auto-detecting coordinate columns (`coords = NULL`) due to a `NULL` branch in `dplyr::case_when()`.
* Fix `get_asp()` always returning `NULL` when `bbox` was supplied.
* Fix `is_lonlat_in_range()` (used by `lonlat_to_sfc()`) checking longitude twice instead of checking latitude.
* Fix `as_bbox()` producing invalid duplicated names when building a bbox from an already-named numeric vector.
* Fix `get_length()` and `get_bearing()` erroring on geometry type conversion (e.g. POINT input) due to a reference to an undefined variable in an internal alert helper.
* Fix examples for `as_startpoint()`/`as_endpoint()`, `get_bearing()`, and `get_margin()` erroring when the `lwgeom`, `geosphere`, and `ggplot2` packages (respectively) are not installed, by guarding them with `rlang::is_installed()`.
* Document previously undocumented arguments in `address_to_sf()` (`method`, `full_results`), `read_sf_esri()` (`where`), and `write_sf_ext()`/`write_sf_gist()` (`description`, `public`, `browse`), which relied on `@inheritParams` from Suggests packages that silently failed to resolve.
* Move the "Introduction to sfext" vignette to `vignettes/articles/` so it is only built by pkgdown, not `R CMD check`/CRAN. It requires the GitHub-only `esri2sf` package and makes several live network calls (ArcGIS, GitHub Gist, Google Maps) with no offline fallback, which previously made `devtools::check()` fail outright.
* Fix `st_make_grid_ext()` using `dplyr::case_when()` with scalar conditions and length-2 vector results, which was deprecated in dplyr 1.2.0 and also silently discarded an already length-2 `n` argument (e.g. `n = c(4, 6)`), causing an unrelated downstream error.

## Tests

* Substantially expand unit test coverage across the package, including previously untested functions (e.g. `bind_sf_coverage()`, `count_features()`, `count_sf_ext()`, `st_filter_pct()`, `st_dissolve()`, `mapview_ext()`, `rdeck_edit()`), and review and improve existing tests (more robust `skip_if_not_installed()` guards, removing brittle assertions).
* Disable the `st_omerc()` `lat_0` assertion in `test-st_transform_ext.R`, which depends on the NAD27 -> WGS84 datum shift PROJ selects at runtime and is not reproducible across machines/PROJ versions.

## Changes

* Replace `st_concave_hull` with `st_concave_hull_ext()`
* Avoid using .onLoad to load packaged data (use namespacing per [guidance in R Packages book](https://r-pkgs.org/data.html#sec-data-data))

## New features

* Add rdeck helper functions `rdeck_edit()` and `rdeck_select()`
* Add mapview helper functions `mapview_ext()` and `mapview_exif()`
* Add new `read_sf_felt()` function (#5)
* Add new `st_dissolve()` function.

# sfext 0.1.1 (2023-03-28)

* Add `is_wgs84()` + `as_wgs84()` functions.
* Export `st_edge()` function.
* Deprecate `get_data_dir()` function (superseded by) `filenamr::get_data_dir()`.
* Removed `write_exif()` function (moved to `filenamr::write_exif()`).
* Replace null.ok parameter with allow_null and list.ok parameter with allow_list.
* Stop importing `purrr::map()` and `purrr::map_lgl()` (replaced with `standalone-purrr.R`).
* Add new pkg parameter to `read_sf_pkg()` as alternative to package parameter.

# sfext 0.1.0.9000 (2023-03-15)

* Initial release!
