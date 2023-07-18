# sfext (development version)

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# sfext development

* Replace st_concave_hull with `st_concave_hull_ext()`
* Add rdeck helper functions `rdeck_edit()` and `rdeck_select()`
* Add mapview helper functions `mapview_ext()` and `mapview_exif()`

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
