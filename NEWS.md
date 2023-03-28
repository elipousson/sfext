<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# sfext 0.1.1 (2023-03-28)

- feat: add is_wgs84 + as_wgs84
- docs: add examples for as_sf + is_sf
- docs: update codemeta + rebuild README
- feat: export st_edge
- refactor: rename list.ok to allow_list
- refactor: update staticimports
- refactor: add input check to st_bbox_ext
- refactor: stop importing map, map_lgl, and discard from purrr
- feat: drop write_exif + start exporting write_exif_from separately
- refactor: improve checking of logical inputs for is_sf and related functions
- fix: pass fileext not filetype to str_add_fileext


<!-* NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# sfext development

* Add `is_wgs84()` + `as_wgs84()` functions.
* Export `st_edge()` function
* Deprecate `get_data_dir()` function (superseded by) `filenamr::get_data_dir()`.
* Removed `write_exif()` function (moved to `filenamr::write_exif()`).
* Replace null.ok parameter with allow_null and list.ok parameter with allow_list.
* Stop importing `purrr::map()` and `purrr::map_lgl()` (replaced with `standalone-purrr.R`).
* Add new pkg parameter to `read_sf_pkg()` as alternative to package parameter.

# sfext 0.1.0.9000 (2023-03-15)

* Initial release!
