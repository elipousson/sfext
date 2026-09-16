# Package index

## Read and write spatial data

- [`read_sf_ext()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_path()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_zip()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_rdata()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_query()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_excel()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_csv()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_url()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_esri()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_gist()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_gmap()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_download()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  [`read_sf_gsheet()`](https://elipousson.github.io/sfext/reference/read_sf_ext.md)
  : Read spatial data in a bounding box to a simple feature object from
  multiple sources
- [`write_sf_ext()`](https://elipousson.github.io/sfext/reference/write_sf_ext.md)
  [`write_sf_list()`](https://elipousson.github.io/sfext/reference/write_sf_ext.md)
  [`write_sf_cache()`](https://elipousson.github.io/sfext/reference/write_sf_ext.md)
  [`write_sf_gist()`](https://elipousson.github.io/sfext/reference/write_sf_ext.md)
  [`write_sf_gsheet()`](https://elipousson.github.io/sfext/reference/write_sf_ext.md)
  : Write or cache a simple feature object to a file
- [`write_sf_svg()`](https://elipousson.github.io/sfext/reference/write_sf_svg.md)
  : Write an sf object to an svg file
- [`read_sf_exif()`](https://elipousson.github.io/sfext/reference/read_sf_exif.md)
  : Read EXIF metadata to create a simple feature object or write EXIF
  metadata to image files
- [`write_exif_from()`](https://elipousson.github.io/sfext/reference/write_exif_from.md)
  : Write EXIF data for photos on spatial join with a sf object or list
  of sf objects
- [`get_data_dir()`](https://elipousson.github.io/sfext/reference/get_data_dir.md)
  [`list_data_files()`](https://elipousson.github.io/sfext/reference/get_data_dir.md)
  **\[superseded\]** : Check if data directory exists and create a new
  directory if needed

## Check and convert sf, sfc, and bbox objects

- [`is_sf()`](https://elipousson.github.io/sfext/reference/is_sf.md)
  [`is_sfg()`](https://elipousson.github.io/sfext/reference/is_sf.md)
  [`is_sfc()`](https://elipousson.github.io/sfext/reference/is_sf.md)
  [`is_bbox()`](https://elipousson.github.io/sfext/reference/is_sf.md)
  [`is_raster()`](https://elipousson.github.io/sfext/reference/is_sf.md)
  [`is_sp()`](https://elipousson.github.io/sfext/reference/is_sf.md)
  [`is_geo_coords()`](https://elipousson.github.io/sfext/reference/is_sf.md)
  : What is the class or spatial attributes of this feature?
- [`check_sf()`](https://elipousson.github.io/sfext/reference/check_sf.md)
  : Check if x is an sf object
- [`is_geom_type()`](https://elipousson.github.io/sfext/reference/is_geom_type.md)
  [`is_point()`](https://elipousson.github.io/sfext/reference/is_geom_type.md)
  [`is_multipoint()`](https://elipousson.github.io/sfext/reference/is_geom_type.md)
  [`is_line()`](https://elipousson.github.io/sfext/reference/is_geom_type.md)
  [`is_multiline()`](https://elipousson.github.io/sfext/reference/is_geom_type.md)
  [`is_polygon()`](https://elipousson.github.io/sfext/reference/is_geom_type.md)
  [`is_multipolygon()`](https://elipousson.github.io/sfext/reference/is_geom_type.md)
  [`st_is_ext()`](https://elipousson.github.io/sfext/reference/is_geom_type.md)
  : What geometry type is this feature?
- [`as_point()`](https://elipousson.github.io/sfext/reference/as_point.md)
  [`as_points()`](https://elipousson.github.io/sfext/reference/as_point.md)
  [`as_startpoint()`](https://elipousson.github.io/sfext/reference/as_point.md)
  [`as_endpoint()`](https://elipousson.github.io/sfext/reference/as_point.md)
  [`as_line()`](https://elipousson.github.io/sfext/reference/as_point.md)
  [`as_lines()`](https://elipousson.github.io/sfext/reference/as_point.md)
  [`as_polygons()`](https://elipousson.github.io/sfext/reference/as_point.md)
  [`as_centroid()`](https://elipousson.github.io/sfext/reference/as_point.md)
  : Convert an sf, numeric, or other object to a POINT (sfg) or POINT,
  MULTIPOINT, LINESTRING, or MULTILINESTRING (sfc) object
- [`as_sf()`](https://elipousson.github.io/sfext/reference/as_sf.md)
  [`as_bbox()`](https://elipousson.github.io/sfext/reference/as_sf.md)
  [`as_sfc()`](https://elipousson.github.io/sfext/reference/as_sf.md)
  [`as_sf_class()`](https://elipousson.github.io/sfext/reference/as_sf.md)
  : Convert an object to a simple feature or bounding box object
- [`as_sf_list()`](https://elipousson.github.io/sfext/reference/sf_list.md)
  [`new_sf_list()`](https://elipousson.github.io/sfext/reference/sf_list.md)
  [`is_sf_list()`](https://elipousson.github.io/sfext/reference/sf_list.md)
  [`sf_list_rbind()`](https://elipousson.github.io/sfext/reference/sf_list.md)
  [`map_as_sf_list()`](https://elipousson.github.io/sfext/reference/sf_list.md)
  [`map_as_sf()`](https://elipousson.github.io/sfext/reference/sf_list.md)
  : Creating and checking sf lists
- [`as_xy()`](https://elipousson.github.io/sfext/reference/as_xy.md) :
  Convert data to a data frame with X/Y coordinate pairs
- [`as_crs()`](https://elipousson.github.io/sfext/reference/as_crs.md)
  [`is_same_crs()`](https://elipousson.github.io/sfext/reference/as_crs.md)
  [`is_wgs84()`](https://elipousson.github.io/sfext/reference/as_crs.md)
  : Convert object to coordinate reference system or check coordinate
  reference system

## Converting sf and data frame objects

- [`sf_to_df()`](https://elipousson.github.io/sfext/reference/sf_to_df.md)
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.md)
  : Convert between simple feature and data frame objects
- [`address_to_sf()`](https://elipousson.github.io/sfext/reference/address_to_sf.md)
  : Use tidygeocoder to convert an address or data frame with an address
  column to an sf object
- [`coords_to_sf()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
  [`check_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
  [`rev_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
  [`has_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
  [`format_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
  [`separate_coords()`](https://elipousson.github.io/sfext/reference/coords_to_sf.md)
  : Convert a data.frame with one or more coordinate columns to an sf
  object
- [`lonlat_to_sfc()`](https://elipousson.github.io/sfext/reference/lonlat_to_sfc.md)
  **\[experimental\]** : Convert a lon/lat or lat/lon coordinate pair to
  a sfc object

## Modify sf, sfc, and bbox objects

- [`st_bbox_ext()`](https://elipousson.github.io/sfext/reference/st_bbox_ext.md)
  [`st_bbox_asp()`](https://elipousson.github.io/sfext/reference/st_bbox_ext.md)
  : Get a bounding box buffered a set distance or to match an aspect
  ratio
- [`st_buffer_ext()`](https://elipousson.github.io/sfext/reference/st_buffer_ext.md)
  [`st_edge()`](https://elipousson.github.io/sfext/reference/st_buffer_ext.md)
  : Buffer a simple feature or bounding box object
- [`st_erase()`](https://elipousson.github.io/sfext/reference/st_erase.md)
  [`st_trim()`](https://elipousson.github.io/sfext/reference/st_erase.md)
  : Erase or trim geometry of a sf or sfc object
- [`st_clip()`](https://elipousson.github.io/sfext/reference/st_clip.md)
  : Clip the side or corner of a simple feature or bounding box object
- [`st_concave_hull_ext()`](https://elipousson.github.io/sfext/reference/st_concave_hull_ext.md)
  : Make a concave hull around simple feature object by attribute
- [`st_union_ext()`](https://elipousson.github.io/sfext/reference/st_union_ext.md)
  [`st_union_by()`](https://elipousson.github.io/sfext/reference/st_union_ext.md)
  : Union simple feature objects and combine name column values
- [`st_dissolve()`](https://elipousson.github.io/sfext/reference/st_dissolve.md)
  : Dissolve geometry preserving existing or supplied grouping variables
- [`st_join_ext()`](https://elipousson.github.io/sfext/reference/st_join_ext.md)
  : Complete a spatial join using a simple feature objects or an object
  and list
- [`st_filter_ext()`](https://elipousson.github.io/sfext/reference/st_filter_ext.md)
  [`st_filter_geom_type()`](https://elipousson.github.io/sfext/reference/st_filter_ext.md)
  : Filter, crop, trim, or erase a simple feature object or list
- [`st_filter_pct()`](https://elipousson.github.io/sfext/reference/st_filter_pct.md)
  [`st_filter_pct_area()`](https://elipousson.github.io/sfext/reference/st_filter_pct.md)
  [`st_filter_pct_length()`](https://elipousson.github.io/sfext/reference/st_filter_pct.md)
  **\[experimental\]** : Filter by share of length or area of one
  geometry overlapping with a second geometry
- [`st_center()`](https://elipousson.github.io/sfext/reference/st_misc.md)
  [`st_circle()`](https://elipousson.github.io/sfext/reference/st_misc.md)
  [`st_circumscribed_circle()`](https://elipousson.github.io/sfext/reference/st_misc.md)
  [`st_donut()`](https://elipousson.github.io/sfext/reference/st_misc.md)
  : Modify the geometry of a simple feature or bounding box object
- [`st_square()`](https://elipousson.github.io/sfext/reference/st_square.md)
  [`st_inscribed_square()`](https://elipousson.github.io/sfext/reference/st_square.md)
  : Create a square within or around a simple feature object or
  collection
- [`st_scale_rotate()`](https://elipousson.github.io/sfext/reference/st_scale_rotate.md)
  : Scale and rotate a simple feature object, simple feature collection,
  or bounding box
- [`st_nudge()`](https://elipousson.github.io/sfext/reference/st_nudge.md)
  : Nudge a simple feature to the center of another feature and/or a set
  distance
- [`st_transform_ext()`](https://elipousson.github.io/sfext/reference/st_transform_ext.md)
  [`st_omerc()`](https://elipousson.github.io/sfext/reference/st_transform_ext.md)
  [`st_wgs84()`](https://elipousson.github.io/sfext/reference/st_transform_ext.md)
  : Transform or convert coordinates of a simple feature or bounding box
  object
- [`st_make_valid_ext()`](https://elipousson.github.io/sfext/reference/st_make_valid_ext.md)
  : Checks if all geometries are already valid and make valid if not
- [`st_cast_ext()`](https://elipousson.github.io/sfext/reference/st_cast_ext.md)
  : Cast geometry of a simple feature object or simple feature
  collection to another type
- [`st_make_grid_ext()`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.md)
  : Make a grid over a simple feature bounding box

## Get information about sf, sfc, and bbox objects

- [`get_coords()`](https://elipousson.github.io/sfext/reference/get_coords.md)
  [`get_minmax()`](https://elipousson.github.io/sfext/reference/get_coords.md)
  : Get coordinates for a simple feature or bounding box object
- [`get_area()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  [`st_area_ext()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  [`get_length()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  [`st_length_ext()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  [`get_dist()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  [`st_distance_ext()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  [`get_bearing()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  [`st_bearing()`](https://elipousson.github.io/sfext/reference/get_measurements.md)
  : Get measurements for simple feature objects

## Additional utility functions for sf and bbox objects

- [`transform_sf()`](https://elipousson.github.io/sfext/reference/misc_sf.md)
  [`relocate_sf_col()`](https://elipousson.github.io/sfext/reference/misc_sf.md)
  [`rename_sf_col()`](https://elipousson.github.io/sfext/reference/misc_sf.md)
  [`get_sf_col()`](https://elipousson.github.io/sfext/reference/misc_sf.md)
  [`get_sf_colnames()`](https://elipousson.github.io/sfext/reference/misc_sf.md)
  : Additional utility functions for sf objects
- [`number_features()`](https://elipousson.github.io/sfext/reference/number_features.md)
  [`number_sf()`](https://elipousson.github.io/sfext/reference/number_features.md)
  [`sort_features()`](https://elipousson.github.io/sfext/reference/number_features.md)
  [`sort_sf()`](https://elipousson.github.io/sfext/reference/number_features.md)
  : Sort and number features by coordinates or distance
- [`count_features()`](https://elipousson.github.io/sfext/reference/count_features.md)
  : Count simple features based on relationship with a second simple
  feature object
- [`count_sf_ext()`](https://elipousson.github.io/sfext/reference/count_sf_ext.md)
  : Count extended for working with sf objects
- [`make_sf_grid_list()`](https://elipousson.github.io/sfext/reference/make_sf_grid_list.md)
  : Make a sf list by grid position
- [`sf_bbox_corners()`](https://elipousson.github.io/sfext/reference/sf_bbox_corners.md)
  : Get bounding box corner points from a bbox, sfc, or sf object
- [`sf_bbox_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  [`sf_bbox_xdist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  [`sf_bbox_ydist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  [`sf_bbox_diagdist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  [`sf_bbox_diag_ratio_to_dist()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  [`sf_bbox_asp()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  [`sf_bbox_orientation()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  [`sf_bbox_check_fit()`](https://elipousson.github.io/sfext/reference/sf_bbox_dist.md)
  : Measure a bounding box using x, y, or diagonal distance
- [`sf_bbox_to_sf()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
  [`sf_bbox_to_sfc()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
  [`sf_bbox_transform()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
  [`sf_bbox_point()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
  [`sf_bbox_to_wkt()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
  [`sf_bbox_to_lonlat_query()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
  [`sf_bbox_to_npc()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.md)
  : Convert and tranform bounding boxes
- [`sf_bbox_shift()`](https://elipousson.github.io/sfext/reference/sf_bbox_shift.md)
  [`sf_bbox_contract()`](https://elipousson.github.io/sfext/reference/sf_bbox_shift.md)
  [`sf_bbox_expand()`](https://elipousson.github.io/sfext/reference/sf_bbox_shift.md)
  : Shift sides, contract, or expand a bounding box
- [`cli_format(`*`<sf>`*`)`](https://elipousson.github.io/sfext/reference/cli_format.sf.md)
  : cli_format style for sf objects

## Utility functions for units and scales

- [`is_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`is_diff_dist()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`is_same_dist()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`is_longer()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`is_shorter()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`get_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`as_dist_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`is_diff_area()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`is_same_area()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  [`is_same_units()`](https://elipousson.github.io/sfext/reference/is_dist_units.md)
  : General utility functions for working with distance units objects
- [`convert_dist_units()`](https://elipousson.github.io/sfext/reference/convert_dist_units.md)
  : Convert distance (and area) values between different units
- [`convert_dist_scale()`](https://elipousson.github.io/sfext/reference/convert_dist_scale.md)
  : Convert distance from scale to actual units
- [`compare_dist()`](https://elipousson.github.io/sfext/reference/compare_dist.md)
  : Compare a distance to the dimension of a bounding box or another
  distance value
- [`bind_units_col()`](https://elipousson.github.io/sfext/reference/bind_units_col.md)
  : Bind units column to data frame
- [`get_scale()`](https://elipousson.github.io/sfext/reference/get_scale.md)
  : Get standard scales and convert to scale distances

## Utility functions for paper and image sizes

- [`get_paper()`](https://elipousson.github.io/sfext/reference/get_paper.md)
  **\[superseded\]** : Get standard paper and image sizes
- [`get_social_image()`](https://elipousson.github.io/sfext/reference/get_social_image.md)
  : Get social media image size to match platform and format
- [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.md)
  : Get aspect ratio from string or based on specific paper and margins
- [`get_margin()`](https://elipousson.github.io/sfext/reference/get_margin.md)
  : Get margins for a ggplot2 plot or map based on style or distance

## Helper functions for interactive mapping

- [`mapview_ext()`](https://elipousson.github.io/sfext/reference/mapview_ext.md)
  [`mapview_exif()`](https://elipousson.github.io/sfext/reference/mapview_ext.md)
  [`mapview_popup_img()`](https://elipousson.github.io/sfext/reference/mapview_ext.md)
  : Use mapview to interactively explore spatial data
- [`rdeck_edit()`](https://elipousson.github.io/sfext/reference/rdeck_edit.md)
  [`rdeck_select()`](https://elipousson.github.io/sfext/reference/rdeck_edit.md)
  [`editor_options()`](https://elipousson.github.io/sfext/reference/rdeck_edit.md)
  : rdeck editor

## Reference data for units, scales, and paper

- [`dist_units`](https://elipousson.github.io/sfext/reference/dist_units.md)
  : Distance units (data frame)
- [`dist_unit_options`](https://elipousson.github.io/sfext/reference/dist_unit_options.md)
  : Distance units (vector)
- [`area_unit_options`](https://elipousson.github.io/sfext/reference/area_unit_options.md)
  : Area units (vector)
- [`standard_scales`](https://elipousson.github.io/sfext/reference/standard_scales.md)
  : Standard map, architectural, and engineering scales
- [`paper_sizes`](https://elipousson.github.io/sfext/reference/paper_sizes.md)
  : Standard paper and image sizes
