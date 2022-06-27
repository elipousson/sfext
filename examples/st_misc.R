nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
nc <- st_transform_ext(nc, crs = 3857)

basemap <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = nc) +
  ggplot2::theme_void()

basemap +
  layer_location_data(
    data = st_scale_rotate(nc, scale = 0.5, rotate = 15),
    fill = NA,
    color = "red"
    )
