nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

nc_bbox <- sf::st_bbox(nc)

nc_3857 <- st_transform_ext(nc, 3857)

st_transform_ext(nc_bbox, crs = 4326)

sf::st_crs(st_transform_ext(nc_3857, crs = nc))$input

sf::st_crs(st_wgs84(nc_3857))$input

