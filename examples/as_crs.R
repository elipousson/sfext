nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

as_crs(nc)$input

nc_wgs84 <- st_transform_ext(nc, 4326)

as_crs(nc_wgs84)$input

is_same_crs(nc, "NAD27")

is_wgs84(nc_wgs84)
