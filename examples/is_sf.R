nc <- read_sf_ext(system.file("shape/nc.shp", package = "sf"))

is_sf(nc)

is_sfc(nc$geometry)

is_sf_list(list(nc[1, ], nc[2, ]))

is_bbox(sf::st_bbox(nc))

is_wgs84(sf::st_transform(nc, 4326))

is_same_crs(nc, 4267)
