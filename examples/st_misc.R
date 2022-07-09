nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
nc <- st_transform_ext(nc, crs = 3857)

plot(st_scale_rotate(nc), max.plot = 1)

plot(st_scale_rotate(nc, scale = 0.6, rotate = 15), max.plot = 1)

plot(st_square(nc[5,]))

plot(st_square(nc[5,], scale = 0.6, rotate = 45), col = "blue", add = TRUE)
