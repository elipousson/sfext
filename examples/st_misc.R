nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
nc <- st_transform_ext(nc, crs = 3857)

plot(nc, max.plot = 1)
plot(st_scale_rotate(nc, scale = 0.75, rotate = 15), max.plot = 1)

plot(st_square(nc[1:10,], by_feature = TRUE), max.plot = 1)

plot(st_circumscribed_circle(nc, by_feature = FALSE), max.plot = 1)
plot(st_circle(nc, by_feature = FALSE), max.plot = 1, add = TRUE)

plot(st_donut(nc[1:10,], by_feature = TRUE), max.plot = 1)

