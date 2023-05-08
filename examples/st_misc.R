nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
nc <- sf::st_transform(nc, crs = 3857)

plot(nc, max.plot = 1)

plot(st_circumscribed_circle(nc, by_feature = FALSE), max.plot = 1)
plot(st_circle(nc, by_feature = FALSE), max.plot = 1, add = TRUE)

plot(st_donut(nc[1:10,], by_feature = TRUE), max.plot = 1)

