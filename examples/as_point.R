nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

as_point(nc)

as_point(c("xmax", "ymax"), bbox = as_bbox(nc))

as_points(nc)

as_points(nc[1, ], nc[2, ])

nc_line <- as_line(c(as_points(nc[1, ]), as_points(nc[10, ])))

as_startpoint(nc_line)

as_endpoint(nc_line)

as_centroid(nc)
