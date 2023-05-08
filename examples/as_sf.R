nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
nc[["category"]] <- sample(c("A", "B", "C"), nrow(nc), replace = TRUE)

as_sf(nc$geometry)

nc_bbox <- as_bbox(nc)

nc_bbox

as_sfc(nc_bbox)

as_xy(nc[1,])

as_sf_list(nc, col = "category")
