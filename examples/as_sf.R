nc <- read_sf_ext(system.file("shape/nc.shp", package = "sf"))
nc[["category"]] <- sample(c("A", "B", "C", "D", "E"), nrow(nc), replace = TRUE)

as_sf(nc$geometry)

as_bbox(nc)

as_sf_list(nc, col = "category")

as_crs(nc)

as_xy(nc[1,])
