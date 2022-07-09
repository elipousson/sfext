nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
nc_union <- st_union_ext(nc[10:15,], name_col = "NAME")
nc_union
plot(nc_union)
