nc <- read_sf_ext(system.file("shape/nc.shp", package = "sf"))

nc <- st_transform_ext(nc, 3657)

plot(
  st_erase(
    st_buffer(nc[1, ], 1000),
    nc[1, ]
  ),
  max.plot = 1
)

plot(
  st_trim(
    nc,
    st_buffer(nc[1, ], 2000)
  ),
  max.plot = 1
)
