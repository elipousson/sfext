nc <- read_sf_ext(system.file("shape/nc.shp", package = "sf"))

plot(
  st_filter_ext(
    nc,
    nc[c(1:10), ]
  ),
  max.plot = 1
)

plot(
  st_filter_ext(
    nc,
    nc[c(1:10), ],
    crop = TRUE
  ),
  max.plot = 1
)


plot(
  st_filter_ext(
    nc,
    nc[c(1:10), ],
    erase = TRUE
  ),
  max.plot = 1
)

plot(
  st_filter_ext(
    nc,
    sf::st_union(nc[c(1:10), ]),
    .predicate = sf::st_disjoint
  ),
  max.plot = 1
)

st_filter_geom_type(nc, "POINT")

st_filter_geom_type(nc, "MULTIPOLYGON")
