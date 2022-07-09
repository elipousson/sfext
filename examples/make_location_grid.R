nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

# Make a 4 by 5 grid covering a mile buffer around a location
make_location_grid(
  location = nc[24,],
  dist = 1,
  unit = "mile",
  cols = 4,
  rows = 5
)

# Make a 2 by 2 grid across a location with a 1000 meter gutter between each cell
make_location_grid(
  location = nc[24,],
  dist = 500,
  unit = "meter",
  cols = 2,
  rows = 2,
  gutter = 1000
)
