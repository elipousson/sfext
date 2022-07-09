nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

# Make a 2 by 2 grid across a location with a 1000 meter gutter between each cell
plot(
  st_make_grid_ext(
    x = nc[24, ],
    dist = 500,
    unit = "meter",
    cols = 2,
    rows = 2,
    gutter = 1000
  )
)

# Make a 5 by 5 grid with a 8.5 by 11 aspect ratio filtered to x
plot(
  st_make_grid_ext(
    x = nc[24, ],
    asp = 8.5 / 11,
    cols = 5,
    rows = 5,
    filter = TRUE
  )
)

# Make a 4 by 5 grid of circles trimmed to x boundaries
plot(
  st_make_grid_ext(
    x = nc[24, ],
    cols = 4,
    rows = 5,
    style = "circle_offset",
    trim = TRUE
  ),
  max.plot = 1
)
