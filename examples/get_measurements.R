nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

# Get area for North Caroline counties
get_area(nc[1:2,])$area
get_area(nc[1:2,], units = "acres")$area
get_area(nc[1:2,], units = "acres", .id = "acreage")$acreage

# Get distances for North Caroline counties
get_dist(nc[1,], to = c("xmax", "ymax"), units = "mile")$dist
get_dist(nc[1,], to = nc[30,], units = "km")$dist

# Create a line between two counties
nc_line <- as_line(c(as_point(nc[1,]), as_point(nc[30,])), crs = nc)

# Get length and bearing of the line
get_length(nc_line)
get_bearing(nc_line)
