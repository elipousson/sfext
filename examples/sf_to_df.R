nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

# Convert a sf object to a data frame
nc_df <- sf_to_df(nc)

# Convert a data frame to a sf object
df_to_sf(nc_df, coords = c("lon", "lat"), remove_coords = TRUE)

# If lon and lat values are present in a single column, use the into parameter
# to split the values back into separate columns
nc_df$xy <- paste(nc_df$lon, nc_df$lat, sep = ",")

df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
