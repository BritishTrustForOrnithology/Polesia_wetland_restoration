##################################################################
# MATCHING POLYGONS WITH SYSTEM INDEX FROM GEE DATA
# M. Kirkland
# 03/09/24
##################################################################

library(sf)
library(dplyr)
library(lwgeom)  

# Which point belongs to which polgyon 
# Load example index, doesn't matter which one
str <- read.csv("./data/GEE_data/STR.csv")
# Convert to spatial format
str_sf <- cbind(st_as_sf(geojson_sf(str$.geo), st_crs = 4326), str)
# Load split polygons
list_polys <- list.files(pattern = "split.*\\.shp$", recursive = T)
polys <- lapply(list_polys, "st_read")
polys_flat <- bind_rows(polys)
polys_flat$polygon <- 1:nrow(polys_flat)
polys_flat$area <- as.numeric(st_area(st_make_valid(polys_flat)))

# Match them up
match <- st_intersection(str_sf, st_make_valid(polys_flat))
# Select columns of interest
match <- subset(match, select = c("system.index", "polygon", "area"))

# Export
write.csv(st_drop_geometry(match), "./data/GEE_data/poly_system_ind.csv", row.names = F)
