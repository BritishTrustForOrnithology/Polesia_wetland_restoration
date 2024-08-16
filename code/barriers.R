##################################################################
# DRAWING PERPENDICULAR TRANSECTS ALONG BOUNDARIES IN POLESIA
# M. Kirkland
# 04/06/24
##################################################################

# Clear data
rm(list = ls())

# Libraries
library(sf) 
library(lwgeom)

# Boundary data supplied by Viktar Fenchuk
setwd("./data")
road <- st_read("Almany-border.kml")

# Transform to coordinate system of Ukraine
road_tr <- st_transform(road, crs = 6383)

# Roughly how many transects should I draw if seperated by 50m 
st_length(road_tr)/50

# Sample points evenly along road
pts <- st_line_sample(road_tr, n = 520, type = "regular") |>
  sf::st_cast(to = "POINT")

# Plot this
plot(road_tr$geometry)
plot(pts, add = T)

# Coordinates of every node of line 
points_from_boundary <- road_tr |>
  sf::st_coordinates() |>
  as.data.frame() |>
  sf::st_as_sf(coords = c("X", "Y"), crs = 6383)

 # Create empty list
perp_line <- rep(list(vector("list", length = 10)), 520)

# Vector of different lengths to create transects 
tlen <- seq(100,1000, by = 100) # Distance in m

for (i in 1:length(pts)) {
  
 # Find closest node
    nearest_pt <-
    st_as_sf(points_from_boundary$geometry[st_nearest_feature(pts[i], points_from_boundary)])
  
    theta = atan2(y = st_coordinates(pts[i])[,2] - unlist(st_geometry(nearest_pt))[2], st_coordinates(pts[i])[,1] - unlist(st_geometry(nearest_pt))[1])  # Angle between points on the line in radians
    
    x <- st_coordinates(pts[i])[,1]
    y <- st_coordinates(pts[i])[,2]
    
    ###### Define transects of 100m to 1000m in length 
    for (j in 1:10) {
      thetaT = theta+pi/2         # Get the angle
      dx_poi <- tlen[j]*cos(thetaT) # Coordinates of point of interest as defined by position length 
      dy_poi <- tlen[j]*sin(thetaT) 
    
      output1 <-
        # X & Y coordinate one side of line
        data.frame(
          x0 = x + dx_poi,
          y0 = y + dy_poi,
          x1 = x,
          y1 = y
        )
  
    mat1 <- as.data.frame(cbind( c( output1$x0[1], output1$x1[1] ) , c( output1$y0[1] , output1$y1[1] ) ))
    perp_points1 <- st_as_sf(mat1, coords = c("V1","V2"), crs = st_crs(road_tr))
   
     # Draw a line between the two points
    line1 <- st_geometry(st_cast(sf::st_union(perp_points1[1,], perp_points1[2,]), "LINESTRING"))
    
    perp_line[[i]][[j]] <- line1
    
    # Now repeat for other side of road
    output2 <-
      # X & Y coordinate one side of line
      data.frame(
        x0 = x - dx_poi,
        y0 = y - dy_poi,
        x1 = x,
        y1 = y
      )
    
    mat2 <- as.data.frame(cbind( c( output2$x0[1], output2$x1[1] ) , c( output2$y0[1] , output2$y1[1] ) ))
    perp_points2 <- st_as_sf(mat2, coords = c("V1","V2"), crs = st_crs(road_tr))
    
    line2 <- st_geometry(st_cast(sf::st_union(perp_points2[1,], perp_points1[2,]), "LINESTRING"))
    
    perp_line[[i]][[j+10]] <- line2
    
    }

}

plot(perp_line[[1]][[10]], add = T, col = "green")
plot(perp_line[[1]][[20]], add = T, col = "red")

# Flatten lists
lines_sf <- st_as_sf(do.call(c, unlist(perp_line, recursive=FALSE)))

# Add column to specify length of transect
lines_sf$length <- rep(tlen)

# Convert to WGS48
lines_sf_wgs <- st_transform(lines_sf, 4326)

# Add transect ID
lines_sf_wgs$transectID <- paste0(rep(1:length(pts), each = 20), rep(c("a","b"), each = 10))

# Export 
st_write(lines_sf_wgs, "transects.shp", append = F)

# Buffer line 
buffered_line <- st_buffer(road_tr, 1000, endCapStyle = "FLAT")

# Perform the split operation to seperate buffer into side of road
split_result <- st_split(buffered_line$geometry, st_cast(st_buffer(road_tr, 0.0001), "MULTILINESTRING")) %>%
  st_collection_extract("POLYGON")

# Convert result to sf object for viewing
split_result_sf <- st_sf(geometry = split_result)

# Add label
split_result_sf$side <- c("a","line","c")

# Plot the results
plot(split_result_sf)

# Remove the line/barrier
split_result_sf <- split_result_sf[split_result_sf$side != "line",]

# Convert back to crs
split_result_sf <- st_transform(split_result_sf, crs = st_crs(road))

# Export
setwd("./data/barrier_polygons")
st_write(split_result_sf, "buffered_barrier.shp", append = F)

