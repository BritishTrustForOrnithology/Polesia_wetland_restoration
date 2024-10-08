##################################################################
# CALCULATING OPTRAM, A MEASURE OF SOIL MOISTURE
# M. Kirkland
# 19/08/24
##################################################################

# Libraries
library(sf)
library(geojsonsf)
library(ggplot2)
library(data.table)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(gganimate)

# Clear data
rm(list = ls())

## Formula and methodology from https://www.sciencedirect.com/science/article/pii/S0034425723002870 

# Loading data from Google Earth engine
# STR
str <- read.csv("./data/GEE_data/Somyne_data/STR-Somyne-2023.csv")
# NDVI
ndvi <- read.csv("./data/GEE_data/Somyne_data/NDVI-Somyne-2023.csv")
# Conver to long format
str_long <- melt(setDT(str), id.vars = c("system.index","lc","remapped",".geo"), variable.name = "id", value.name = "STR")
ndvi_long <- melt(setDT(ndvi), id.vars = c("system.index","lc","remapped",".geo"), variable.name = "id", value.name = "NDVI")

# Calculate OPTRAM using mean slop and intercept parameter 
ind <- cbind(str_long, ndvi_long)

# Using weighted mean of parameters derived from subsets of the study area
ind$dry <- -0.304605683 + (3.931136813*ind$NDVI) 
ind$wet <- 5.227581293 + (2.051394241*ind$NDVI)

# Edge parameters for each polygon 
# dry <- read.csv("./data/GEE_data/edge_params_dry.csv")
# wet <- read.csv("./data/GEE_data/edge_params_wet.csv")
# Crosswalk matching data from GEE output to polygon
# cw <- read.csv("./data/GEE_data/poly_system_ind.csv")
# Merge them all 
# merged_params <- Reduce(function(x, y) merge(x, y, by = "polygon"), list(cw, wet, dry))

# Calculate OPTRAM 
ind$optram <- (ind$STR - ind$dry)/(ind$wet - ind$dry)

# Convert land cover to factor
ind$lc <- as.factor(ind$lc)

# Convert month to numeric
ind$month_num <- as.numeric(substr(ind$month, 6, 7))

# Extract coordiantes to correct for for spatial autocorrelation 
ind_sf <- cbind(st_as_sf(geojson_sf(ind$.geo), st_crs = 4326),ind)

# Extract coordiantes to correct for for spatial autocorrelation 
ind_sf <- cbind(ind_sf, st_coordinates(ind_sf))

# Shapefile of Almany border
canals <- st_read("./data/GIS_data-20240927T074221Z-001/GIS_data/hydro/kanal.shp")

# Convert necessary columns to data.table for faster aggregation
ind_dt <- as.data.table(ind_sf[, c("system.index", "month_num", "optram", "geometry")])

# Perform aggregation with faster geometry handling
monthly_dt <- ind_dt[, .(
  optram = mean(optram, na.rm = TRUE),
  geometry = geometry[1]  # Take the first geometry (avoid st_union)
), by = .(system.index, month_num)]


# Convert back to sf at the end
monthly_mean <- st_as_sf(monthly_dt, crs = 4326)

# Boundary of PA
pa <- st_read("./data/GIS_data-20240927T074221Z-001/GIS_data/somyne/mega_polygon.shp")

col <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf',
         '#abd9e9','#74add1','#4575b4','#313695') ## Colour palette

anim <- ggplot() +
  geom_sf(data = pa, fill = NA) +
  geom_sf(data = monthly_mean, mapping = aes(col = optram, group = system.index), size = .2) +
  theme_map() +
  labs(col = "OPTRAM") +
  scale_colour_gradientn(colours = col, limits = c(0,1)) +
  theme(legend.position = c(.01,.5)) + 
  transition_states(year_month) +
  labs(caption = "{closest_state}") 
animate(anim, width = 5, height = 4, units = "in", res = 300, fps = 5)
anim_save(paste0(getwd(),"/results/animation-Somyne.gif"))
