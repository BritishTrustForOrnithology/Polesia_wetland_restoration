##################################################################
# CALCULATING OPTRAM IN CHEREMSKE BOG
# M. Kirkland
# 19/08/24
##################################################################

# Libraries
library(sf)
library(geojsonsf)
library(mgcv)
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
pa_name <- "Bile"
# STR
str_files <- list.files(pattern = paste0("STR-",pa_name), recursive = T) 
str_list <- lapply(str_files, "read.csv") 
str_df <- Reduce(function(x, y) merge(x, y, all=T), str_list)
                 
# NDVI
ndvi_files <- list.files(pattern = paste0("NDVI-",pa_name), recursive = T)
ndvi_list <- lapply(ndvi_files, "read.csv") 
ndvi_df <- Reduce(function(x, y) merge(x, y, all=T), ndvi_list)

# Rename columns to show months. Some columns have full stops between year and month so remove these. 
colnames(str_df)[-(1:4)] <- substr(gsub("\\.","",colnames(str_df)[-(1:4)]),2,7)
colnames(ndvi_df)[-(1:4)] <- substr(gsub("\\.","",colnames(ndvi_df)[-(1:4)]),2,7)

# Convert to long format
str_long <- melt(setDT(str_df), id.vars = c("system.index","lc","remapped",".geo"), variable.name = "year_month", value.name = "STR")
ndvi_long <- melt(setDT(ndvi_df), id.vars = c("system.index","lc","remapped",".geo"), variable.name = "year_month", value.name = "NDVI")

# Remove NA 
str_long <- str_long[!is.na(str_long$STR)]
ndvi_long <- ndvi_long[!is.na(ndvi_long$NDVI)]

# Separate year and month
str_long$year <- as.numeric(substr(str_long$year_month,1,4))
str_long$month <- as.numeric(substr(str_long$year_month,5,6))
ndvi_long$year <- as.numeric(substr(ndvi_long$year_month,1,4))
ndvi_long$month <- as.numeric(substr(ndvi_long$year_month,5,6))

# Remove December to Feb
str_long <- str_long[str_long$month != 12 & str_long$month != 1 & str_long$month != 2,]
ndvi_long <- ndvi_long[ndvi_long$month != 12 & ndvi_long$month != 1 & ndvi_long$month != 2,]

# Combine data and make sure data are numeric
ind <- str_long
ind$STR <- as.numeric(ind$STR)
ind$NDVI <- as.numeric(ndvi_long$NDVI)

# Edge parameters for each year 
params <- read.csv("./data/GEE_data/restoration_optram_params.csv")
# Select for specific restoration area
params <- params[params$restoration.area == pa_name,]
# Convert to wide format 
params_wide <- dcast(setDT(params), year ~ edge, value.var = c("y", "slope"))
# Merge with NDVI and STR data
ind <- merge(ind, params_wide)

# Calculate edges
# ind$dry <- ind$y_dry + ind$slope_dry*ind$NDVI
# ind$wet <- ind$y_wet + ind$slope_wet*ind$NDVI
# Average across years
ind$dry <- mean(params$y[params$edge == "dry"]) + mean(params$slope[params$edge == "dry"])*ind$NDVI
ind$wet <- mean(params$y[params$edge == "wet"]) + mean(params$slope[params$edge == "wet"])*ind$NDVI

# Calculate OPTRAM 
ind$optram <- (ind$STR - ind$dry)/(ind$wet - ind$dry)

# Convert land cover to factor
ind$lc <- as.factor(ind$lc)

# Remove points with missing STR/NDVI
ind_sub <- ind[!is.na(ind$STR),]

# Explore OPTRAM values
hist(ind_sub$optram)
table(ind_sub$lc[ind_sub$optram > 1])
table(ind_sub$month[ind_sub$optram > 1])

# Remove over-saturated pixels above the wet edge 
ind_sub <- ind_sub[ind_sub$optram <= 1,]

# Distribution of data points. 
table(ind_sub$month,ind_sub$lc) 

# Focus on peatlands
ind_sub <- ind_sub[ind_sub$lc == 6 | ind_sub$lc == 7,]

# Combine geometry and attributes in one step
ind_sf <- st_as_sf(cbind(ind_sub, st_as_sf(geojson_sf(ind_sub$.geo), crs = 4326)))class(ind_sf)

# Extract coordinates to correct for for spatial autocorrelation 
ind_sf <- cbind(ind_sf, st_coordinates(ind_sf))

# Run model exploring OPTRAM over time
spatial_mod <- bam(
  optram ~
    s(year, k = 3) +
    s(month, bs = "cc", k = 3) + 
    ti(year, month, bs = c("tp", "cc"), k = c(3,3)) +
    s(X, Y, bs = "ds", k = 99),    
  gamma = 1.4, select = T, discrete = T,
  data = ind_sf
)

# Explore results 
summary(spatial_mod)
plot(spatial_mod)

# Plot interaction between year and month
# Create a new data frame for predictions
newdata <- expand.grid(
  month = unique(ind_sf$month),    # Include all unique values of month
  year = unique(ind_sf$year),    # Include all unique values of month
  X = mean(ind_sf$X),                      # Control for X at its mean value
  Y = mean(ind_sf$Y)                       # Control for Y at its mean value
)

# Add predicted values to the new data frame
predictions <- predict(spatial_mod, newdata, se.fit = TRUE)
newdata$pred <- predictions$fit
newdata$se <- predictions$se.fit
# Calculate 95% confidence intervals
newdata$lower <- newdata$pred - 1.96 * newdata$se
newdata$upper <- newdata$pred + 1.96 * newdata$se

# Plot using ggplot2
ggplot(newdata, aes(x = year, y = pred, col = as.factor(month), fill = as.factor(month), group = month)) +
  geom_line() +
  scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6'), labels = c("March","April","May","June","July","August","September","October","November")) +
  scale_colour_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6'), labels = c("March","April","May","June","July","August","September","October","November")) +
  geom_ribbon(aes(ymin = lower, ymax = upper), col = NA, alpha = .5) +
  labs(col = "", fill = "") + xlab("") + ylab("Predicted OPTRAM
                                  ") + 
  theme_minimal() 

# Produce animation ####

# Create 'year_month' as a factor for mapping
ind_sf$year_month <- as.factor(paste(ind_sf$year, ind_sf$month, sep = "-"))

# Convert necessary columns to data.table for faster aggregation
ind_dt <- as.data.table(ind_sf[, c("system.index", "year_month", "optram", "geometry")])

# Perform aggregation with faster geometry handling
monthly_dt <- ind_dt[, .(
  optram = mean(optram, na.rm = TRUE),
  geometry = geometry[1]  # Take the first geometry (avoid st_union)
), by = .(system.index, year_month)]

# Ensure 'year_month' stays a factor (if it was altered during aggregation)
monthly_dt$year_month <- factor(monthly_dt$year_month, levels = levels(ind_sf$year_month))

# Convert back to sf at the end
monthly_mean <- st_as_sf(monthly_dt, crs = 4326)

# Boundary of PA
bound <- st_read("./data/elsppolesiashapes/Study_sites_merged.shp")
pa <- bound[bound$Site_name == "Cheremske Bog",]

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
anim_save(paste0(getwd(),"/results/animation.gif"))

