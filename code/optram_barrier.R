##################################################################
# CALCULATING OPTRAM, A MEASURE OF SOIL MOISTURE
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
library(emmeans)

# Clear data
rm(list = ls())

## Formula and methodology from https://www.sciencedirect.com/science/article/pii/S0034425723002870 

# Loading data from Google Earth engine
# STR
str <- read.csv("./data/GEE_data/STR.csv")
# NDVI
ndvi <- read.csv("./data/GEE_data/NDVI.csv")
# Conver to long format
str_long <- melt(setDT(str), id.vars = c("system.index","lc","remapped","side",".geo"), variable.name = "month", value.name = "STR")
ndvi_long <- melt(setDT(ndvi), id.vars = c("system.index","lc","remapped","side",".geo"), variable.name = "month", value.name = "NDVI")

# Calculate OPTRAM using mean slop and intercept parameter 
ind <- merge(str_long, ndvi_long)

# Using weighted mean of parameters derived from subsets of the study area
ind$dry <- -0.4287096 + (3.329872*ind$NDVI) 
ind$wet <- -0.01501658 + (7.144965*ind$NDVI)

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
ind$month_num <- as.numeric(as.factor(ind$month))

# Extract coordiantes to correct for for spatial autocorrelation 
ind_sf <- cbind(st_as_sf(geojson_sf(ind$.geo), st_crs = 4326),ind)

# Extract coordiantes to correct for for spatial autocorrelation 
ind_sf <- cbind(ind_sf, st_coordinates(ind_sf))

# Set side of road as north or south
ind_sf$side <- as.factor(ifelse(ind_sf$side == "a", "south", "north"))

# Shapefile of Almany border
road <- st_read("./data/Almany-border.kml")

# Calculate distance to road 
ind_sf$dist <- as.numeric(st_distance(ind_sf, road))/1000

# Remove those very close to road
ind_sub <- ind_sf[ind_sf$dist > .02,]

# Convert month to numeric
ind_sub$month_num <- as.numeric(as.factor(ind_sub$month))

# Remove points with missing STR/NDVI
ind_sub <- ind_sub[!is.na(ind_sub$STR),]
ind_sub <- ind_sub[ind_sub$optram <= 1,] # Optram values should be between 0 and 1. Remove over-saturated pixels. 

# Distribution of data points. Not many data points for Feb. 
table(ind_sub$month_num, ind_sub$side, ind_sub$lc)

# Run model 
spatial_mod <- bam(
  optram ~
    dist*side + # Main effect of distance varying by land cover and side
    s(month_num, bs = "cc", k = 3) + # Main effect of month varying by land cover and side
    lc*side,                                # Main effect of land cover
  gamma = 1.4, select = T, discrete = T,
  data = ind_sub
)

## No interaction within month due to unbalanced dataset and similar effect of month across land cover types.
summary(spatial_mod)
AIC(spatial_mod)

emmeans(spatial_mod, list(pairwise ~ lc|side), adjust = "tukey")
test(emtrends(spatial_mod, ~ side, var = "dist"))

# Create a new data frame fodist()# Create a new data frame for predictions of effect of land cover and side
newdata <- expand.grid(
  dist = c(.02, .999),                       # Fixed values of dist
  lc = levels(ind_sub$lc),                  # All levels of lc
  side = levels(ind_sub$side),              # All levels of side
  month_num = unique(ind_sub$month_num),    # Include all unique values of month_num
  X = mean(ind_sub$X),                      # Control for X at its mean value
  Y = mean(ind_sub$Y)                       # Control for Y at its mean value
)

# Add predicted values to the new data frame
predictions <- predict(spatial_mod, newdata, se.fit = TRUE)
newdata$pred <- predictions$fit
newdata$se <- predictions$se.fit
# Calculate 95% confidence intervals
newdata$lower <- newdata$pred - 1.96 * newdata$se
newdata$upper <- newdata$pred + 1.96 * newdata$se

# Calculate average predictions for each combination of dist, land cover and side
avg_predictions <- aggregate(cbind(pred, lower, upper) ~ dist + lc + side, data = newdata, FUN = mean)

# Create group from distance and side of barrier so that in the plot it goes 1km south, .2km south, .2km north, 1km north
avg_predictions$group_fact <- factor(paste(avg_predictions$side, avg_predictions$dist), levels = c("south 0.999", "south 0.02", "north 0.02", "north 0.999"))

# Plot using ggplot2
ggplot(avg_predictions, aes(x = group_fact, y = pred, col = as.factor(lc))) +
  geom_point(position = position_dodge(0.5), size = .8) +
  scale_x_discrete(labels = NULL) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(0.5)) +
  labs(col = "") + xlab("") + ylab("Predicted OPTRAM
                                  ") + ## Higher values indicate higher water table
  theme_minimal() +
  scale_colour_manual(values = c('#336600','#33cc33','#ffb3ff','#cc6600','#808000','#F0E68C'), labels = c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised bog", "Fen/\ntransition mire", "Scrub")) 

# Compare map of index with and without barrier

# Colour palette for map
col <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')

newdata_p <- ind_sub[ind_sub$month_num == 3,] # Predict for August
newdata_p$pred <- as.vector(predict(spatial_mod, newdata = newdata_p))
newdata_p$side <- "south"
newdata_p$dist <- 1
newdata_p$pred_no_barrier <- as.vector(predict(spatial_mod, newdata = newdata_p))

p1 <- ggplot() +
  geom_sf(data = newdata_p, mapping = aes(col = pred), size = .05) +
  geom_sf(data = road, linetype = "dashed") + 
  theme_map() +
  labs(col = "OPTRAM") +
  theme(legend.position = "bottom") +
  scale_colour_gradientn(colours = col) +
  ggtitle("(b) With barrier effect") 

p2 <- ggplot() +
  geom_sf(data = newdata_p, mapping = aes(col = pred_no_barrier), size = .05) +
  geom_sf(data = road, linetype = "dashed") + 
  theme_map() +
  scale_colour_gradientn(colours = col) +
  guides(col = "none") +
  ggtitle("(a) Without barrier effect") 

grid.arrange(p2, p1, ncol = 1)

# Look at effect of distance
# Create a new data frame for predictions of effect of distance
newdata2 <- expand.grid(
  dist = seq(.2, .999, length.out = 100),                      # Fixed values of dist
  lc = levels(ind_sub$lc),                  # All levels of lc
  side = levels(ind_sub$side),              # All levels of side
  month_num = unique(ind_sub$month_num),    # Include all unique values of month_num
  X = mean(ind_sub$X),                      # Control for X at its mean value
  Y = mean(ind_sub$Y)                       # Control for Y at its mean value
)

# Add predicted values to the new data frame
predictions2 <- predict(spatial_mod, newdata2, se.fit = TRUE)
newdata2$pred <- predictions2$fit
newdata2$se <- predictions2$se.fit
# Calculate 95% confidence intervals
newdata2$lower <- newdata2$pred - 1.96 * newdata2$se
newdata2$upper <- newdata2$pred + 1.96 * newdata2$se

# Calculate average predictions for each combination of dist, land cover and side
avg_predictions2 <- aggregate(cbind(pred, lower, upper) ~ dist + lc + side, data = newdata2, FUN = mean)

# Facet labels
facet_labels2 <- c("south" = "South/West", "north" = "North/East")

# Plot using ggplot2
ggplot(avg_predictions2, aes(x = dist, y = pred, col = lc, fill = lc)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .4, col = NA) +
  facet_wrap(vars(side,lc)) + # Separate plots for each value of dist +
  scale_colour_manual(values = c('#336600','#33cc33','#ffb3ff','#cc6600','#808000','#F0E68C'), labels = c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised bog", "Fen/\ntransition mire", "Scrub")) +
  scale_fill_manual(values = c('#336600','#33cc33','#ffb3ff','#cc6600','#808000','#F0E68C'), labels = c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised bog", "Fen/\ntransition mire", "Scrub")) +
  labs(col = "", fill = "") + xlab("Distance from barrier (km)") + ylab("Predicted OPTRAM") +
  theme_minimal() ## 600x700

