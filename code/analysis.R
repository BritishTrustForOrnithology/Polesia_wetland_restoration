##################################################################
# ASSESSING WETNESS EITHER SIDE OF BARRIES IN POLESIA
# M. Kirkland
# 12/07/24
##################################################################

# Clear data
rm(list = ls())

# Libraries
library(sf) 
library(dplyr)
library(terra)
library(data.table)
library(geojsonsf)
library(nlme)
library(stringr)
library(ggplot2)
library(ggthemes)
library(mgcv)

# Obtained from GEE
setwd("./data/GEE_data")
ind <- read.csv("MSI-results.csv")[,-1] # Ignore first column

# Convert coordinates to sf object
ind_sf <- cbind(st_as_sf(geojson_sf(ind$.geo), st_crs = 4326),ind)

# Convert land cover to factor
ind_sf$lc <- as.factor(ind_sf$lc)

# Extract coordiantes to correct for for spatial autocorrelation 
ind_sf <- cbind(ind_sf, st_coordinates(ind_sf))

# Set side of road as north or south
ind_sf$side <- as.factor(ifelse(ind_sf$side == "a", "south", "north"))

# Shapefile of Almany border
setwd("~/BTO projects/Polesia project/wetland-restoration")
road <- st_read("Almany-border.kml")

# Calculate distance to road 
ind_sf$dist <- as.numeric(st_distance(ind_sf, road))/1000

# Remove those very close to road
ind_sub <- ind_sf[ind_sf$dist > .02,]

# Convert month to numeric
ind_sub$month_num <- as.numeric(as.factor(ind_sub$month))

# Model differences in index either side of barrier account for spatial autocorrelation 
spatial_mod <- gam(
  index ~
    s(dist, by = interaction(lc, side), k = 5) + # Main effect of distance varying by land cover and side
    s(month_num, bs = "cc", by = side, k = 11) + # Main effect of month varying by land cover and side
    s(month_num, bs = "cc", by = lc, k = 11) +
    ti(dist, month_num, bs = c("tp", "cc"), by = lc) + # Interaction between distance and month, varying by land cover and side
    ti(dist, month_num, bs = c("tp", "cc"), by = side) +  
    s(X, Y, bs = "ds", k = 99) +          # Spatial smooth term
    lc +                                  # Main effect of land cover
    side,                                 # Main effect of side
  data = ind_sub
)

# Explore results
summary(spatial_mod)
plot(spatial_mod)
gam.check(spatial_mod)

# Plot results ####

# Create a new data frame for predictions of effect of land cover and side
newdata <- expand.grid(
  dist = c(.1, .999),                       # Fixed values of dist
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

# Define a custom labeller for the facets
facet_labels <- c("0.1" = "100m", "0.999" = "1km")

# Plot using ggplot2
ggplot(avg_predictions, aes(x = lc, y = pred, col = side)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.5)) +
  facet_wrap(~ dist, labeller = as_labeller(facet_labels), nrow = 2) + # Separate plots for each value of dist
  labs(col = "Side of Almany barrier") + xlab("") + ylab("Predicted MSI") +
  theme_minimal() +
  scale_colour_manual(values = c("#d95f02", "#7570b3"), labels = c("North/East", "South/West")) +
  scale_x_discrete(labels = c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised\nbog", "Fen/\ntransition mire", "Scrub"))
## Export at 600x600

# Map index
# Create new dataframe this time with geometry
newdata_p <- ind_sub %>% 
  mutate(X = mean(ind_sub$X),                     
         Y = mean(ind_sub$Y))
# Predict MSI controlling for month and spatial autocorrelation 
newdata_p <- newdata_p %>% 
  mutate(pred = predict(spatial_mod, newdata = newdata_p)) %>% 
  arrange(desc(pred)) %>% 
  group_by(geometry) %>% summarise(mean = mean(pred))

col_water <- rev(c('#d73027','#f46d43','#fee090','#ffffbf','#abd9e9','#74add1','#4575b4','#253494'))
col_NDVI <- c('#8e0152','#c51b7d','#de77ae','#f1b6da','#b8e186','#7fbc41','#4d9221','#276419')

# Plot this
ggplot() +
  geom_sf(data = newdata_p, mapping = aes(col = mean), size = .05) +
  geom_sf(data = road) + 
  theme_map() +
  scale_colour_gradientn(colours = col_water) +
  labs(col = "Predicted MSI") +
  theme(legend.position = c(.8,.5)) ## 650x400

# Create a new data frame for predictions of effect of distance
newdata2 <- expand.grid(
  dist = seq(.1, .999, length.out = 100),                      # Fixed values of dist
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
  facet_wrap(~ side, nrow = 2, labeller = as_labeller(facet_labels2)) + # Separate plots for each value of dist +
  scale_colour_manual(values = c('#336600','#33cc33','#ffb3ff','#cc6600','#808000','#F0E68C'), labels = c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised bog", "Fen/\ntransition mire", "Scrub")) +
  scale_fill_manual(values = c('#336600','#33cc33','#ffb3ff','#cc6600','#808000','#F0E68C'), labels = c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised bog", "Fen/\ntransition mire", "Scrub")) +
  labs(col = "", fill = "") + xlab("Distance from barrier (km)") + ylab("Predicted MSI") +
  theme_minimal() ## 600x700

