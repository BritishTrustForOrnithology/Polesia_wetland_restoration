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
ind_sub <- ind_sub[ind_sub$optram <= 1,] # Optram values should be between 0 and 1, not convinced about removing these, but its November and January anyway when there aren't many data

# Date exploration
month_labels <- c("June", "July", "Aug", "Sept", "Oct", "Nov", "Dec",
                  "Jan", "Feb", "March", "April", "May")
month_labeller <- as_labeller(setNames(month_labels, 1:12))

ggplot(ind_sub, aes(x = side, y = optram, col = lc)) +
  geom_boxplot() + ## Overall, looks higher on the north side? 
  xlab("") + ylab("OPTRAM") +
  scale_colour_manual(values = c('#336600','#33cc33','#ffb3ff','#cc6600','#808000','#F0E68C'), labels = c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised bog", "Fen/\ntransition mire", "Scrub")) +
  labs(col = NULL) +
  facet_wrap(vars(month_num), labeller = labeller(month_num = month_labeller)) 

# Distribution of data points. Remove June, November, January? 
table(ind_sub$month_num, ind_sub$side, ind_sub$lc)

# Run model 
spatial_mod <- bam(
  optram ~
    s(dist, by = interaction(lc, side), k = 3) + # Main effect of distance varying by land cover and side
    s(month_num, bs = "cc", by = side, k = 9) + # Main effect of month varying by land cover and side
    s(month_num, bs = "cc", by = lc, k = 9) +
    ti(dist, month_num, bs = c("tp", "cc"), by = lc) + # Interaction between distance and month, varying by land cover and side
    ti(dist, month_num, bs = c("tp", "cc"), by = side) +  
    s(X, Y, bs = "ds", k = 99) +          # Spatial smooth term
    lc +                                  # Main effect of land cover
    side,                                # Main effect of land cover
  gamma = 1.4, select = T, discrete = T,
  data = ind_sub
)

## No interaction within month due to unbalanced dataset and similar effect of month across land cover types.
summary(spatial_mod)
AIC(spatial_mod)

# Create a new data frame for predictions of effect of land cover and side
newdata <- expand.grid(
  dist = c(.2, .999),                       # Fixed values of dist
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
# avg_predictions <- aggregate(cbind(pred, lower, upper) ~ dist + lc + side, data = newdata, FUN = mean)

# Define custom labels for lc and month_num
lc_labels <- c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised bog", 
               "Fen/\ntransition mire", "Scrub")

# Map these labels to the corresponding values in your data
lc_labeller <- as_labeller(setNames(lc_labels, unique(ind_sub$lc)))

# Plot using ggplot2
ggplot(newdata, aes(x = as.factor(month_num), y = pred, col = interaction(as.factor(dist), side))) +
  geom_point(position = position_dodge(0.5), size = .8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(0.5)) +
  facet_grid(rows = vars(lc),  labeller = labeller(lc = lc_labeller)) + # Separate plots for each value of dist
  labs(col = "") + xlab("") + ylab("Predicted OPTRAM
                                  ") + ## Higher values indicate higher water table
  scale_colour_manual(values = c("#78c679","#238443","#a6bddb","#0570b0"), labels = c("North/West:\n200m", "1km", "South/East:\n200m", "1km")) +
  theme_minimal() +
  scale_x_discrete(labels =  c("June", "July", "Aug", "Sept", "Oct", "Feb", "March", "April", "May"))
## Export at 600x400

## No interaction within month due to unbalanced dataset and similar effect of month across land cover types.
summary(spatial_mod)
AIC(spatial_mod)

# Create a new data frame for predictions of effect of land cover and side
newdata <- expand.grid(
  dist = .5,                       # Fixed values of dist
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

## Calculate average predictions for each combination of dist, land cover and side
avg_predictions <- aggregate(cbind(pred, lower, upper) ~ lc + side, data = newdata, FUN = mean)

# Define custom labels for lc and month_num
lc_labels <- c("Coniferous\nforest", "Deciduous\nforest", "Meadow", "Raised bog", 
               "Fen/\ntransition mire", "Scrub")

# Map these labels to the corresponding values in your data
lc_labeller <- as_labeller(setNames(lc_labels, unique(ind_sub$lc)))

# Plot using ggplot2
ggplot(avg_predictions, aes(x = lc, y = pred, col = side)) +
     geom_point(position = position_dodge(0.5), size = .8) +
     geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(0.5)) +
     labs(col = "") + xlab("") + ylab("Predicted OPTRAM
                                  ") + ## Higher values indicate higher water table
     theme_minimal()
     scale_x_discrete(labels =  c("June", "July", "Aug", "Sept", "Oct", "Nov", "Jan", "Feb", "March", "April", "May"))
## Export at 600x400

# Compare map of index with and without barrier
# Predict to orginal dataset
newdata_p <- ind_sub[ind_sub$month_num == 2,] # Predict for July when differences are most clear and for which we have more data (not much data for June)
newdata_p$pred <- as.vector(predict(spatial_mod, newdata = newdata_p))
newdata_p$side <- "south"
newdata_p$dist <- 1
newdata_p$pred_no_barrier <- as.vector(predict(spatial_mod, newdata = newdata_p))

# Colour palette for map
col <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')

# Plot this
p1 <- ggplot() +
  geom_sf(data = newdata_p, mapping = aes(col = pred), size = .05) +
  geom_sf(data = road, linetype = "dashed") + 
  theme_map() +
  scale_colour_gradientn(colours = col) +
  guides(col = "none") +
  ggtitle("(b)") +
  theme(legend.position = c(.8,.5)) ## 650x400

p2 <- ggplot() +
  geom_sf(data = newdata_p, mapping = aes(col = pred_no_barrier), size = .05) +
  geom_sf(data = road, linetype = "dashed") + 
  theme_map() +
  scale_colour_gradientn(colours = col, labels = NULL) +
  labs(col = "") + 
  ggtitle("(a)") +
  theme(legend.position = c(.8,.5)) ## 650x400

# Now predict for winter month
newdata_m <- ind_sub[ind_sub$month_num == 10,] # Predict for July when differences are most clear and for which we have more data (not much data for June)
newdata_m$pred <- as.vector(predict(spatial_mod, newdata = newdata_m))
newdata_m$side <- "south"
newdata_m$dist <- 1
newdata_m$pred_no_barrier <- as.vector(predict(spatial_mod, newdata = newdata_m))

# Plot this
p3 <- ggplot() +
  geom_sf(data = newdata_m, mapping = aes(col = pred), size = .05) +
  geom_sf(data = road, linetype = "dashed") + 
  theme_map() +
  guides(col = "none") +
  scale_colour_gradientn(colours = col) +
  ggtitle("(d)") 

p4 <- ggplot() +
  geom_sf(data = newdata_m, mapping = aes(col = pred_no_barrier), size = .05) +
  geom_sf(data = road, linetype = "dashed") + 
  theme_map() +
  scale_colour_gradientn(colours = col) +
  guides(col = "none") +
  ggtitle("(c)") 

grid.arrange(p2, p1, p4, p3, ncol = 2)

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

