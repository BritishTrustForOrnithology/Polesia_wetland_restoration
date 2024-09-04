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
# STR
str <- read.csv("./data/GEE_data/STR-Cheremske.csv")
# NDVI
ndvi <- read.csv("./data/GEE_data/NDVI-Cheremske.csv")

# Rename columns to show months
colnames(str)[-c(1,(ncol(str)-2):ncol(str))] <- substr(colnames(str)[-c(1,(ncol(str)-2):ncol(str))],6,7)
colnames(ndvi)[-c(1,(ncol(ndvi)-2):ncol(ndvi))] <- substr(colnames(ndvi)[-c(1,(ncol(ndvi)-2):ncol(ndvi))],6,7)

# Convert to long format
str_long <- melt(setDT(str), id.vars = c("system.index","lc","remapped",".geo"), variable.name = "month", value.name = "STR")
ndvi_long <- melt(setDT(ndvi), id.vars = c("system.index","lc","remapped",".geo"), variable.name = "month", value.name = "NDVI")

# Remove NA 
str_long <- str_long[!is.na(str_long$STR)]
ndvi_long <- ndvi_long[!is.na(ndvi_long$NDVI)]

# Calculate OPTRAM using mean slop and intercept parameter 
ind <- str_long
ind$NDVI <- ndvi_long$NDVI

# Using weighted mean of parameters derived from subsets of the study area
ind$dry <- -0.2019163477065955 + 2.7118313659398905*ind$NDVI
ind$wet <- 2.5645077855283933	+ 6.854940135998448*ind$NDVI

# Calculate OPTRAM 
ind$optram <- (ind$STR - ind$dry)/(ind$wet - ind$dry)

# Convert land cover to factor
ind$lc <- as.factor(ind$lc)

# Convert month to numeric
ind$month_num <- as.numeric(as.factor(ind$month))

# Remove points with missing STR/NDVI
ind_sub <- ind[!is.na(ind$STR),]

# Extract coordinates and convert to sf object 
ind_sf <- cbind(st_as_sf(geojson_sf(ind_sub$.geo), st_crs = 4326),ind_sub)
ind_sf <- cbind(ind_sf, st_coordinates(ind_sf))

# Map optram
col <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf',
         '#abd9e9','#74add1','#4575b4','#313695') ## Colour palette

ggplot() +
  geom_sf(data = ind_sf %>% arrange(optram, decreasing = T), mapping = aes(col = optram), size = .05) +
  theme_map() +
  scale_colour_gradientn(colours = col) +
  theme(legend.position = c(.8,.5)) 

table(ind_sub$lc[ind_sub$optram > 1]) ## Mostly peatlands and deciduous forest
table(ind_sub$month[ind_sub$optram > 1]) ## Mostly summer

# Remove over-saturated pixels above the wet edge 
ind_sub <- ind_sub[ind_sub$optram <= 1,]

# Distribution of data points. Predominantly fen and transition mire. 
table(ind_sub$month_num,ind_sub$lc) ## Little data for Feb

ggplot(ind_sub %>% filter(lc == 6), aes(x = as.factor(month_num), y = optram)) +
  geom_violin(bw = .02, width = 1) + 
  geom_point(ind_sub %>% filter(lc == 6) %>%  group_by(month_num) %>% summarise(mean = mean(optram)), mapping = aes(as.factor(month_num), y = mean), size = 2, shape = 24, fill = "blue") +
  xlab("") + ylab("OPTRAM") +
  theme_classic() +
  scale_x_discrete(labels = c("June", "July", "Aug", "Sept", "Oct", "Nov", 
                   "Jan", "Feb", "March", "April", "May"))

# Produce animation  
ind_sf <- cbind(st_as_sf(geojson_sf(ind_sub$.geo), st_crs = 4326),ind_sub)
ind_sf <- cbind(ind_sf, st_coordinates(ind_sf))

anim <- ggplot() +
  geom_sf(data = ind_sf %>% group_by(system.index, month_num) %>% summarise(optram = mean(optram)), mapping = aes(col = optram, group = system.index), size = .2) +
  theme_map() +
  labs(col = "OPTRAM") +
  scale_colour_gradientn(colours = col, limits = c(0,1)) +
  theme(legend.position = c(.01,.5)) +
  ggtitle(month_num) +
  transition_time(month_num) 
animate(anim, width = 5, height = 8, units = "in", res = 300, fps = 100)
anim_save(paste0(getwd(),"/results/animation.gif"))
