##################################################################
# CALCULATING OPTRAM, A MEASURE OF WATER TABLE DEPTH
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
library(tidyverse)
library(mgcv)
library(stringr)

# Clear data
rm(list = ls())

## Formula and methodology from https://www.sciencedirect.com/science/article/pii/S0034425723002870 

# Loading data from Google Earth engine
pa_name <- "somyne"
# STR
str_files <- list.files(pattern = paste0("STR-",pa_name), recursive = T) 
str_list <- lapply(str_files, "read.csv") 

# NDVI
ndvi_files <- list.files(pattern = paste0("NDVI-",pa_name), recursive = T)
ndvi_list <- lapply(ndvi_files, "read.csv") 

# Add file path (i.e. site ID) to dataframes
ndvi_list <- lapply(seq_along(ndvi_list), function(i) {
  df <- ndvi_list[[i]]               # Get each data frame
  df$dataset <- paste0(pa_name,str_extract_all(ndvi_files[i], "\\d+")[[1]])    # Add dataset as a column
  return(df)                        # Return the updated data frame
})

str_list <- lapply(seq_along(str_list), function(i) {
  df <- str_list[[i]]               
  df$dataset <- paste0(pa_name,str_extract_all(ndvi_files[i], "\\d+")[[1]])    
  return(df)                        
})

# Flatten list into dataframe 
ndvi_df <- Reduce(function(x, y) merge(x, y, all=T), ndvi_list)
str_df <- Reduce(function(x, y) merge(x, y, all=T), str_list)

# Rename columns to show dates. Some columns have full stops between year and month so remove these. 
colnames(str_df)[!colnames(str_df) %in% c("system.index","dataset",".geo","remapped")] <- substr(gsub("\\.","",colnames(str_df)[!colnames(str_df) %in% c("system.index","dataset",".geo","remapped")]),2,9)
colnames(ndvi_df)[!colnames(str_df) %in% c("system.index","dataset",".geo","remapped")] <- substr(gsub("\\.","",colnames(ndvi_df)[!colnames(str_df) %in% c("system.index","dataset",".geo","remapped")]),2,9)

# Convert to long format
str_long <- melt(setDT(str_df), id.vars = c("system.index","dataset",".geo","remapped"), variable.name = "date", value.name = "STR")
ndvi_long <- melt(setDT(ndvi_df), id.vars = c("system.index","dataset",".geo","remapped"), variable.name = "date", value.name = "NDVI")

# Remove NA 
str_long <- str_long[!is.na(str_long$STR)]
ndvi_long <- ndvi_long[!is.na(ndvi_long$NDVI)]

# Separate year and month
str_long$year <- as.numeric(substr(str_long$date,1,4))
str_long$month <- as.numeric(substr(str_long$date,5,6))
str_long$day <- as.numeric(substr(str_long$date,7,8))
str_long$date <- as.Date(str_long$date, format = "%Y%m%d")

# Combine data and make sure data are numeric
ind <- str_long
ind$STR <- as.numeric(ind$STR)
ind$NDVI <- as.numeric(ndvi_long$NDVI)

# Dry and wet edge paramaters. This is where you have storted them from GEE. 
params <- read.csv("./data/GEE_data/restoration_optram_params.csv")
params <- params %>% filter(grepl(pa_name, params$dataset))
params <- params %>% filter(ndvi.threshold == 0.9) ## Best threshold for NDVI for our study region
mrg <- merge(ind, params)
mrg$dry <- mrg$dry.y + (mrg$dry.slope*mrg$NDVI) 
mrg$wet <- mrg$wet.y + (mrg$wet.slope*mrg$NDVI)
	
# Calculate OPTRAM 
mrg$optram <- (mrg$STR - mrg$dry)/(mrg$wet - mrg$dry)

# Remove OPTRAM value below and above 1 
mrg <- mrg[mrg$optram <= 1 & mrg$optram >= 0,]

# Convert to spatial object and plot
sf <- cbind(st_as_sf(geojson_sf(mrg$.geo), st_crs = 4326),mrg)
# Add coordinates 
sf <- cbind(sf, st_coordinates(sf))

# Plot data
ggplot(mrg, aes(x = date, y = optram)) +
  geom_boxplot(mapping = aes(group = year), outlier.shape = NA) +
  geom_point(mapping = aes(col = optram), size = 1) +
  theme_classic() +
  ylim(0,1) +
  scale_colour_gradientn(colors = c('#d73027','#f46d43','#fdae61','#fee090','#abd9e9','#74add1','#4575b4','darkblue'), limits = c(0,1)) +
  xlab("Year") + ylab("OPTRAM") +
  labs(col = NULL, title = str_to_title(pa_name))
