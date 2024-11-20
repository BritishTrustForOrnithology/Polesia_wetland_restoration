##################################################################
# PLOTTING NDVI~STR SPACE
# M. Kirkland
# 18/11/2024
##################################################################

library(dplyr)
library(ggplot2)
library(stringr)

# Loading data from Google Earth engine
pa_name <- "somyne"

# Indices
str <- read.csv(paste0("./data/GEE_data/",pa_name,"-data/STR-space.csv"))
ndvi <- read.csv(paste0("./data/GEE_data/",pa_name,"-data/NDVI-space.csv"))

# Edge parameters using different NDVI thresholds
params <- read.csv("./data/GEE_data/restoration_optram_params.csv")
params_pa <- params %>% filter(str_detect(dataset, pa_name)) 
# Because we split the study area up, calculate mean for visualisation
params_mean <- params %>% group_by(ndvi.threshold) %>% select(-dataset) %>% summarise_all(mean)

# Combine indices into single dataframe
space <- inner_join(str, ndvi)

# Plot STR-NDVI space with edge slopes
ggplot(space, aes(x = NDVI, y = STR)) +
  geom_point(alpha = .1) +
  ## Wet slope
  geom_abline(params_mean, mapping = aes(slope = wet.slope, intercept = wet.y, col = as.factor(ndvi.threshold)), linewidth = 1) +
  ## Dry slope
  geom_abline(params_mean, mapping = aes(slope = dry.slope, intercept = dry.y, col = as.factor(ndvi.threshold)), linewidth = 1) +
  theme_bw() +
  labs(col = "NDVI threshold")
			
