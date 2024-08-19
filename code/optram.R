##################################################################
# CALCULATING OPTRAM, A MEASURE OF SOIL MOISTURE
# M. Kirkland
# 19/08/24
##################################################################

## Formula and methodology from https://www.sciencedirect.com/science/article/pii/S0034425723002870 

# Loading data from Google Earth engine
# STR from wet and dry edges
files_dry <- list.files(pattern="*dry*")
files_wet <- list.files(pattern="*wet*")
list_dry <- lapply(files_dry, "read.csv")
list_wet <- lapply(files_wet, "read.csv")
dry_edge <-  do.call("rbind", list_dry)
wet_edge <-  do.call("rbind", list_wet)
colnames(dry_edge)[3] <- "dry"
colnames(wet_edge)[3] <- "wet"
# STR
str <- read.csv("STR.csv")
colnames(str)[2] <- "STR"
# Create ID variable for grid cells for merging since methods in GEE changed slightly 
str$system.index.short <- sub("\\_.*","",str$system.index)
wet_edge$system.index.short <- sub("\\_.*","",wet_edge$system.index)
# Merge datasets
optram <- merge(str, subset(wet_edge, select = -c(.geo, system.index)), by = c("system.index.short", "month", "lc", "side"), all = F)

# Calculate OPTRAM 
optram <- (optram$STR - optram$dry)/(optram$wet - optram$dry)