## ISRaD v2 data analysis ##
## Calculate area of each climate zone ##
## Sophie von Fromm ##
## August 08, 2025 ##

library(terra)
library(tidyverse)

# Load climate zones 
  # climate zone data needs to be downloaded from Beck et al. 2023; 1991-2020
  # https://www.nature.com/articles/s41597-023-02549-6
kg_zones <- rast("./data/koppen_geiger_0p00833333.tif") 

# plot(kg_zones)
# kg_zones

# Check the coordinate reference system
crs(kg_zones, describe = TRUE)

# Remove Antartica (SOC stocks are also w/o Antartica)
kg_zones_no_antarctica <- crop(kg_zones, ext(-180, 180, -60, 90))

plot(kg_zones_no_antarctica)

# Calculate cell sizes 
zone_areas <- expanse(kg_zones_no_antarctica, unit = "km", byValue = TRUE)

zone_areas_df <- zone_areas %>% 
  rename(pro_KG_present = value,
         area_km2 = area) %>% 
  #remove 0 which is not part of climate zones
  filter(pro_KG_present != 0) %>% 
  dplyr::select(-layer) %>% 
  tibble()

zone_areas_df

# check area
zone_areas_df %>% 
  summarise(sum_area = sum(area_km2))

# See the largest climate zones
zone_areas_df[order(-zone_areas_df$area_km2), ]

# Calculate what percentage each zone represents
zone_areas_df$percent_area <- (zone_areas_df$area_km2 / sum(zone_areas_df$area_km2)) * 100

zone_areas_df

# Load KG legend
kg_legend <- read.csv("./data/KG_present_legend.csv")

# Merge legend with area estimates
kg_area <- kg_legend %>% 
  left_join(zone_areas_df)

head(kg_area)

# Save data frame
write.csv(kg_area, "./data/KG_present_area.csv")
