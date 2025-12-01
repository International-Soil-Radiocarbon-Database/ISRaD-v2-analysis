## ISRaD v2 data analysis ##
## Global SOC distribution by climate zones ##
## Sophie von Fromm, Nate Looker ##
## May 05, 2025 ##

### Downloading SOC stocks from SoilGrids and estimate SOC stocks (0-200cm) for each climate zone
# https://drive.google.com/drive/folders/1MU5VOdGtllu6onQ5BeZH9IkyIWLvdy4S

library(terra)

# ocs_1 <- rast("./data/soilgrids_ocs_0_200-0000000000-0000000000.tif")
# ocs_2 <- rast("./data/soilgrids_ocs_0_200-0000000000-0000032768.tif")
# 
# plot(ocs_1)
# plot(ocs_2)
# 
# # Merge the two raster files into one
# ocs_global <- merge(ocs_1, ocs_2)
# 
# plot(ocs_global)
# 
# # Save to file
# writeRaster(ocs_global, "./data/soilgrids_ocs_0_200_global.tif")

# Load global soilgrids ocs raster (kg/m2)
ocs_global <- rast("./data/soilgrids_ocs_0_200_global.tif")

ocs_global
plot(ocs_global)
summary(ocs_global)

# Load climate zones (Beck et al. 2023, v2)
# https://figshare.com/articles/dataset/Present_and_future_K_ppen-Geiger_climate_classification_maps_at_1-km_resolution/6396959/2?file=12407516
kg_zones <- rast("./data/koppen_geiger_0p00833333.tif")

plot(kg_zones)
kg_zones
summary(kg_zones)

legend_kg <- read.csv("./data/KG_present_legend.csv")

# Check compatibility between ocs and climate zones
if(!compareGeom(ocs_global, kg_zones, stopOnError = FALSE)) {
  # Resample climate zones to match ocs_global, nearest neighbor to preserve classes
  kg_zones <- resample(kg_zones, ocs_global, method = "near")
}

# Calculate area per cell (in m)
cell_areas <- cellSize(ocs_global, unit = "m")

# Total OCS per cell (in kg)
ocs_total_kg <- ocs_global * cell_areas

# Mask out cells where either raster is NA
ocs_total_masked <- mask(ocs_total_kg, kg_zones)
kg_masked <- mask(kg_zones, ocs_total_kg)

plot(ocs_total_masked)
plot(kg_masked)

# Zonal sum: total kg per climate zone
sums_kg <- zonal(ocs_total_masked, kg_masked, fun = 'sum', na.rm = TRUE)
colnames(sums_kg)[1] <- "pro_KG_present"

# Join with legend
ocs_kg <- merge(legend_kg, sums_kg, by = "pro_KG_present", all.x = TRUE)
ocs_kg

# Convert kg to Gt
ocs_kg$ocs_0_200_Gt <- ocs_kg$ocs_0_200 / 1e12   # 1 Gt = 1e12 kg

colnames(ocs_kg)[4] <- "ocs_0_200_kg"

head(ocs_kg)
sum(ocs_kg$ocs_0_200_Gt)

### Calculate Area for each climate zone (based on area that has soils)

# Check the coordinate reference system
crs(kg_masked, describe = TRUE)

# Calculate cell sizes 
zone_areas <- expanse(kg_masked, unit = "km", byValue = TRUE)

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

# Merge legend with area estimates
kg_area_stocks <- legend_kg %>% 
  left_join(zone_areas_df) %>% 
  left_join(ocs_kg)

kg_area_stocks

# Save data frame
write.csv(kg_area_stocks, "./data/KG_present_area_stocks.csv", row.names = FALSE)

