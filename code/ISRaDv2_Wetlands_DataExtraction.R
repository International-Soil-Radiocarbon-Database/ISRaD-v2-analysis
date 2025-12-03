## Extract wetland data ##
## Sophie von Fromm ##
## 2025-10-17 ##

# Load packages
library(ISRaD)
library(tidyverse)

# Get most recent ISRaD data
izz <- ISRaD::ISRaD.getdata("./data", extra = TRUE, force_download = F)

### Extract and merge all data types

## Layer data ##
lyr_data <- ISRaD.flatten(izz, "layer") %>% 
  drop_na(lyr_14c) %>% 
  mutate(DataType = "Layer") %>% 
  dplyr::select(DataType, entry_name, pro_name, pro_peatland, lyr_name, 
                lyr_all_org_neg, lyr_14c, pro_land_cover, site_name, pro_long,
                pro_lat)

## Fraction data ##
frc_data <- ISRaD.flatten(izz, "fraction") %>% 
  drop_na(frc_14c) %>% 
  mutate(DataType = "Fraction") %>% 
  dplyr::select(DataType, entry_name, pro_name, pro_peatland, lyr_name, 
                lyr_all_org_neg, lyr_14c, frc_name, frc_scheme, frc_14c, 
                pro_land_cover, site_name, pro_long, pro_lat)

## Incubation data ##
inc_data <- ISRaD.flatten(izz, "incubation") %>% 
  drop_na(inc_14c) %>% 
  mutate(DataType = "Incubation") %>% 
  dplyr::select(DataType, entry_name, pro_name, pro_peatland, lyr_name, 
                lyr_all_org_neg, lyr_14c, inc_name, inc_14c, pro_land_cover, 
                site_name, pro_long, pro_lat)

## Flux data ##
flx_data <- ISRaD.flatten(izz, "flux") %>% 
  drop_na(flx_14c) %>% 
  mutate(DataType = "Flux") %>% 
  dplyr::select(DataType, entry_name, pro_name, pro_peatland, flx_name, flx_14c, 
                pro_land_cover, site_name, pro_long, pro_lat)

## Interstitial data ##
ist_data <- ISRaD.flatten(izz, "interstitial") %>% 
  drop_na(ist_14c) %>% 
  mutate(DataType = "Interstitial") %>% 
  dplyr::select(DataType, entry_name, pro_name, pro_peatland, ist_name, 
                ist_all_org_neg, ist_14c, pro_land_cover, site_name, pro_long, 
                pro_lat)

all_data <- lyr_data %>% 
  full_join(frc_data) %>% 
  full_join(inc_data) %>% 
  full_join(flx_data) %>% 
  full_join(ist_data) %>% 
  mutate(Version = "ISRaDv2")

# Extract wetland data
summary(all_data$pro_peatland) #2514 peatland data when 14C NAs are excluded, 28,503 with NAs 
summary(all_data$lyr_all_org_neg) #2839, 30,242 with NAs
summary(all_data$ist_all_org_neg) #95, 119 with all NAs
all_data %>% count(pro_land_cover) # without 14C NAs: wetland is 2922; with 14C NAs: wetland is 26,679

wet_data <- all_data %>% 
  filter(pro_land_cover == "wetland" |
           !is.na(pro_peatland) |
           !is.na(lyr_all_org_neg) |
           !is.na(ist_all_org_neg))

# get a count for 14C data of across data types
wet_data %>% 
  group_by(DataType) %>% 
  summarise(
    n_profile = n_distinct(entry_name, site_name, pro_name),
    n_sites = n_distinct(entry_name, site_name),
    n_entries = n_distinct(entry_name)) %>% 
  arrange(-n_entries)

# Fraction data by fraction scheme
wet_data %>%
  filter(DataType == "Fraction") %>% 
  count(frc_scheme) %>% 
  arrange(-n)

# Incubation studies in peatlands
wet_data %>%
  filter(DataType == "Incubation") %>% 
  group_by(pro_peatland) %>% 
  summarise(
    n_profile = n_distinct(entry_name, site_name, pro_name),
    n_sites = n_distinct(entry_name, site_name),
    n_entries = n_distinct(entry_name)) %>% 
  arrange(-n_entries)

## Load ISRaD v1 for comparison and mapping (Figure 6)
# Need to be downloaded from github first
## Layer data ##
lyr_data_v1 <- read.csv("./data/ISRaD_v1/ISRaD_database_files/ISRaD_extra_flat_layer_v1-2019-03-27.csv") %>% 
  drop_na(lyr_14c) %>% 
  mutate(DataType = "Layer") %>% 
  dplyr::select(DataType, entry_name, pro_name, lyr_name, lyr_all_org_neg, 
                lyr_14c, pro_land_cover, site_name, pro_long, pro_lat)

## Fraction data ##
frc_data_v1 <- read.csv("./data/ISRaD_v1/ISRaD_database_files/ISRaD_extra_flat_fraction_v1-2019-03-27.csv") %>% 
  drop_na(frc_14c) %>% 
  mutate(DataType = "Fraction") %>% 
  dplyr::select(DataType, entry_name, pro_name, lyr_name, lyr_all_org_neg, 
                lyr_14c, frc_name, frc_scheme, frc_14c, pro_land_cover, 
                site_name, pro_long, pro_lat)

## Incubation data ##
inc_data_v1 <- read.csv("./data/ISRaD_v1/ISRaD_database_files/ISRaD_extra_flat_incubation_v1-2019-03-27.csv") %>% 
  drop_na(inc_14c) %>% 
  mutate(DataType = "Incubation") %>% 
  dplyr::select(DataType, entry_name, pro_name, lyr_name, lyr_all_org_neg, 
                lyr_14c, inc_name, inc_14c, pro_land_cover, 
                site_name, pro_long, pro_lat)

## Flux data ##
flx_data_v1 <- read.csv("./data/ISRaD_v1/ISRaD_database_files/ISRaD_extra_flat_flux_v1-2019-03-27.csv") %>% 
  drop_na(flx_14c) %>% 
  mutate(DataType = "Flux") %>% 
  dplyr::select(DataType, entry_name, pro_name, flx_name, flx_14c,
                pro_land_cover, site_name, pro_long, pro_lat)

## Interstitial data ##
ist_data_v1 <- read.csv("./data/ISRaD_v1/ISRaD_database_files/ISRaD_extra_flat_interstitial_v1-2019-03-27.csv") %>% 
  drop_na(ist_14c) %>% 
  mutate(DataType = "Interstitial") %>% 
  dplyr::select(DataType, entry_name, pro_name, ist_name, ist_all_org_neg, 
                ist_14c, pro_land_cover, site_name, pro_long, pro_lat)

all_data_v1 <- lyr_data_v1 %>% 
  full_join(frc_data_v1) %>% 
  full_join(inc_data_v1) %>% 
  full_join(flx_data_v1) %>% 
  full_join(ist_data_v1) %>% 
  mutate(Version = "ISRaDv1")

### Merge both datasets
## v1 is missing column pro_peatland; only add v1 studies that are present in v2

# Get unique entries from v2
v2_entries <- wet_data %>%
  distinct(entry_name, site_name, pro_name) %>%
  mutate(in_v2 = TRUE)

# Filter v1 to only keep entries that exist in v2
v1_filtered <- all_data_v1 %>%
  semi_join(v2_entries, by = c("entry_name", "site_name","pro_name"))

# Merge both datasets
wet_data_v1_v2 <- v1_filtered %>%
  full_join(wet_data) %>% 
  dplyr::select(DataType, Version, pro_long, pro_lat, entry_name)
write.csv(wet_data_v1_v2, "./data/ISRaD_v1_v2_WetlandSites.csv", 
          row.names = FALSE)

# Quick check of sampling locations
library(rnaturalearth)
library(sf)

world <- rnaturalearth::ne_countries()

points_sf <- wet_data_v1_v2 %>%
  st_as_sf(coords = c("pro_long", "pro_lat"), crs = 4326)

points_sf$DataType <- factor(points_sf$DataType,
                             levels = c("Layer", "Flux", "Interstitial",
                                        "Fraction", "Incubation"),
                             ordered = TRUE)

wet_data_v1_v2 %>%
  ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray90", size = 0.2) +
  geom_sf(data = points_sf, aes(color = DataType), 
          size = 1.5, alpha = 0.7) +
  facet_wrap(~Version) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "top",
    axis.text = element_text(color = "black")
  ) +
  scale_color_manual("Data type:",
                     values = c("#F0E442", "#BBBBBB", "#66CCEE", "#228833",
                                "#EE6677"))


