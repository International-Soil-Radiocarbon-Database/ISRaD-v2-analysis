## Extract wetland data ##
## Sophie von Fromm ##
## 2025-10-17 ##

# Load packages
library(ISRaD)
library(tidyverse)

# Get most recent ISRaD data
izz <- ISRaD::ISRaD.getdata("./data/", extra = TRUE, force_download = F)

### Extract and merge all data types

## Layer data ##
lyr_data <- ISRaD.flatten(izz, "layer") %>% 
  drop_na(lyr_14c) %>% 
  mutate(DataType = "Layer")

lyr_data$modification_date_y <- as.integer(lyr_data$modification_date_y)
lyr_data$modification_date_m <- as.integer(lyr_data$modification_date_m)
lyr_data$modification_date_d <- as.integer(lyr_data$modification_date_d)
lyr_data$template_version <- as.double(lyr_data$template_version)

## Fraction data ##
frc_data <- ISRaD.flatten(izz, "fraction") %>% 
  drop_na(frc_14c) %>% 
  mutate(DataType = "Fraction")

frc_data$modification_date_y <- as.integer(frc_data$modification_date_y)
frc_data$modification_date_m <- as.integer(frc_data$modification_date_m)
frc_data$modification_date_d <- as.integer(frc_data$modification_date_d)
frc_data$template_version <- as.double(frc_data$template_version)

## Incubation data ##
inc_data <- ISRaD.flatten(izz, "incubation") %>% 
  drop_na(inc_14c) %>% 
  mutate(DataType = "Incubation")

inc_data$modification_date_y <- as.integer(inc_data$modification_date_y)
inc_data$modification_date_m <- as.integer(inc_data$modification_date_m)
inc_data$modification_date_d <- as.integer(inc_data$modification_date_d)
inc_data$template_version <- as.double(inc_data$template_version)

## Flux data ##
flx_data <- ISRaD.flatten(izz, "flux") %>% 
  drop_na(flx_14c) %>% 
  mutate(DataType = "Flux")

flx_data$modification_date_y <- as.integer(flx_data$modification_date_y)
flx_data$modification_date_m <- as.integer(flx_data$modification_date_m)
flx_data$modification_date_d <- as.integer(flx_data$modification_date_d)
flx_data$template_version <- as.double(flx_data$template_version)

## Interstitial data ##
ist_data <- ISRaD.flatten(izz, "interstitial") %>% 
  drop_na(ist_14c) %>% 
  mutate(DataType = "Interstitial")

ist_data$modification_date_y <- as.integer(ist_data$modification_date_y)
ist_data$modification_date_m <- as.integer(ist_data$modification_date_m)
ist_data$modification_date_d <- as.integer(ist_data$modification_date_d)
ist_data$template_version <- as.double(ist_data$template_version)

all_data <- lyr_data %>% 
  full_join(frc_data) %>% 
  full_join(inc_data) %>% 
  full_join(flx_data) %>% 
  full_join(ist_data)

# Extract wetland data
summary(all_data$pro_peatland)
summary(all_data$lyr_all_org_neg)
summary(all_data$ist_all_org_neg)
all_data %>% count(pro_land_cover)

org_data <- all_data %>% 
  filter(pro_land_cover == "wetland" |
           !is.na(pro_peatland) |
           !is.na(lyr_all_org_neg) |
           !is.na(ist_all_org_neg))

summary(org_data$pro_peatland)
summary(org_data$lyr_all_org_neg)
summary(org_data$ist_all_org_neg)
org_data %>% count(pro_land_cover)
summary(org_data$entry_name)

write.csv(org_data, "./data/Wetlands14C.csv", row.names = FALSE)

