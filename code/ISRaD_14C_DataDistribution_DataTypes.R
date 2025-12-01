## ISRaD v2 data analysis ##
## Data distribution of 14C across data types ##
## Sophie von Fromm ##
## May 14, 2025 ##

library(ISRaD)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

#### Get most recent ISRaD data ####
# force_download = TRUE to download database
izz <- ISRaD::ISRaD.getdata("./Data/", extra = TRUE, force_download = F)

## Prepare data for data type analysis
lyr_data <- ISRaD.flatten(izz, "layer") %>% 
  # drop_na(lyr_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name, pro_long, pro_lat, lyr_name,
                lyr_14c, lyr_top, lyr_bot, lyr_dd14c,
                pro_land_cover, pro_usda_soil_order, pro_KG_present_long,
                pro_KG_present_short, pro_peatland, lyr_all_org_neg) %>% 
  mutate(DataType = "layer")

frc_data <- ISRaD.flatten(izz, "fraction") %>% 
  # drop_na(frc_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name,pro_long, pro_lat, lyr_top, 
                lyr_bot, frc_name, frc_14c, frc_scheme, 
                frc_dd14c, frc_agent, frc_property, frc_lower, frc_upper, 
                pro_peatland, lyr_all_org_neg) %>% 
  mutate(DataType = "fraction")

ist_data <- ISRaD.flatten(izz, "interstitial") %>% 
  # drop_na(ist_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name,
                pro_long, pro_lat, ist_name, ist_depth, ist_14c, ist_analyte, 
                ist_dd14c, ist_phase,
                pro_land_cover, pro_usda_soil_order, pro_KG_present_long,
                pro_KG_present_short, pro_peatland) %>% 
  mutate(DataType = "interstitial") 

flx_data <- ISRaD.flatten(izz, "flux") %>% 
  # drop_na(flx_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name,
                pro_long, pro_lat, flx_name, flx_14c, flx_pathway, flx_analyte,
                flx_ecosystem_component, flx_dd14c,
                pro_land_cover, pro_usda_soil_order, pro_KG_present_long,
                pro_KG_present_short, pro_peatland) %>%
  mutate(DataType = "flux")

inc_data <- ISRaD.flatten(izz, "incubation") %>% 
  # drop_na(inc_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name,
                pro_long, pro_lat, lyr_top, lyr_bot, inc_name, inc_14c, inc_type,
                inc_analyte, inc_dd14c,
                pro_land_cover, pro_usda_soil_order, pro_KG_present_long,
                pro_KG_present_short, pro_peatland, lyr_all_org_neg) %>%
  mutate(DataType = "incubation")

all_data <- lyr_data %>% 
  full_join(frc_data) %>% 
  full_join(ist_data) %>% 
  full_join(flx_data) %>% 
  full_join(inc_data)

all_data %>%
  select(contains("_14c")) %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Count") %>%
  arrange(desc(Count)) %>% 
  mutate(sum_all14C = sum(Count))

## Explore and plot data
all_data %>% 
  group_by(DataType) %>% 
  summarise(n_entries = n_distinct(entry_name),
            n_sites = n_distinct(entry_name, site_name),
            n_profile = n_distinct(entry_name, site_name, pro_name))

# Layer data
all_data %>% 
  filter(DataType == "layer") %>% 
  drop_na(lyr_14c) %>% 
  count(pro_KG_present_long, pro_KG_present_short)

all_data %>% 
  filter(DataType == "layer") %>% 
  drop_na(lyr_14c) %>% 
  count(pro_usda_soil_order)

all_data %>% 
  filter(DataType == "layer") %>% 
  drop_na(lyr_14c) %>% 
  count(pro_land_cover)

# incubation data
all_data %>% 
  filter(DataType == "incubation") %>%
  drop_na(inc_14c) %>% 
  # Need to check if they are all CO2 or not
  count(inc_analyte)

all_data %>% 
  filter(DataType == "incubation") %>%
  drop_na(inc_14c) %>%
  count(inc_type)

all_data %>% 
  filter(DataType == "incubation") %>%
  drop_na(inc_14c) %>%
  filter(inc_type == "root-picked soil") %>% 
  count(pro_usda_soil_order) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

all_data %>% 
  filter(DataType == "incubation") %>%
  drop_na(inc_14c) %>%
  filter(inc_type == "root-picked soil") %>% 
  count(pro_land_cover) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

all_data %>% 
  filter(DataType == "incubation") %>%
  drop_na(inc_14c) %>%
  filter(inc_type == "root-picked soil") %>% 
  count(pro_KG_present_long, pro_KG_present_short) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

# interstitial data
all_data %>% 
  filter(DataType == "interstitial") %>%
  drop_na(ist_14c) %>% 
  count(ist_analyte)

all_data %>% 
  filter(DataType == "interstitial") %>%
  drop_na(ist_14c) %>% 
  count(ist_phase)

all_data %>% 
  filter(DataType == "interstitial") %>%
  drop_na(ist_14c) %>%
  count(ist_phase, ist_analyte)

all_data %>% 
  filter(DataType == "interstitial") %>%
  drop_na(ist_14c) %>%
  filter(ist_analyte == "CO2") %>% 
  count(pro_usda_soil_order) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

all_data %>% 
  filter(DataType == "interstitial") %>%
  drop_na(ist_14c) %>%
  filter(ist_analyte == "CO2") %>% 
  count(pro_land_cover) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

all_data %>% 
  filter(DataType == "interstitial") %>%
  drop_na(ist_14c) %>% 
  filter(ist_analyte == "CO2") %>% 
  count(pro_KG_present_long, pro_KG_present_short) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

# Flux data
all_data %>% 
  filter(DataType == "flux") %>%
  drop_na(flx_14c) %>%
  count(flx_analyte, flx_ecosystem_component)

all_data %>% 
  filter(DataType == "flux") %>%
  drop_na(flx_14c) %>% 
  #Missing flx_pathway are all flx_ecosystem_component = atmosphere
  count(flx_pathway)

all_data %>% 
  filter(DataType == "flux") %>%
  drop_na(flx_14c) %>% 
  count(flx_ecosystem_component)

all_data %>% 
  filter(DataType == "flux") %>%
  drop_na(flx_14c) %>% 
  count(flx_pathway, flx_ecosystem_component, flx_analyte)

all_data %>% 
  filter(DataType == "flux") %>% 
  drop_na(flx_14c) %>% 
  filter(flx_analyte == "CO2",
         flx_ecosystem_component == "ecosystem",
         flx_pathway == "soil emission") %>% 
  count(pro_usda_soil_order) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

all_data %>% 
  filter(DataType == "flux") %>% 
  drop_na(flx_14c) %>% 
  filter(flx_analyte == "CO2",
         flx_ecosystem_component == "ecosystem",
         flx_pathway == "soil emission") %>% 
  count(pro_land_cover) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

all_data %>% 
  filter(DataType == "flux") %>% 
  drop_na(flx_14c) %>% 
  filter(flx_analyte == "CO2",
         flx_ecosystem_component == "ecosystem",
         flx_pathway == "soil emission") %>% 
  count(pro_KG_present_long, pro_KG_present_short) %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total)

all_data_df <- all_data %>% 
  filter(DataType != "fraction") %>% 
  # remove -INF values for lyr_top and lyr_bot
  filter(!is.infinite(lyr_top) , !is.infinite(lyr_bot)) %>% 
  # remove peatlands and wetlands since they have different depth estimates
  filter(is.na(pro_peatland)) %>% 
  filter(pro_land_cover != "wetland"|
           is.na(pro_land_cover)) %>% 
  # remove studies for which mineral-organic interface is not clear
  filter(is.na(lyr_all_org_neg)) %>% 
  mutate(depth_cat = case_when(
    lyr_top < 0 ~ "litter",
    ((lyr_bot - lyr_top)/2 + lyr_top) <= 20 ~ "0-20 cm",
    ((lyr_bot - lyr_top)/2 + lyr_top) <= 50 ~ "20-50 cm",
    ((lyr_bot - lyr_top)/2 + lyr_top) > 50 ~ ">50 cm",
    ist_depth < 0 ~ "litter",
    ist_depth <= 20 ~ "0-20 cm",
    ist_depth <= 50 ~ "20-50 cm",
    ist_depth > 50 ~ ">50 cm"
  )) %>% 
  mutate(depth_cat = case_when(
    is.na(depth_cat) ~ "surface",
    TRUE ~ depth_cat
  )) %>% 
  mutate(depth_cat = factor(depth_cat, levels = c("litter", "0-20 cm",
                                                  "20-50 cm", ">50 cm",
                                                  "surface"),
                            ordered = TRUE)) %>% 
  #focus only on root-picked incubations
  filter(inc_type == "root-picked soil"|
           is.na(inc_type) & is.na(inc_14c)) %>% 
  #focus only on CO2 ecosystem soil fluxes from 
  filter(flx_analyte == "CO2"|
           is.na(flx_analyte) & is.na(flx_14c)) %>% 
  filter(flx_ecosystem_component == "ecosystem"|
           is.na(flx_ecosystem_component) & is.na(flx_14c)) %>% 
  #focus only on CO2 interstitial
  filter(ist_analyte == "CO2"|
           is.na(ist_analyte) & is.na(ist_14c)) %>% 
  pivot_longer(cols = c(lyr_dd14c, inc_dd14c,
                        flx_dd14c, ist_dd14c),
               names_to = "names",
               values_to = "value_dd14c") %>% 
  mutate(DataType = factor(DataType, levels = c("layer", "incubation", 
                                                "interstitial", "flux"),
                            ordered = TRUE))

all_data_Stats <- all_data_df %>% 
  group_by(depth_cat, DataType) %>% 
  summarise(median_dd14C = median(value_dd14c, na.rm = TRUE))

# Create labels for facets
facet_labels <- c(
  layer = "a) Bulk layer",
  incubation = "b) Incubation",
  interstitial = "c) Interstitial",
  flux = "d) Flux"
)

ggplot() +
  geom_segment(data = all_data_Stats, aes(y = median_dd14C, color = depth_cat,
                                    x = 0, xend = 900),
               linetype = "dashed", linewidth = 1) +
  geom_histogram(data = all_data_df, aes(y = value_dd14c, fill = depth_cat), bins = 50) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top",
        strip.text = element_text(size = 12, face = "bold", hjust = 0),
        strip.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent', color = NA)) +
  scale_x_continuous("Number of samples", expand = c(0,0), limits = c(0,900)) +
  scale_y_continuous(expression(paste(Delta,Delta^14, "C [‰]")), expand = c(0,0),
                     limits = c(-1500,300)) +
  scale_fill_manual("Layer depth", values = c("#238443", "#cc4c02", "#fe9929", "#fed98e", "grey")) +
  scale_color_manual("Layer depth", values = c("#238443", "#cc4c02", "#fe9929", "#fed98e", "grey")) +
  facet_wrap("DataType", nrow = 1, labeller = labeller(DataType = facet_labels))

ggsave("./output/Figure4.png", width = 8, height = 5, dpi = 600)




# Fraction data
# frc_data %>% 
#   summarise(n_frc = nrow(.),
#             n_entries = n_distinct(entry_name),
#             n_sites = n_distinct(entry_name, site_name),
#             n_profile = n_distinct(entry_name, site_name, pro_name))
# 
# frc_data %>% 
#   # Most samples are density fractionation with SPT 
#   count(frc_scheme, frc_property)
# 
# frc_data %>% 
#   filter(frc_scheme == "density" | frc_scheme == "particle size") %>% 
#   mutate(fraction = case_when(
#     frc_property == "heavy" ~ "mineral",
#     frc_property == "free light" ~ "particulate",
#     frc_property == "coarse" ~ "particulate",
#     frc_property == "fine" ~ "mineral",
#     frc_property == "sand" ~ "particulate",
#     frc_property == "clay" ~ "mineral",
#     frc_property == "silt+sand" ~ "particulate",
#     frc_property == "silt+clay" ~ "mineral",
#     frc_property == "silt" ~ "mineral"
#   )) %>% 
#   drop_na() %>% 
#   mutate(depth_cat = case_when(
#     #inconsistency - not all litter samples are inc_type == litter
#     lyr_top < 0 ~ "organic",
#     # inc_type == "litter" ~ "organic",
#     ((lyr_bot - lyr_top)/2 + lyr_top) <= 20 ~ "0-20 cm",
#     ((lyr_bot - lyr_top)/2 + lyr_top) <= 50 ~ "20-50 cm",
#     ((lyr_bot - lyr_top)/2 + lyr_top) > 50 ~ ">50 cm",
#   )) %>% 
#   mutate(depth_cat = factor(depth_cat, levels = c("organic", "0-20 cm",
#                                                   "20-50 cm", ">50 cm"),
#                             ordered = TRUE),
#          fraction = factor(fraction, levels = c("particulate", "mineral"),
#                            ordered = TRUE)) %>% 
#   ggplot(aes(y = frc_dd14c, fill = depth_cat)) +
#   facet_wrap(~fraction) +
#   geom_histogram(bins = 50) +
#   theme_bw(base_size = 16) +
#   theme(axis.text = element_text(color = "black"),
#         legend.position = "top") +
#   scale_x_continuous("Number of samples", expand = c(0,0), limits = c(0,250)) +
#   scale_y_continuous(expression(paste(Delta,Delta^14, "C [‰]")), expand = c(0,0),
#                      limits = c(-1500,650)) +
#   scale_fill_manual("Layer depth", values = c("#238443", "#cc4c02", "#fe9929", "#fed98e"))






