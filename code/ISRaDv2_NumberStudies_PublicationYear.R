## ISRaD v2 data analysis ##
## Global representative analysis ##
## Sophie von Fromm ##
## August 5, 2025 ##

# Load packages
library(ISRaD)
library(tidyverse)

#### Get most recent ISRaD data ####
izz <- ISRaD::ISRaD.getdata("./Data/", extra = TRUE, force_download = F)

# Prepare data
lyr_data <- ISRaD.flatten(izz, "layer") %>% 
  drop_na(lyr_14c) %>% 
  unite("ID", c(entry_name, site_name, pro_name), remove = FALSE) %>% 
  dplyr::select(ID, entry_name, pro_long, pro_lat, lyr_14c, lyr_top, lyr_bot,
                pro_KG_present_short, lyr_obs_date_y) %>%
  rename(obs_date_y = lyr_obs_date_y) %>%
  mutate(DataType = "layer")

frc_data <- ISRaD.flatten(izz, "fraction") %>% 
  drop_na(frc_14c) %>% 
  unite("ID", c(entry_name, site_name, pro_name), remove = FALSE) %>% 
  dplyr::select(ID, entry_name, pro_long, pro_lat, lyr_top, lyr_bot, frc_14c, frc_scheme,
                pro_KG_present_short, lyr_obs_date_y.x) %>% 
  rename(obs_date_y = lyr_obs_date_y.x) %>% 
  mutate(DataType = "fraction")

ist_data <- ISRaD.flatten(izz, "interstitial") %>% 
  drop_na(ist_14c) %>% 
  unite("ID", c(entry_name, site_name, pro_name), remove = FALSE) %>% 
  dplyr::select(ID, entry_name, pro_long, pro_lat, ist_depth, ist_14c, ist_analyte,
                pro_KG_present_short, ist_obs_date_y) %>% 
  rename(obs_date_y = ist_obs_date_y) %>% 
  mutate(DataType = "interstitial")

flx_data <- ISRaD.flatten(izz, "flux") %>% 
  drop_na(flx_14c) %>% 
  unite("ID", c(entry_name, site_name, pro_name), remove = FALSE) %>% 
  dplyr::select(ID, entry_name, pro_long, pro_lat, flx_14c, flx_pathway, flx_analyte, 
                flx_ecosystem_component, pro_KG_present_short, flx_obs_date_y) %>% 
  rename(obs_date_y = flx_obs_date_y) %>% 
  mutate(DataType = "flux")

inc_data <- ISRaD.flatten(izz, "incubation") %>% 
  drop_na(inc_14c) %>% 
  unite("ID", c(entry_name, site_name, pro_name), remove = FALSE) %>% 
  dplyr::select(ID, entry_name, pro_long, pro_lat, lyr_top, lyr_bot, inc_14c, inc_type,
                inc_analyte, pro_KG_present_short, inc_obs_date_y) %>% 
  rename(obs_date_y = inc_obs_date_y) %>% 
  mutate(DataType = "incubation")

all_data <- lyr_data %>% 
  full_join(frc_data) %>% 
  full_join(ist_data) %>% 
  full_join(flx_data) %>% 
  full_join(inc_data)

# Number of studies per year
# all_data_sum <- all_data %>% 
#   #_unpub studies do not get a number
#   mutate(year = as.numeric(str_extract(entry_name, "\\d{4}"))) %>% 
#   filter(year != 2025) %>% 
#   group_by(entry_name, DataType, year) %>%
#   count()
  
# all_data_sum %>% 
#   filter(year != 2025) %>% 
#   ggplot(aes(x = year, fill = DataType)) +
#   geom_bar() +
#   theme_bw(base_size = 16) +
#   theme(axis.text = element_text(color = "black"),
#         axis.ticks = element_line(color = "black"),
#         plot.margin = margin(t = 10,  # Top margin
#                              r = 20,  # Right margin
#                              b = 10,  # Bottom margin
#                              l = 10)) + # Left margin) 
#   scale_x_continuous("Publication year", expand = c(0,0),
#                      limits = c(1950,2025), breaks = seq(1950,2025,5)) +
#   scale_y_continuous("Number of publications", expand = c(0,0),
#                      limits = c(0,40))

# Number of data types by climate zone and year 
## Create color scheme
all_zones <- levels(all_data$pro_KG_present_short)

main_group <- substr(all_zones, 1, 1)

main_palette <- c(
  A = "#1f78b4",   # blue (tropic)
  B = "#ffb400",   # orange (arid)
  C = "#33a02c",   # green (temperate)
  D = "#a858a7",   # purple (continental)
  E = "#b0b0b0"    # grey (polar)
)

kg_color <- setNames(main_palette[main_group], all_zones)

# Create legend
main_names <- c(
  A = "Tropical (A)",
  B = "Arid (B)",
  C = "Temperate (C)",
  D = "Continental (D)",
  E = "Polar (E)"
)

all_data$main_group <- substr(all_data$pro_KG_present_short, 1, 1)

all_data$DataType <- factor(all_data$DataType, 
                            levels = c("layer", "flux", "interstitial",
                                       "fraction", "incubation"))

# Create labels for facets
facet_labels <- c(
  layer = "a) Bulk layer",
  flux = "b) Flux",
  interstitial = "c) Interstitial",
  fraction = "d) Fraction",
  incubation = "e) Incubation"
)

all_data %>% 
  group_by(entry_name, obs_date_y, pro_KG_present_short, DataType, main_group) %>% 
  count() %>% 
  drop_na(pro_KG_present_short) %>% 
  ggplot(aes(x = obs_date_y, y = n, fill = main_group)) +
  geom_col() +
  theme_bw(base_size = 16) +
  theme(axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.margin = margin(t = 10,  # Top margin
                             r = 20,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10), # Left margin
        legend.position = "top",
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        strip.text = element_text(size = 12, face = "bold", hjust = 0, color = "black"),
        legend.background = element_rect(fill = "transparent", color = NA)) +  
  scale_x_continuous("Observation year", expand = c(0,0),
                     limits = c(1895,2025), breaks = seq(1895,2025,25)) +
  scale_y_continuous(expression(paste("Number of  ", Delta^14, "C observations")), 
                     expand = c(0,0)) +
  facet_wrap(~DataType, scales = "free_y", 
             labeller = labeller(DataType = facet_labels)) +
  scale_fill_manual(
    name = "Main climate zone",
    values = main_palette,
    labels = main_names)
ggsave("./output/FigureS1.png", width = 10, height = 6, dpi = 600)

### Number of published 14C studies

## Get number of 14C studies based on web of science (accessed: Aug, 5 2025)
num_14C <- read.delim("./Data/WoF_Soil_Radiocarbon_Search_05082025.txt")

# Plot Data
num_14C %>% 
  filter(Publication.Years != 2025) %>% 
  ggplot(aes(x = Publication.Years, y = Record.Count)) +
  geom_col() +
  theme_bw(base_size = 16) +
  theme(axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.margin = margin(t = 10,  # Top margin
                             r = 20,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10), # Left margin
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)) +  
  scale_x_continuous("Publication year", expand = c(0,0),
                     limits = c(1950,2025), breaks = seq(1950,2025,5)) +
  scale_y_continuous("Number of publications", expand = c(0,0),
                     limits = c(0,200))
ggsave("./output/FigureS2.png", width = 10, height = 5, dpi = 600)
