## ISRaD v2 data analysis ##
## Data distribution by depth and climate zones ##
## Shane Stoner, Sophie von Fromm ##
## May 02, 2025 ##

# Load packages
library(ISRaD)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

## Get most recent ISRaD data
# force_download = TRUE to download database
izz <- ISRaD::ISRaD.getdata("./data/", extra = TRUE, force_download = F)

#### Data distribution of all data types: Figure 1 ####

### Extract and merge all data types
## Layer data ##
lyr_data <- ISRaD.flatten(izz, "layer") %>% 
  drop_na(lyr_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name, pro_KG_present_short, 
                pro_land_cover, pro_usda_soil_order,
                lyr_name, lyr_14c) %>% 
  mutate(DataType = "Layer")

## Fraction data ##
frc_data <- ISRaD.flatten(izz, "fraction") %>% 
  drop_na(frc_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name, pro_KG_present_short, 
                pro_land_cover, pro_usda_soil_order,
                lyr_name, frc_name, frc_14c) %>%  
  mutate(DataType = "Fraction")

## Incubation data ##
inc_data <- ISRaD.flatten(izz, "incubation") %>% 
  drop_na(inc_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name, pro_KG_present_short, 
                pro_land_cover, pro_usda_soil_order,
                lyr_name, inc_name, inc_14c) %>%  
  mutate(DataType = "Incubation")

## Flux data ##
flx_data <- ISRaD.flatten(izz, "flux") %>% 
  drop_na(flx_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name, pro_KG_present_short, 
                pro_land_cover, pro_usda_soil_order,
                flx_name, flx_14c) %>%  
  mutate(DataType = "Flux")

## Interstitial data ##
ist_data <- ISRaD.flatten(izz, "interstitial") %>% 
  drop_na(ist_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name, pro_KG_present_short, 
                pro_land_cover, pro_usda_soil_order,
                ist_name, ist_14c) %>%  
  mutate(DataType = "Interstitial")

all_data <- lyr_data %>% 
  full_join(frc_data) %>% 
  full_join(inc_data) %>% 
  full_join(flx_data) %>% 
  full_join(ist_data)

# summary(all_data)
# 
# all_data %>% 
#   group_by(pro_usda_soil_order, DataType) %>% 
#   count()

# Modify land cover names for better plotting
all_data$pro_land_cover <- factor(all_data$pro_land_cover,
                                  levels = c("bare", "cultivated", "forest",
                                             "rangeland/grassland", "shrubland",
                                             "tundra", "urban", "wetland"),
                                  labels = c("bare", "cultivated", "forest",
                                             "range-/grassland", "shrubland",
                                             "tundra", "urban", "wetland"))

### Look into data distribution
## Climate zones
climate_sum_func <- function(variable){
  all_data %>% 
    drop_na(pro_KG_present_short) %>% 
    mutate(climate_group = substr(pro_KG_present_short, 1, 1)) %>%
    filter(DataType == {{variable}}) %>% 
    group_by(climate_group) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(n_total = sum(n),
           perc = (n * 100)/n_total)
}

climate_sum_func("Layer")
climate_sum_func("Interstitial")
climate_sum_func("Incubation")
climate_sum_func("Fraction")
climate_sum_func("Flux")

## Land cover
land_sum_func <- function(variable){
  all_data %>% 
    drop_na(pro_land_cover) %>% 
    filter(DataType == {{variable}}) %>% 
    group_by(pro_land_cover) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(n_total = sum(n),
           perc = (n * 100)/n_total) %>% 
    arrange(-perc)
}

land_sum_func("Layer")
land_sum_func("Interstitial")
land_sum_func("Incubation")
land_sum_func("Fraction")
land_sum_func("Flux")

## Soil order
soil_sum_func <- function(variable){
  all_data %>% 
    drop_na(pro_usda_soil_order) %>% 
    filter(DataType == {{variable}}) %>% 
    group_by(pro_usda_soil_order) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(n_total = sum(n),
           perc = (n * 100)/n_total) %>% 
    arrange(-perc)
}

soil_sum_func("Layer")
soil_sum_func("Interstitial")
soil_sum_func("Incubation")
soil_sum_func("Fraction")
soil_sum_func("Flux")

### Plot data ##
# function to plot data
plot_func <- function(variable){
  all_data %>% 
    drop_na({{variable}}) %>% 
    group_by(DataType, {{variable}}) %>% 
    count()  %>% 
    ggplot(aes(x = {{variable}}, y = DataType, fill = n)) +
    geom_tile() +
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    theme_classic(base_size = 17) +
    theme(axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.title = element_blank(),
          plot.background = element_rect(fill = 'transparent', color = NA),
          legend.background = element_rect(fill = 'transparent', color = NA),
          title = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(t = 0.1, r = 0.1, b = 0.3, l = 0.1), "cm")) +
    scale_fill_gradient(low = "white", high="blue", trans = "log10",
                        limits = c(1,3545), breaks = c(1,10,100,1000),
                        labels = c("0", "10", "100", "1000")) +
    geom_text(aes(label = n), color = "white", size = 4)
}

pc <- plot_func(pro_KG_present_short) +
  ggtitle("a) Koeppen Geiger Climate Zones")
pl <- plot_func(pro_land_cover) +
  ggtitle("b) Land Cover")
ps <- plot_func(pro_usda_soil_order) +
  ggtitle("c) USDA Soil Order")

ggarrange(pc, pl, ps, ncol = 1, common.legend = TRUE, legend = "right")

ggsave("./output/Figure1.png", width = 12, height = 8, dpi = 600)


#######################################################

#### Data distribution of bulk layer data with depth: Figure 2, S3, S4 ####

#######################################################

# Prepare data
lyr_data_14C <- ISRaD.flatten(izz, "layer") %>% 
  drop_na(lyr_14c) %>%
  # remove peatlands and wetlands since they have different depth estimates
  filter(is.na(pro_peatland)) %>% 
  filter(pro_land_cover != "wetland"|
           is.na(pro_land_cover)) %>% 
  # remove studies for which mineral-organic interface is not clear
  filter(is.na(lyr_all_org_neg)) %>% 
  # remove -INF values for lyr_top and lyr_bot
  filter(!is.infinite(lyr_top) , !is.infinite(lyr_bot)) %>% 
  # limit depth to 2 m for display purpose
  filter(lyr_bot <= 200) 

# summary(lyr_data_14C$lyr_top)
# summary(lyr_data_14C$lyr_bot)

# lyr_data_14C %>% 
#   unite("pro_id", c(entry_name, site_name, pro_name)) %>% 
#   distinct(pro_id, .keep_all = TRUE) %>% 
#   count(pro_KG_present_short) %>% 
#   arrange(n)

# Load C stocks for each climate zone (based on Beck et al. 2023)
kg_c_stocks <- read.csv("./data/KG_present_area_stocks.csv")
head(kg_c_stocks)

# Arrange climate zones by C stocks 
zone_order <- kg_c_stocks %>%
  arrange(desc(ocs_0_200_Gt)) %>%
  pull(pro_KG_present_short)

# Set factors for plotting
lyr_data_14C$pro_KG_present_short <- factor(lyr_data_14C$pro_KG_present_short, 
                                            levels = zone_order)

# Create a label data frame to show ALL zones
label_df <- tibble(pro_KG_present_short = factor(zone_order, levels = zone_order), 
                   y = 50)

## Create color scheme
all_zones <- levels(lyr_data_14C$pro_KG_present_short)

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

lyr_data_14C$main_group <- substr(lyr_data_14C$pro_KG_present_short, 1, 1)

p1 <- lyr_data_14C %>% 
  drop_na(pro_KG_present_short) %>% 
  ggplot(aes(x = pro_KG_present_short, fill = main_group)) +
  geom_rect(aes(xmin = as.numeric(pro_KG_present_short) - 0.4, 
                xmax = as.numeric(pro_KG_present_short) + 0.4,
                ymin = -lyr_bot, 
                ymax = -lyr_top), 
            alpha = 0.05) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")  +
  # Add ALL possible zone labels along x axis at y=50
  geom_text(data = label_df,
            aes(x = pro_KG_present_short, y = y, label = pro_KG_present_short),
            inherit.aes = FALSE) +
  scale_y_continuous("Depth (cm)", expand = c(0,0), limits = c(-200,70)) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(
    name = "Main climate zone",
    values = main_palette,
    labels = main_names) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(.5,2.0,1.7,0.5), "cm"),
        legend.direction = "horizontal",
        legend.position = "top",
        plot.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

# Check data distribution
lyr_data_14C %>%
  drop_na(pro_KG_present_short) %>%
  mutate(depth_bin = cut(-lyr_top, breaks=seq(-200,70, by = 10))) %>% 
  mutate(climate_group = substr(pro_KG_present_short, 1, 1)) %>%
  group_by(depth_bin) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total) %>% 
  arrange(-perc)

lyr_data_14C %>%
  drop_na(pro_KG_present_short) %>%
  mutate(climate_group = substr(pro_KG_present_short, 1, 1)) %>%
  group_by(climate_group) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total) %>% 
  arrange(-perc)

# Plot depth density
p2 <- lyr_data_14C %>%
  drop_na(pro_KG_present_short) %>%
  mutate(depth_bin = cut(-lyr_top, breaks=seq(-200,70, by = 10))) %>%
  ggplot(aes(x = depth_bin, fill = main_group)) +
  geom_vline(aes(xintercept = 50.5), linetype = "dashed")  +
  geom_bar(position = "stack") +
  coord_flip() +
  scale_y_reverse("Count", expand = c(0,0),
                  limit = c(2600,0)) +   # Reverse so 'deeper' is lower
  scale_x_discrete("Depth (cm)", drop = FALSE) +
  theme_bw(base_size = 18) +
  scale_fill_manual(
    name = "Main climate zone",
    values = main_palette,
    labels = main_names) +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.5,0.1,0.5,0.5), "cm"),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

## Plot soil carbon stocks for each climate zone
# Reorder factors so bars align
kg_c_stocks$pro_KG_present_short <- factor(
  kg_c_stocks$pro_KG_present_short,
  levels = zone_order
)

# Set color for bars from main group, so colors match below!
kg_c_stocks$main_group <- substr(kg_c_stocks$pro_KG_present_short, 1, 1)

# Bar plot: soil C stocks per climate zone
p3 <- kg_c_stocks %>% 
  ggplot(aes(x = pro_KG_present_short)) +
  geom_col(aes(y = ocs_0_200_Gt, fill = main_group), width = 0.8) +
  geom_point(aes(y = area_km2 / 50000), color = "#8c96c6", size = 2) +  # Scale area to fit
  geom_line(aes(y = area_km2 / 50000, group = 1), color = "#8c96c6", linewidth = 1) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = main_palette, guide = FALSE) +
  scale_y_continuous(name = "Soil C stock (Gt)", expand = c(0,0),
                     limits = c(0,500),
                     sec.axis = sec_axis(~ . * 50000, 
                                         name = "Area (Million km²)", 
                                         labels = function(x) x/1000000)) + # Convert to millions
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(color = "black"),
        axis.text.y.right = element_text(color = "#8c96c6"),
        axis.title.y.right = element_text(color = "#8c96c6"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.1,0.75), "cm"),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

## Plot all graphs together
# create an empty plot
blank <- ggplot() + 
  theme_void()

# Extract the legend from p1:
legend <- get_legend(p1)

# Remove legends from the p1
p1_noleg <- p1 +
  theme(legend.position = "none")

# Make right panel (stack p3 over p1)
right_panel <- ggarrange(
  p3, p1_noleg,
  ncol = 1,
  heights = c(1, 2),
  labels = c("a)", "c)"))

# Make left panel
left_panel <- ggarrange(
  blank, p2, 
  ncol = 1, 
  heights = c(1, 2),
  labels = c("", "b)"))

# Arrange p2 and right_panel side by side (p2 only as tall as p1)
main <- ggarrange(
  left_panel, right_panel, 
  ncol = 2, 
  widths = c(0.3, 1))

# Add legend on top
final_plot <- ggarrange(
  legend, main, 
  ncol = 1, 
  heights = c(0.1, 1))

final_plot

ggsave("./output/Figure2.png", width = 14, height = 10, dpi = 600)

### Figures with soil type: S3
lyr_data_14C$pro_usda_soil_order <- droplevels(lyr_data_14C$pro_usda_soil_order)

lyr_data_14C %>% 
  unite("pro_id", c(entry_name, site_name, pro_name)) %>% 
  distinct(pro_id, .keep_all = TRUE) %>% 
  count(pro_usda_soil_order)

lyr_data_14C %>%
  drop_na(pro_usda_soil_order) %>%
  count(pro_usda_soil_order) %>% 
  ungroup() %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total) %>% 
  arrange(-perc)

# lyr_data_14C %>%
#   drop_na(pro_usda_soil_order) %>%
#   mutate(depth_bin = cut(-lyr_top, breaks=seq(-200,70, by = 10))) %>% 
#   group_by(depth_bin, pro_usda_soil_order) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(n_total = sum(n),
#          perc = (n * 100)/n_total) %>% 
#   arrange(-perc) %>% view()

# Plot depth density
p1.1 <- lyr_data_14C %>% 
  drop_na(pro_usda_soil_order) %>% 
  ggplot(aes(x = pro_usda_soil_order, fill = pro_usda_soil_order)) +
  geom_rect(aes(xmin = as.numeric(pro_usda_soil_order) - 0.4, 
                xmax = as.numeric(pro_usda_soil_order) + 0.4,
                ymin = -lyr_bot, 
                ymax = -lyr_top), 
            alpha = 0.05) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")  +
  scale_y_continuous("Depth (cm)", expand = c(0,0), limits = c(-200,70)) +
  scale_x_discrete(drop = FALSE) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.direction = "horizontal",
        legend.position = "none",
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

# Plot depth density
p2.1 <- lyr_data_14C %>% 
  drop_na(pro_usda_soil_order) %>%
  mutate(depth_bin = cut(-lyr_top, breaks = seq(-200,70, by = 10))) %>%
  ggplot(aes(x = depth_bin, fill = pro_usda_soil_order)) +
  geom_vline(aes(xintercept = 50.5), linetype = "dashed")  +
  geom_bar(position = "stack") +
  coord_flip() +
  scale_y_reverse("Count", expand = c(0,0),
                  limit = c(1400,0)) +   # Reverse so 'deeper' is lower
  scale_x_discrete("Depth (cm)", drop = FALSE) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.5,0.1,1.3,0.5), "cm"),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

soil_order_p <- ggarrange(p2.1, p1.1,  labels = c("a)", "b)"))
soil_order_p

ggsave("./output/FigureS3.png", width = 14, height = 8, dpi = 600)

### Figures with land cover
lyr_data_14C$pro_land_cover <- droplevels(lyr_data_14C$pro_land_cover)

lyr_data_14C %>% 
  unite("pro_id", c(entry_name, site_name, pro_name)) %>% 
  distinct(pro_id, .keep_all = TRUE) %>% 
  count(pro_land_cover)

lyr_data_14C %>%
  drop_na(pro_land_cover) %>%
  count(pro_land_cover) %>% 
  ungroup() %>% 
  mutate(n_total = sum(n),
         perc = (n * 100)/n_total) %>% 
  arrange(-perc)

# Plot depth density
p1.2 <- lyr_data_14C %>% 
  drop_na(pro_land_cover) %>% 
  ggplot(aes(x = pro_land_cover, fill = pro_land_cover)) +
  geom_rect(aes(xmin = as.numeric(pro_land_cover) - 0.4, 
                xmax = as.numeric(pro_land_cover) + 0.4,
                ymin = -lyr_bot, 
                ymax = -lyr_top), 
            alpha = 0.05) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")  +
  scale_y_continuous("Depth (cm)", expand = c(0,0), limits = c(-200,70)) +
  scale_x_discrete(drop = FALSE) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.direction = "horizontal",
        legend.position = "none",
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

# Plot depth density
p2.2 <- lyr_data_14C %>% 
  drop_na(pro_land_cover) %>%
  mutate(depth_bin = cut(-lyr_top, breaks = seq(-200,70, by = 10))) %>%
  ggplot(aes(x = depth_bin, fill = pro_land_cover)) +
  geom_vline(aes(xintercept = 50.5), linetype = "dashed")  +
  geom_bar(position = "stack") +
  coord_flip() +
  scale_y_reverse("Count", expand = c(0,0),
                  limit = c(2500,0)) +   # Reverse so 'deeper' is lower
  scale_x_discrete("Depth (cm)", drop = FALSE) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.5,0.1,2.7,0.5), "cm"),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

land_cover_p <- ggarrange(p2.2, p1.2,  labels = c("a)", "b)"))
land_cover_p

ggsave("./output/FigureS4.png", width = 14, height = 8, dpi = 600)



  


