## ISRaD v2 data analysis ##
## Data distribution of 14C across fractions and depths ##
## Olga Vindušková, Caitlin Hicks Pries, Sophie von Fromm ##
## November 14, 2025 ##

library(ISRaD)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

#### Get most recent ISRaD data ####
# force_download = TRUE to download database
izz <- ISRaD::ISRaD.getdata("./Data/", extra = TRUE, force_download = F)

frc_data_full <- ISRaD.flatten(izz, "fraction") %>% 
  # drop_na(frc_14c) %>% 
  dplyr::select(entry_name, site_name, pro_name,pro_long, pro_lat, lyr_top, 
                lyr_bot, frc_name, frc_14c, frc_scheme, 
                frc_dd14c, frc_agent, frc_property, frc_lower, frc_upper, 
                pro_peatland, lyr_all_org_neg) %>% 
  mutate(DataType = "fraction")

## Check data distribtuion
frc_data_full %>% 
  count(frc_scheme) %>% 
  arrange(-n)

## Prepare data for data type analysis
frc_data <- ISRaD.flatten(izz, "fraction") %>% 
  drop_na(frc_14c) %>%
  # remove peatlands and wetlands since they have different depth estimates
  filter(is.na(pro_peatland)) %>%
  filter(pro_land_cover != "wetland"|
           is.na(pro_land_cover)) %>%
  # remove studies for which mineral-organic interface is not clear
  filter(is.na(lyr_all_org_neg)) %>%
  # remove -INF values for lyr_top and lyr_bot
  filter(!is.infinite(lyr_top) , !is.infinite(lyr_bot)) %>%
  dplyr::select(entry_name, bibliographical_reference, site_name, pro_name, 
                pro_long, pro_lat, lyr_top, lyr_name,frc_input,
                lyr_bot, frc_name, frc_obs_date_y, frc_14c, frc_scheme, 
                frc_dd14c, frc_agent, frc_property, frc_lower, frc_upper, 
                frc_scheme_units) %>% 
  mutate(DataType = "fraction")

#starting with top 20 cm
frc_subset <- frc_data %>% 
  filter(
         # lyr_bot <= 30, #use only data from top 30 cm
         frc_scheme %in% c("aggregate", "chemical", "density", "particle size"),
         !frc_property %in% c("carbonate", "microbial residues"),
         !(frc_property == "free light" & frc_scheme == "particle size"), #weirdness, can delete once ISRaD updated
         !(frc_property == "sand" & frc_scheme == "density"), #weirdness, can delete once ISRaD updated
         !(frc_property == "humin" & frc_scheme == "particle size"), #weirdness Anderson 1984, I think this is a weird scheme and the template did the best it could
         !(frc_property == "extracted" & frc_scheme == "particle size"), #also Anderson 1984
         !is.na(frc_property)) %>% 
  mutate(depth = if_else(lyr_bot > 50, ">50 cm",
                if_else(lyr_bot <=20, "0-20 cm", "20-50 cm")))

frc_subset$depth <- ordered(frc_subset$depth, levels= c("0-20 cm", "20-50 cm", ">50 cm"))


nrow(frc_subset)
#3467, after filtering out unwanted categories (thermal, carbonate, microbial residues) and weirdnesses

#add new fractionation scheme category = frc_scheme2 for graph
frc_subset$frc_scheme2 <- frc_subset$frc_scheme

#labels for graph
frc_subset <- frc_subset %>%
  mutate(
    frc_scheme2 = case_when(
      frc_property %in% c("fine", "coarse") ~ "particle size - other",
      frc_property %in% c("fulvic acid", "humic acid", "humin") ~ "chemical - HS",
      frc_property %in% c("extracted", "residual") ~ "chemical - other",
      TRUE ~ frc_scheme2  # keep existing value if none of the conditions match
    )
  )

#filter out categories we do not want and order them
frc_subset$frc_scheme2 <- as.factor(frc_subset$frc_scheme2)
levels(frc_subset$frc_scheme2)
frc_subset$frc_property <- as.character(frc_subset$frc_property)
frc_subset$frc_property <- as.factor(frc_subset$frc_property)
levels(frc_subset$frc_property)

frc_subset <- frc_subset %>%
  filter(frc_scheme2 != "particle size - other"|is.na(frc_scheme2)) %>%
  mutate(frc_scheme2 = droplevels(frc_scheme2)) %>%
  filter(frc_property != "fine"|is.na(frc_property)) %>%
  mutate(frc_property = droplevels(frc_property))


frc_subset$frc_scheme2<- ordered(frc_subset$frc_scheme2, c("density",
                                                           "aggregate",
                                                           "particle size",
                                                           #"particle size - other",
                                                           "chemical - HS",
                                                           "chemical - other"
                                                          ))


frc_subset$frc_property<- ordered(frc_subset$frc_property, c("free light",
                                                             "occluded light", 
                                                             "heavy",
                                                             "macroaggregate",
                                                             "microaggregate",
                                                             "sand",
                                                             "silt+sand",
                                                             "silt",
                                                             "silt+clay",
                                                             "clay",
                                                             #"coarse", 
                                                             #"fine",
                                                             "fulvic acid",
                                                             "humic acid",
                                                             "humin",
                                                             "extracted",
                                                             "residual"))

table(frc_subset$frc_property)


#colours####

soc_colors <- c("#fdae61", #free light
                "#d73027", #occluded light
                "#4d2600", #heavy
                "#f46d43", #macroaggregates
                "#a63603", #microaggregates
                "#fdae61", #sand
                "#f46d43", #silt+sand
                "#d73027", #silt
                "#a63603", #silt+clay
                "#4d2600", #clay
               # "#fdd49e", #coarse
                "#fdae61", #fulvic
                "#d73027", #humic
                "#4d2600", # humin,
                "#fdae61", #extracted
                "#4d2600"  #residual
                )

  
#calculate median values
frc_data_Stats <- frc_subset %>% 
  # group_by(frc_scheme2, frc_property) %>% 
  group_by(frc_scheme2, frc_property, depth) %>% 
  summarise(median_dd14C = median(frc_dd14c, na.rm = TRUE),
            median_d14C = median(frc_14c, na.rm = TRUE),
            n_dd14C = sum(!is.na(frc_dd14c)),
            n_14C = sum(!is.na(frc_14c)))

key <- data.frame(frc_scheme2 = levels(frc_data_Stats$frc_scheme2), 
                  #xend manually adjusts length of horizontal lines for medians
                  xend_manual = c(250, 10, 30, 10, 30)) 

frc_data_Stats <- left_join(frc_data_Stats, key) 
levels(frc_data_Stats$frc_scheme2)
frc_data_Stats$frc_scheme2<- ordered(frc_data_Stats$frc_scheme2, c("density",
                                                           "aggregate",
                                                           "particle size",
                                                           "chemical - HS",
                                                           "chemical - other"
))

levels(frc_data_Stats$frc_property)
table(frc_data_Stats$frc_property) #equivalent to number of depths for each category

#**********GRAPHS**************####

#dd14c####

ggplot(data = frc_subset, aes(y = frc_dd14c, color = frc_property)) +
  geom_segment(data = frc_data_Stats, aes(y = median_dd14C, color = frc_property,
                                           x = -Inf, xend = xend_manual),
               linetype = "dashed", linewidth = 1) +
  geom_histogram(data = frc_subset, aes(y = frc_dd14c, fill = frc_property), bins = 50) +
  theme_bw(base_size = 16) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top") +
  scale_x_continuous("Number of samples", expand = c(0,0)) +
  scale_y_continuous(expression(paste(Delta,Delta^14, "C [‰]")), expand = c(0,0),
                     limits = c(-900,350)) +
  scale_fill_manual("fraction", values = soc_colors) +
  scale_color_manual("fraction", values = soc_colors) +
  facet_grid(depth ~ frc_scheme2, scales = "free_x" )

ggsave("./output/Figure5.jpeg", width = 12, height = 8)

