################################################################################
#############             EcoMon ZP DATA           #############################
################################################################################
#### EUPHAUSIIDS

## EUPHAUSIIDS areal # per 10m2 data #######
# Purpose: Explore and visualize euphausiid abundance trends in NES (all regions) 

# Blank cells in the Data spreadsheet indicate that the plankton sample was not
# processed for either zooplankton or ichthyoplankton

# euph_10m2 is the sum of taxa codes 2000-2099 and euph1_10m2 is just 
# unidentified euphausicea (just taxa code 2000) -Harvey 

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)
library(RColorBrewer)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
zp <- read_csv("raw/EcoMon_Plankton_Data_v3_10_wStrataMeta.csv")

## ------------------------------------------ ##
#            Tidy -----
## ------------------------------------------ ##

zp1 <- zp %>% 
  # remove extra regions
  filter(!Region %in% c("CC","NS")) %>%
  # keep only _10m2 data
  select(
    cruise_name:volume_1m2,                     
    matches("^.*_10m2$") 
  ) %>%
  # keep only euphausiids
  select(
    cruise_name:volume_1m2,                      
    euph_10m2,
    euph1_10m2:nemabo_10m2                      
  ) %>%
  rename_with(~ gsub("_10m2", "", .x), matches("_10m2$"))

# change data from wide to long
zp_long <- zp1 %>%
  pivot_longer(cols = c(euph:nemabo),
               names_to = "taxa", 
               values_to = "abundance")

eu_l_group <- zp_long %>%
  group_by(cruise_name, year, taxa, Region, season) %>%
  mutate(mean_10m2 = mean(abundance, na.rm = T)) %>%
  distinct(cruise_name, year, taxa, Region, season, .keep_all = TRUE) %>%
  ungroup() 

eu_l_mean <- eu_l_group %>%
  group_by(year, taxa, Region, season) %>%
  mutate(mean_10m2 = mean(mean_10m2, na.rm = T)) %>%
  distinct(year, taxa, Region, season,.keep_all = TRUE) %>%
  ungroup() 

## ------------------------------------------ ##
#            Plots -----
## ------------------------------------------ ##
####  --- COLORS ------ ####
mycolors = c(brewer.pal(name="Dark2", n = 8), 
             brewer.pal(name="Paired", n = 11))
####  ----------------- ####

ggplot(data=eu_l_mean[!eu_l_mean$taxa == "euph1", ], 
       aes(x=year, y=mean_10m2, color = taxa)) + 
  geom_line() + 
  facet_grid(season~Region, scales = "free") +
  labs(y = "10m2")

ggplot(data=eu_l_mean %>% filter(taxa != "euph" & taxa != "euph1"), 
       aes(x=year, y=mean_10m2, fill = taxa)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "RelatieAbundance (%)")+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  facet_grid(season~Region) +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))


# fewer taxa
eu_l_mean_sub <- eu_l_mean %>% 
  filter(taxa %in% c("euphkr", "megan","thysgr") #taxa == "thyslo"
  )

ggplot(data=eu_l_mean_sub %>% filter(taxa != "euph" & taxa != "euph1"), 
       aes(x=year, y=mean_10m2, fill = taxa)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "RelatieAbundance (%)")+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  facet_grid(season~Region) +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))

ggplot(data=eu_l_mean_sub, 
       aes(x=year, y=mean_10m2, color = taxa)) + 
  geom_line() +
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  facet_grid(season~Region, scales = "free")