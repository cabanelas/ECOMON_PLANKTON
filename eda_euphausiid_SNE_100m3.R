################################################################################
#############      EcoMon Euphausiid DATA SNE       ############################
################################################################################
## by: Alexandra Cabanelas 
################################################################################
## created JUNE 2023; update OCT 2025
## previously EcoMon version 3.8 (~May 2023)
## now using EcoMon version 3.10

## EUPHAUSIIDS volume # per 100m3 data #######
# Purpose: Explore and visualize euphausiid abundance trends in SNE 

# Blank cells in the Data spreadsheet indicate that the plankton sample was not
# processed for either zooplankton or ichthyoplankton

# euph_10m2 is the sum of taxa codes 2000-2099 and euph1_10m2 is just 
# unidentified euphausicea (just taxa code 2000) -Harvey 

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)
library(RColorBrewer)

options(scipen = 999) 
# to make all columns viewable
#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

#  --- SOUTHERN NEW ENGLAND REGION ------- 
# created in 02_ecomon_tidy_subsets.R
# long format 
e_sne_l <- read_csv("output/EcoMon_v3_10_Euphausiid_SNE_m3_long.csv") 

# wide format
e_sne_w <- read_csv("output/EcoMon_v3_10_Euphausiid_SNE_m3_wide.csv")
str(e_sne_w) 

# taxa names
taxa_names <- read_csv("raw/EcoMon_TaxaNames_v3_10_cleaned.csv") %>%
  select(-comments)

## ------------------------------------------ ##
#            Tidy -----
## ------------------------------------------ ##
# make all colnames lowercase
colnames(e_sne_l) <- tolower(colnames(e_sne_l))
colnames(e_sne_w) <- tolower(colnames(e_sne_w))

# remove NA rows in abund values 
e_sne_l1 <- e_sne_l[!is.na(e_sne_l$ind_100m3),]
e_sne_w1 <- e_sne_w %>% drop_na(euph_100m3:nemabo_100m3)

####  --- SUBSET REMOVE LOW ABUND/RARE TAXA ------

# --- subset long df -----
eu_l <- e_sne_l1 %>%
  group_by(taxa) %>%
  # Remove always 0/not present: eupham; thypsp
  filter(sum(ind_100m3, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  # Very low abun/very rare: nemabo; nemaspp; shysac
  filter(!taxa %in% c("nemabo", "nemaspp", "shysac"))
  # stylspp; euphspp*; stylel are also very rare but keeping for now

# --- subset wide df -----
eu_w <- e_sne_w1 %>%
  select(cruise_name:volume_1m2, 
         all_of(
           e_sne_w1 %>%
             select(ends_with("_100m3")) %>%
             # remove taxa that are always 0
             summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
             select(where(~ . > 0)) %>%
             names()
         ))
# that ^ removes eupham, thypsp
setdiff( #this check which cols were removed above
  names(e_sne_w1 %>% select(ends_with("_100m3"))),
  names(e_sne_w1 %>% select(ends_with("_100m3")) %>%
          summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
          select(where(~ . > 0)))
)

## remove very low abund/very rare: nemabo; nemaspp; shysac
eu_w <- eu_w %>%
  select(-nemabo_100m3,nemaspp_100m3, shysac_100m3)
###############################################################################

## ------------------------------------------ ##
#            Plots -----
## ------------------------------------------ ##
####  --- COLORS ------ ####
mycolors = c(brewer.pal(name="Dark2", n = 8), 
             brewer.pal(name="Paired", n = 6))
####  ----------------- ####

# --- line plot by taxa; need to group by yr -----
eu_l_group <- eu_l %>%
  group_by(cruise_name, year, taxa_name) %>%
  mutate(mean_100m3 = mean(ind_100m3)) %>%
  distinct(cruise_name, year, taxa_name, .keep_all = TRUE) %>%
  ungroup() 

eu_l_mean <- eu_l_group %>%
  group_by(year, taxa_name) %>%
  mutate(mean_100m3 = mean(mean_100m3)) %>%
  distinct(year, taxa_name, .keep_all = TRUE) %>%
  ungroup() 
############################################################################## 

# --- 1) time series, one panel per spp -----
ggplot(eu_l_mean, aes(x = year, y = mean_100m3)) + 
  geom_line(linewidth = 1) +
  facet_wrap(~taxa_name, scales = "free")

############################################################################## 
# --- 2) bar plot, no euph group -----
ggplot(data=eu_l_mean %>% filter(taxa != "euph"), 
       aes(x=year, y=mean_100m3, fill = taxa_name)) +
  geom_bar(stat = "identity") +
  labs(y = expression(paste("per 100", m^{3}, "of water volume")))+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
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
############################################################################## 

############################################################################## 
# --- 3) bar plot, no euph nor euph1 group  -----
ggplot(data=eu_l_mean %>% filter(taxa != "euph" & taxa != "euph1"), 
       aes(x=year, y=mean_100m3, fill = taxa_name)) + 
  geom_bar(stat = "identity") +
  labs(y = expression(paste("per 100", m^{3}, "of water volume")))+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
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
############################################################################## 

############################################################################## 
# --- 4) relative abund bar plot, no euph nor euph1 group  -----
ggplot(data=eu_l_mean %>% filter(taxa != "euph" & taxa != "euph1"), 
       aes(x=year, y=mean_100m3, fill = taxa_name)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
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
############################################################################## 

############################################################################## 
# --- 5) bar plot, euph group ONLY  -----
ggplot(data=eu_l_mean %>% filter(taxa == "euph"), 
       aes(x=year, y=mean_100m3, fill = taxa_name)) +
  geom_bar(stat = "identity") +
  labs(y = expression(paste("per 100", m^{3}, "of water volume")))+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = c(0.2,0.95), 
        legend.title = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
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
############################################################################## 

############################################################################## 
# --- grouping some taxa  -----
## combine Euphausia krohnii & euphausia spp
## combine Stylocheiron (Stylspp and Stylel)
## combine thysanoessa
## leave M norvegica & N Megalops as is
eu_w_subset <- eu_w %>%
  mutate(
    EUSPPKR = rowMeans(select(., euphspp_100m3, euphkr_100m3), na.rm = TRUE),
    STYLO   = rowMeans(select(., stylspp_100m3, stylel_100m3), na.rm = TRUE),
    THYSAN  = rowMeans(select(., thysin_100m3, thysra_100m3, thysgr_100m3, 
                              thysspp_100m3, thyslo_100m3), na.rm = TRUE)
  ) %>%
  #remove the orig taxa from df 
  select(-c(euphspp_100m3, euphkr_100m3, stylspp_100m3, stylel_100m3, thysin_100m3,
          thysra_100m3, thysgr_100m3, thysspp_100m3, thyslo_100m3)) %>% 
  # remove suffix from col names
  rename_with(~ gsub("_100m3", "", .x))

# long format
eu_w_subset_l <- eu_w_subset %>%
          pivot_longer(
                  cols = euph:THYSAN,
                  names_to = "taxa",
                  values_to = "ind_100m3")

# add taxa names
t_names <- taxa_names %>%
  filter(unit == "100m3") %>%
  select(taxa, TAXA_NAME)

eu_w_subset_l <- left_join(eu_w_subset_l, t_names, by = "taxa")
############################################################################## 
# --- line plot by taxa; need to group by yr -----
eu_w_subset1a <- eu_w_subset_l %>%
  group_by(cruise_name, year, TAXA_NAME) %>%
  mutate(mean_100m3 = mean(ind_100m3)) %>%
  distinct(cruise_name, year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup() 

eu_w_subset1b <- eu_w_subset1a %>%
  group_by(year, TAXA_NAME) %>%
  mutate(mean_100m3 = mean(mean_100m3)) %>%
  distinct(year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup() 
############################################################################## 

############################################################################## 
# --- 6) bar plot, grouped taxa, no euph group  -----
ggplot(data=eu_w_subset1b %>% filter(taxa != "euph"), 
       aes(x=year, y=mean_100m3, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  labs(y = expression(paste("per 100", m^{3}, "of water volume")))+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
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
############################################################################## 

############################################################################## 
# --- 7) relative abund bar plot, grouped taxa, no euph group  -----
ggplot(data=eu_w_subset1b %>% filter(taxa != "euph"), 
       aes(x=year, y=mean_100m3, fill = TAXA_NAME)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = expression(paste("per 100", m^{3}, "of water volume")))+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
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
############################################################################## 

############################################################################## 
# --- 8) relative abund bar plot, grouped taxa, no euph nor euph 1 group  -----
ggplot(data=eu_w_subset1b %>% filter(taxa != "euph" & taxa != "euph1"), 
       aes(x=year, y=mean_100m3, fill = TAXA_NAME)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative abundance (%)")+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
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
############################################################################## 

############################################################################## 
# --- averaging differently, by strata type -----
eu_l_TYPE <- eu_l %>%
  group_by(cruise_name, type, year, taxa_name) %>%
  mutate(mean_100m3 = mean(ind_100m3)) %>%
  distinct(cruise_name, type, year, taxa_name, .keep_all = TRUE) %>%
  ungroup() 

eu_l_TYPE1 <- eu_l_TYPE %>%
  group_by(type, year, taxa_name) %>%
  mutate(mean_100m3 = mean(mean_100m3)) %>%
  distinct(type, year, taxa_name, .keep_all = TRUE) %>%
  ungroup() 

## IS = inner shelf, MS = midshelf, SB = shelf-break, C = coast, OFF = offshelf
types <- c('C' = "Coastal", 'IS' = "Inner Shelf", 'MS' = "Mid Shelf",
           'OFF' = "Off Shelf", 'SB' = "Shelf Break")
############################################################################## 

############################################################################## 
# --- 9) bar plot by strata type, no euph nor euph 1 group  -----
ggplot(data=eu_l_TYPE1 %>% filter(taxa != "euph" & taxa != "euph1",
                                  type != "C" & type != "OFF"),
       aes(x=year, y=mean_100m3, fill = taxa_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~type, scales = "free_y",
             labeller = as_labeller(types)) +
  labs(y = expression(paste("per 100", m^{3}, "of water volume")))+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
# --- 10) relative abund bar plot by strata type, no euph nor euph 1 group  -----
ggplot(data=eu_l_TYPE1 %>% filter(taxa != "euph" & taxa != "euph1",
                                  type != "C" & type != "OFF"),
       aes(x=year, y=mean_100m3, fill = taxa_name)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~type, scales = "free_y",
             labeller = as_labeller(types)) +
  labs(y = "Relative abundance (%)") +
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
# --- 11) bar plot by strata type, no euph  -----
ggplot(data=eu_l_TYPE1 %>% filter(taxa != "euph"),
       aes(x=year, y=mean_100m3, fill = taxa_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~type, scales = "free_y",
             labeller = as_labeller(types)) +
  labs(y = expression(paste("per 100", m^{3}, "of water volume")))+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        #strip.text.x = element_text(size=18), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
# --- 12) relative abund bar plot by strata type, no euph  -----
ggplot(data=eu_l_TYPE1 %>% filter(taxa != "euph"),
       aes(x=year, y=mean_100m3, fill = taxa_name)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~type, scales = "free_y",
             labeller = as_labeller(types)) +
  labs(y = "Relative Abundance (%)")+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = c(0.87,0.2), 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=15), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
# --- 13) bar plot by strata type by taxa, legend type  -----
ggplot(data=eu_l_TYPE1, aes(x=year, y=mean_100m3, fill = type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~taxa_name, scales = "free_y")+
  labs(y = expression(paste("per 100", m^{3}, "of water volume"))) +
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
# --- 14) relative abund bar plot by strata type by taxa, legend type  -----
ggplot(data=eu_l_TYPE1, aes(x=year, y=mean_100m3, fill = type)) + 
  geom_bar(stat = "identity", positio = "fill") +
  facet_wrap(~taxa_name, scales = "free_y")+
  labs(y = "Relative Abundance (%)") +
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
# --- line plot of euphausiid group that includes 18 taxa -----
manyeuphausiids <- eu_l_mean %>% 
  filter(taxa != "euph") #euph_10m# is the sum of taxa codes 2000-2099

manyeuphausiids <- aggregate(mean_100m3 ~ year, 
                             data = manyeuphausiids, 
                             FUN = "max")

ggplot(manyeuphausiids, aes(x = year, y = mean_100m3)) + 
  geom_line(color = "blue", linewidth = 1) + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  ggtitle("Euphausiids in SNE")
############################################################################## 

############################################################################## 
# --- line plot without big euphausiid group -----
eu_l_mean2 <- eu_l_mean %>%
  filter(taxa != "euph") #sum of taxa codes 2000-2099 

ggplot(data=eu_l_mean2, aes(x=year, y=mean_100m3, color=taxa_name)) + 
  geom_line() + 
  facet_wrap(~taxa_name, scales = "free")
############################################################################## 

######### STOPPED FIXING STUFF HERE....
############################################################################## 
## euph, euphkr, euphspp
euphausi_g <- eu_l_mean %>%
  filter(taxa == "euph" | taxa == "euphkr" | taxa == "euphspp")

euphausi_g <- euphausi_g %>%
  mutate(date1=as.Date(date, format = "%d-%b-%y"))

euphausi <- euphausi_g %>%
  group_by(date1, taxa) %>%
  mutate(mean_100m3 = mean(mean_100m3)) %>%
  distinct(date1, taxa, .keep_all = TRUE) %>%
  ungroup()

ggplot(euphausi, aes(x = date1, y = mean_100m3, color = taxa)) + 
  geom_line() +
  facet_wrap(~taxa_name, scales = "free_y")

## i did the same thing by averaging by cruise_name, year, taxa and then year
# and taxa and gives same result at least for line plots 

euphausi2 <- euphausi_g %>%
  group_by(cruise_name, year, taxa) %>%
  mutate(mean_100m3 = mean(ind_100m3)) %>%
  distinct(cruise_name, year, taxa, .keep_all = TRUE) %>%
  ungroup()

euphausi2 <- euphausi2 %>%
  group_by(year, taxa) %>%
  mutate(mean_100m3 = mean(mean_100m3)) %>%
  distinct(year, taxa, .keep_all = TRUE) %>%
  ungroup()

ggplot(euphausi2, aes(x = year, y = mean_100m3, color = taxa)) + 
  geom_line() 

ggplot(euphausi2, aes(x = date1, y = mean_100m3, color = taxa)) + 
  geom_line() +
  facet_wrap(~TAXA_NAME, scales = "free_y")


############################################################################## 
## right now im looking at 12 taxa
euph <- eu_l_mean %>%
  filter(taxa == "euph")
thysin <- eu_l_mean %>%
  filter(taxa == "thysin")
megan <- eu_l_mean %>%
  filter(taxa == "megan")
thysra <- eu_l_mean %>%
  filter(taxa == "thysra")
thyslo <- eu_l_mean %>%
  filter(taxa == "thyslo")
euphkr <- eu_l_mean %>%
  filter(taxa == "euphkr")
euphspp <- eu_l_mean %>%
  filter(taxa == "euphspp")
thysgr <- eu_l_mean %>%
  filter(taxa == "thysgr")
stylel <- eu_l_mean %>%
  filter(taxa == "stylel")
nemame <- eu_l_mean %>%
  filter(taxa == "nemame")
thysspp <- eu_l_mean %>%
  filter(taxa == "thysspp")
############################################################################## 
############################################################################## 
## i skipped a bunch of plots so can look back to the original
#ecomon_euphaus_100m3 script 

##############################################################################    
### MEGAN
############################################################################## 
boxplot(megan$mean_100m3)

ggplot(megan, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(megan$TAXA_NAME)
############################################################################## 

############################################################################## 
### THYSIN
############################################################################## 
boxplot(thysin$mean_100m3)

ggplot(thysin, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(thysin$TAXA_NAME)
############################################################################## 

############################################################################## 
### THYSRA
############################################################################## 
boxplot(thysra$mean_100m3)

ggplot(thysra, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(thysra$TAXA_NAME)
############################################################################## 

############################################################################## 
### THYSLO
############################################################################## 
boxplot(thyslo$mean_100m3)

ggplot(thyslo, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(thyslo$TAXA_NAME)
############################################################################## 

############################################################################## 
### EUPHKR
############################################################################## 
boxplot(euphkr$mean_100m3)

ggplot(euphkr, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(euphkr$TAXA_NAME)
############################################################################## 


############################################################################## 
### EUPHSPP
############################################################################## 
boxplot(euphspp$mean_100m3)

ggplot(euphspp, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(euphspp$TAXA_NAME)
############################################################################## 

############################################################################## 
### THYSGR
############################################################################## 
boxplot(thysgr$mean_100m3)

ggplot(thysgr, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(thysgr$TAXA_NAME)
############################################################################## 

############################################################################## 
### STYLEL
############################################################################## 
boxplot(stylel$mean_100m3)

ggplot(stylel, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(stylel$TAXA_NAME)
############################################################################## 


############################################################################## 
### NEMAME
############################################################################## 
boxplot(nemame$mean_100m3)

ggplot(nemame, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(nemame$TAXA_NAME)
############################################################################## 


############################################################################## 
### THYSSPP
############################################################################## 
boxplot(thysspp$mean_100m3)

ggplot(thysspp, aes(x = year, y = mean_100m3)) + 
  geom_line() +
  #facet_wrap(~season) +
  theme_bw() + 
  ggtitle(thysspp$TAXA_NAME)
############################################################################## 


##############################################################################
################          SELECTING THE TOP TAXA      ######################## 
############################################################################## 
eu_taxapick <- eu_l_mean %>%
  filter(taxa == "thysin"| taxa == "megan"| taxa == "thysra"| taxa == "thyslo" |
           taxa == "euphkr"| taxa == "thysgr"| taxa == "stylel"| taxa == "nemame"|
           taxa == "thysspp")
############################################################################## 
ggplot(eu_taxapick, aes(x = year, y = mean_100m3, color = taxa)) + 
  geom_line() +
  theme_bw()
############################################################################## 
ggplot(eu_taxapick, aes(x = year, y = mean_100m3,  fill = TAXA_NAME)) + 
  geom_bar(stat = "identity" , position = "fill") +
  scale_fill_manual(values = mycolors) +
  labs(y = "Relative Abundance (%)") +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
## thy spp
thyspp <- eu_l_mean %>%
  filter(taxa == "thysin" | taxa == "thysra" | taxa == "thyslo" |
           taxa == "thysgr" 
         #| taxa == "thysspp"
  )
ggplot(thyspp, aes(x = year, y = mean_100m3, color = TAXA_NAME)) + 
  geom_line() +
  theme_bw()
############################################################################## 




##############################################################################
########################        SEASONAL TRENDS      ######################## 
############################################################################## 
eu_sea <- eu_l %>%
  group_by(cruise_name, season, year, TAXA_NAME) %>%
  mutate(mean_100m3 = mean(ind_100m3)) %>%
  distinct(cruise_name, season, year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup() 

eu_sea_m <- eu_sea %>%
  group_by(season, year, TAXA_NAME) %>%
  mutate(mean_100m3 = mean(mean_100m3)) %>%
  distinct(season, year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup() 
############################################################################## 

############################################################################## 
ggplot(data=eu_sea_m %>% filter(taxa != "euph" & taxa != "euph1"),
       aes(x=year, y=mean_100m3, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  facet_wrap(~season, scales = "free_y") +
  labs(y = expression(paste("per 100", m^{3}, "of water volume")))+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
ggplot(data=eu_sea_m %>% filter(taxa != "euph" & taxa != "euph1"),
       aes(x=year, y=mean_100m3, fill = TAXA_NAME)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~season) +
  labs(y = "Relative Abundance (%)")+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 

############################################################################## 
ggplot(data=eu_sea_m %>% filter(taxa != "euph"),
       aes(x=year, y=mean_100m3, fill = TAXA_NAME)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~season) +
  labs(y = "Relative Abundance (%)")+
  scale_fill_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
############################################################################## 


############################################################################## 
ggplot(data=eu_sea_m %>% filter(taxa != "euph" & taxa != "euph1"),
       aes(x=year, y=mean_100m3, color = TAXA_NAME)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~season, scales = "free") +
  labs(y = expression(paste("per 100", m^{3}, "of water volume"))) +
  scale_color_manual(values = mycolors) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        #legend.text = element_text(size = 20, face = "bold"),
        #legend.background = element_rect(fill = "white", color = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    face = "bold"),
        axis.text.x = element_text(color = "black", 
                                   size = 15, angle = 45, hjust = 1,
                                   vjust = 1),
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        strip.text.x = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) 
############################################################################## 



############################################################################## 
############################################################################## 
#####################  -----        LOOPS       -------      ################# 
############################################################################## 
############################################################################## 
Sp <- unique(eu_sea_m$TAXA_NAME) #create list of species to loop over

TAXA_Dens <- list()


for(Sp_ in Sp) {
  TAXA_Dens[[Sp_]] = ggplot(eu_sea_m %>% filter(TAXA_NAME == Sp_), 
                            aes(x=year, y=mean_100m3)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~season) +
    labs(y = expression(paste("per 100", m^{3}, "of water volume"))) +
    theme_bw() + 
    theme(
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16, 
                                      face = "bold"),
          axis.text.x = element_text(color = "black", 
                                     size = 15, angle = 45, hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(color = "black", 
                                     size = 14),
          strip.text.x = element_text(size=12), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.length = unit(0.2,"cm")) +
    scale_y_continuous(expand = expansion(mult = c(0,0.01))) + 
    ggtitle(paste0(Sp_))
  print(TAXA_Dens[[Sp_]])
  ggsave(TAXA_Dens[[Sp_]], file=paste0("Dens_hist_", Sp_, ".png"))
}

############################################################################## 

for(Sp_ in Sp) {
  TAXA_Dens[[Sp_]] = ggplot(eu_sea_m %>% filter(TAXA_NAME == Sp_), 
                            aes(x=year, y=mean_100m3)) +
    geom_line() + 
    facet_wrap(~season) +
    labs(y = expression(paste("per 100", m^{3}, "of water volume"))) +
    theme_bw() + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 16, 
                                  face = "bold"),
      axis.text.x = element_text(color = "black", 
                                 size = 15, angle = 45, hjust = 1,
                                 vjust = 1),
      axis.text.y = element_text(color = "black", 
                                 size = 14),
      strip.text.x = element_text(size=12), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(0.2,"cm")) + 
    ggtitle(paste0(Sp_))
  print(TAXA_Dens[[Sp_]])
  ggsave(TAXA_Dens[[Sp_]], file=paste0("SNE_Season_Line_", Sp_, ".png"))
}

############################################################################## 
# same as previous but free y 
for(Sp_ in Sp) {
  TAXA_Dens[[Sp_]] = ggplot(eu_sea_m %>% filter(TAXA_NAME == Sp_), 
                            aes(x=year, y=mean_100m3)) +
    geom_line() + 
    facet_wrap(~season, scales = "free_y") +
    labs(y = expression(paste("per 100", m^{3}, "of water volume"))) +
    theme_bw() + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 16, 
                                  face = "bold"),
      axis.text.x = element_text(color = "black", 
                                 size = 15, angle = 45, hjust = 1,
                                 vjust = 1),
      axis.text.y = element_text(color = "black", 
                                 size = 14),
      strip.text.x = element_text(size=12), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(0.2,"cm")) + 
    ggtitle(paste0(Sp_))
  print(TAXA_Dens[[Sp_]])
  ggsave(TAXA_Dens[[Sp_]], file=paste0("SNE_Season_Line_", Sp_, "FREE_Y.png"))
}


## need to go back to other script to check rest
## line 894 
















##### NEED TO TIDY THIS UP 

## Data 
abun <- read_csv("output/EcoMon_Plankton_v3_10_abund100m3_long.csv") #long
#abun2test <- readRDS("output/test.rds")

#abun_w <- read.csv("output/EcoMon_Plank_Abu_m3_wide.csv", header = TRUE) #wide
abun_w <- read_csv("raw/EcoMon_Plankton_Data_v3_10_wStrataMeta.csv")


# to make all columns viewable
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

#to see all columns; diff format; just cant know column index/position
utils::View(abun)
colnames(abun)

colnames(abun) <- tolower(colnames(abun))
## Data summary
str(abun)
abun %>% distinct(cruise_name) #360 cruises
abun %>% distinct(region)
abun %>% distinct(type)
abun %>% distinct(taxa)
abun %>% distinct(taxa_name)
abun %>% distinct(year)

library(dlookr)
describe(abun_w) %>% print(n = 110)

#test of normality on numeric variables 
normality(abun) #Shapiro-Wilk normality test
normality(abun_w, euph_100m3)
normality(abun_w, ctyp_100m3:euph_100m3)
##

#sort variables that do not follow a normal distribution in order of p_value
abun %>%
  normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

abun %>%
  group_by(region) %>%
  normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

abun %>%
  group_by(region) %>%
  plot_normality(euph_100m3)
##

MAB <- abun %>%
  filter(region == "MAB")

SNE <- abun %>%
  filter(region == "SNE")

SNE_w <- abun_w %>%
  filter(region == "SNE")

## SUBSET TO EUPHAUSIIDS
# 18 euphausiid groups; 17 since euphasiacea is repeated 
euph <- SNE %>%
  filter(TAXA_NAME %in% c("Euphausiacea", "EUPHAUSIACEA", "THYSANOESSA INERMIS", 
                          "MEGANYCTIPHANES NORVEGICA", "THYSANOESSA RASCHII",
                          "THYSANOESSA LONGICAUDATA", "EUPHAUSIA AMERICANA", 
                          "EUPHAUSIA KROHNII", "EUPHAUSIA SPP.", 
                          "THYSANOESSA GREGARIA", "NEMATOSCELIS SPP.", 
                          "STYLOCHEIRON SPP.", "STYLOCHEIRON ELONGATUM", 
                          "NEMATOSCELIS MEGALOPS", "THYSANOESSA SPP.",
                          "THYSANOPODA ACUTIFRONS", "THYPANVESSA SPINIFERA", 
                          "NEMATOBRACHION BOOPIS"))

euph %>% distinct(TAXA_NAME)

euph_w <- SNE_w[, c(1:19,44, 70:86)]

##
euph %>% 
  correlate() %>% 
  plot()

euph_w %>% 
  correlate() %>% 
  plot()
## 
arrange(euph, desc(ind_100m3), group_by = TAXA_NAME)

top3 <- euph %>%
  group_by(TAXA_NAME, yr) %>%
  top_n(3, ind_100m3) %>%
  arrange(ind_100m3, .by_group = TRUE)


# comparing euphausiid columns to see where they differ 
check <- euph_w[, c(1,11,20,21)]
identical(check[['euph_100m3']],check[['euph1_100m3']])
all(check$euph_100m3 == check$euph1_100m3)
check$V3 <- check$euph_100m3 - check$euph1_100m3
which(check$V3 > 0)
checkdiff <- check %>%
  filter(V3>0)
checkdiff %>% distinct(yr)

euph_w %>% distinct(yr)

unique(euph_w$yr)
select(euph_w, cruise_name) %>% unique %>% nrow




## ------------------------------------------ ##
#            EDA -----
## ------------------------------------------ ##

library(dlookr)

e_sne_w %>% 
  correlate() %>% 
  plot()
# helps see how certain taxa are never present + relationship between
# the two euph groups 

#count total missing values in each column
sapply(e_sne_l, function(x) sum(is.na(x)))

#count NA values by column
colSums(is.na(e_sne_l))
colSums(is.na(e_sne_w))

plot_normality(eu_w, euph_100m3:thysspp_100m3)

eu_w %>%
  group_by(type) %>%
  correlate() %>%
  plot() 


categ <- target_by(eu_l, taxa)
cat_num <- relate(categ, ind_100m3)
summary(cat_num)

fivenum(eu_w$megan_100m3)
summary(eu_w$megan_100m3)
boxplot(eu_w$megan_100m3, col = "blue")
hist(eu_w$megan_100m3, col = "pink")
rug(eu_w$megan_100m3)
boxplot(megan_100m3 ~ type, data = eu_w, col = "red")
boxplot(megan_100m3 ~ year, data = eu_w, col = "red")


OutVals = boxplot(euph$ind_100m3)$out
which(euph$ind_100m3 %in% OutVals)
summary(euph$ind_100m3)[["1st Qu."]]

nozero_eup <- euph %>% filter(ind_100m3 > 0)
summary(nozero_eup$ind_100m3)[["1st Qu."]]
boxplot(nozero_eup$ind_100m3)

hist(euph_season$ind_100m3,
     xlab = "ind_100m3",
     main = "Histogram of euph ind_100m3",
     breaks = sqrt(nrow(euph_season))) # set number of bins
# number of bins corresponding to the square root of the number of 
# observations in order to have more bins than the default option

library("GGally")  
egally <- eu_w[, c(13,15:34)]
ggpairs(egally)

library(psych)
pairs.panels(df)

library(psych)
corPlot(df)

library(corrgram)
corrgram(df)


zp_l_SNE %>% glimpse()
zp_l_SNE %>% skimr::skim()




#https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html
library(DataExplorer)


plot_str(zp_l_SNE)

introduce(zp_l_SNE)
plot_intro(zp_l_SNE)
plot_missing(zp_l_SNE)
sum(is.na(zp_l_SNE$TAXA_NAME))
create_report(zp_l_SNE)

plot_bar(zp_l_SNE)
plot_bar(zp_l_SNE, by = "taxa")
plot_histogram(zp_l_SNE, ggtheme = theme_tq())
plot_density(zp_l_SNE)
plot_qq(zp_l_SNE) #can also do the "by" argument 
plot_correlation(zp_l_SNE)
plot_boxplot(zp_l_SNE, by = "taxa")
plot_scatterplot(split_columns(zp_l_SNE)$ind_10m2, by = "taxa", 
                 sampled_rows = 1000L)



################################################
################################################
########              GGally             #######
################################################
################################################
library(GGally) 

# change plot size (optional)
options(repr.plot.width = 20, repr.plot.height = 10)

zp_l_SNE %>% 
  select("type", "station", "month", "year", "depth", "ind_10m2") %>%
  ggpairs(mapping = aes(color = zp_l_SNE$taxa, alpha = 0.5))


################################################
################################################
########            SmartEDA             #######
################################################
################################################
library(SmartEDA)

ExpReport(zp_l_SNE,
          Target = "ind_10m2",
          op_file = "SNE_taxa_report.html",
          op_dir = getwd())

ExpData(zp_l_SNE, type = 1) #displays overview of dataset
ExpData(zp_l_SNE, type = 2) #shows data structure
ExpCatStat(zp_l_SNE, Target="cardio", Pclass="1", plot=TRUE)



################################################
################################################
########            TableONE             #######
################################################
################################################
library(tableone)

#https://towardsdatascience.com/four-r-packages-for-automated-exploratory-data-analysis-you-might-have-missed-c38b03d4ee16


#delete really low abund taxa
# nemabo - only reported once SNE 1987
# thypsp - only reported once GOM 1986
# shysac - only reported thrice (3x) SNE 77; GB 78; MAB 86
# stylel - only reported thrice (3x) GOM 77; SNE 78 + 78
# nemaspp - only recorded twice GB 91; SNE 79
# eupham - only recorded once GB 2005 