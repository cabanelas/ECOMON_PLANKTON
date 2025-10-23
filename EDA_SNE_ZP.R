################################################################################
#############             EcoMon ZP DATA EDA  SNE  #############################
################################################################################
## by: Alexandra Cabanelas 
################################################################################
## created JUNE 2023; update OCT 2025
## previously EcoMon version 3.8 (~May 2023)
## now using EcoMon version 3.10

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)
#library(DataExplorer)
library(RColorBrewer)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
#zpold <- read.csv("raw/EcoMon_v3_8_wDateStrata_Isabel.csv")
#zp1old <- read.csv("output/older_EcoMon_Data_versions/EcoMon_Plank_Abu_m2_wide.csv")
# the main difference between Isabel's and my region csv
# is that mine includes some of the offshore/out of system
# ones so nearby # can easily remove based on Name (> #s)

zp <- read_csv("raw/EcoMon_Plankton_Data_v3_10_wStrataMeta.csv") %>%
  filter(Region == "SNE" &
         Type != "OFF") %>%
  # keep only metadata and _10m2 data
  select(
    cruise_name:volume_1m2, 
    matches("^.*_10m2$") 
  ) %>%
  # remove the "_10m2" suffix
  rename_with(~ gsub("_10m2", "", .x), matches("_10m2$"))

taxaName <- read_csv("raw/EcoMon_TaxaNames_v3_10_cleaned.csv") %>%
  filter(unit == "10m2") %>%
  select(taxa, TAXA_NAME, group) 

copepod_taxa <- c(
  "ctyp", "calfin", "pseudo", "tlong", "cham", "para", "acarspp",
  "mlucens", "calminor", "copepoda", "clauso", "acarlong", "euc",
  "fur", "calspp", "oncaea", "cory", "tstyl", "oithspp", "oithspin",
  "temspp", "tort", "paraspp"
)

## ------------------------------------------ ##
#            Tidy -----
## ------------------------------------------ ##

zp_o <- zp %>%
  # keep only _10m2 data
  #select(
  #  -(cruise_name:volume_1m2) 
  #) %>%
  #rename_with(~ gsub("_10m2", "", .x), matches("_10m2$")) %>%
  pivot_longer(
    cols = ctyp:pnepau, #lopame if including fish; pnepau for zp only
    names_to = "taxa",
    values_to = "ind_10m2") %>%
  left_join(taxaName, by = "taxa") #this removes a lot of cols i may need?

zp_o %>%
  group_by(taxa) %>%
  summarize(MeanT = mean(ind_10m2, na.rm = TRUE)) %>%
  mutate(PercentageM = MeanT / sum(MeanT) * 100) %>%
  arrange(desc(MeanT)) %>%
  print(n = 125)

#### --- COPEPODS ------ ####
cop <- zp %>%
  select(cruise_name:volume_1m2,
         all_of(copepod_taxa))

#create_report(cop)

cop_l <- cop %>%
  pivot_longer(
              cols = ctyp:paraspp,
              names_to = "taxa",
              values_to = "ind_10m2") %>%
  left_join(taxaName, by = "taxa")

cop_l %>%
  group_by(taxa) %>%
  summarize(MeanT = mean(ind_10m2, na.rm = TRUE)) %>%
  mutate(PercentageM = MeanT / sum(MeanT) * 100) %>%
  arrange(desc(MeanT)) %>%
  print(n = 25)

cop_l %>%
  group_by(taxa, year) %>%
  summarize(MeanT = mean(ind_10m2, na.rm = TRUE)) %>%
  mutate(PercentageM = MeanT / sum(MeanT) * 100) %>%
  arrange(desc(year))

cop_l_m <- cop_l %>%
  group_by(TAXA_NAME, year, season, Type) %>%
  summarize(MeanT = mean(ind_10m2, na.rm = TRUE)) %>% 
  filter(Type != "OFF")

################################################################
## ------------------------------------------ ##
#            Plots -----
## ------------------------------------------ ##
####  --- COLORS ------ ####
mycolors = c(brewer.pal(name="Dark2", n = 8), 
             brewer.pal(name="Paired", n = 6))

facet_titles <- c("C" = "Coastal", "IS" = "Inner Shelf", 
                  "MS" = "Mid-Shelf", "SB" = "Shelf Break")

options(scipen = 999)
################################################################

# --- 1) copepod line plots -----
taxas <- unique(cop_l_m$TAXA_NAME) 

cop_lineplots <- list()

for(TAXA_NAME_ in taxas) {
  cop_lineplots[[TAXA_NAME_]] = ggplot(cop_l_m %>% 
                                         filter(TAXA_NAME == TAXA_NAME_),
                               aes(x=year, y = MeanT,
                                   color = season)) +
    geom_line(linewidth = 1, alpha = 0.7) +
    ylab(expression("Ind/" ~ m^{2})) +
    scale_color_manual(values = mycolors) +
    facet_wrap(~Type, scales = "free", labeller = labeller(Type = as_labeller(facet_titles))) +
    ggtitle(paste0(" ", TAXA_NAME_)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          legend.text = element_text(size = 12), 
          plot.title = element_text(hjust = 0.5),
          strip.text = element_text(size = 14), 
          axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
          axis.text.y = element_text(color = "black"), 
          axis.title.x = element_blank()) +
    scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
  print(cop_lineplots[[TAXA_NAME_]])
  #ggsave(cop_lineplots[[TAXA_NAME_]], file=paste0("plot_", TAXA_NAME_, ".png"))
}
################################################################

################################################################
#picking top 9
cop_l_top <- cop_l %>%
  filter(taxa == "ctyp" | taxa== "calfin" | taxa == "pseudo" |
         taxa == "tlong" | taxa == "para" | taxa == "mlucens" |
         taxa == "oithspp" | taxa == "acarspp"  | taxa == "cham")

ggplot(cop_l_top, aes(year, ind_10m2, fill = taxa, color = taxa)) +
  geom_col(position = "fill") +
  facet_wrap(~Type)

ggplot(cop_l_top, aes(year, ind_10m2, fill = taxa, color = taxa)) +
  geom_col(position = "fill") +
  facet_wrap(season~Type)

cop_l_top_g <- cop_l_top %>%
  group_by(taxa, year, Type) %>%
  summarize(MeanT = mean(ind_10m2, na.rm = TRUE))

ggplot(cop_l_top_g, aes(year, MeanT, fill = taxa, 
                        group = taxa, color = taxa)) +
  geom_line(size = 1.2) +
  facet_wrap(~Type) +
  scale_color_manual(values = mycolors)

cop_l_top_g_s <- cop_l_top %>%
  group_by(taxa, year, Type, season) %>%
  summarize(MeanT = mean(ind_10m2, na.rm = TRUE))

ggplot(cop_l_top_g_s, aes(year, MeanT, fill = taxa, 
                        group = taxa, color = taxa)) +
  geom_line(size = 1.2) +
  facet_wrap(season~Type, scales = "free") +
  scale_color_manual(values = mycolors)
################################################################

# --- 2) relative abund bar plot of top copepods by type -----
ggplot(cop_l_top, aes(year, ind_10m2, fill = taxa, color = taxa)) +
  geom_col(position = "fill") +
  facet_wrap(~Type, labeller = labeller(Type = as_labeller(facet_titles))) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  ylab(expression("Relative Abundance")) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,
                                    color = "black"),
        axis.text.x = element_text(color = "black", 
                                   size = 12, 
                                   angle = 45, hjust = 1
        ),
        strip.text = element_text(color = "black", 
                                  size = 12,
                                  face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) + 
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), labels=scales::percent)
################################################################

# --- 3) relative abund bar plot of top copepods by season -----
ggplot(cop_l_top, aes(year, ind_10m2, fill = taxa, color = taxa)) +
  geom_col(position = "fill") +
  facet_wrap(~season) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  ylab(expression("Relative Abundance")) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,
                                    color = "black"),
        axis.text.x = element_text(color = "black", 
                                   size = 12, 
                                   angle = 45, hjust = 1
        ),
        strip.text = element_text(color = "black", 
                                  size = 12,
                                  face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) + 
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), labels=scales::percent)
################################################################

# --- 4) relative abund bar plot of top copepods type and season -----
ggplot(cop_l_top, aes(year, ind_10m2, fill = TAXA_NAME, color = TAXA_NAME)) +
  geom_col(position = "fill") +
  facet_grid(season~Type, labeller = labeller(Type = as_labeller(facet_titles))) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  ylab(expression("Relative Abundance")) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,
                                    color = "black"),
        axis.text.x = element_text(color = "black", 
                                   size = 12, 
                                   angle = 45, hjust = 1
        ),
        strip.text = element_text(color = "black", 
                                  size = 12,
                                  face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) + 
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), labels=scales::percent)
################################################################

# --- 5) relative abund bar plot of top copepods season and type -----
ggplot(cop_l_top, aes(year, ind_10m2, fill = TAXA_NAME, color = TAXA_NAME)) +
  geom_col(position = "fill") +
  facet_grid(Type~season, labeller = labeller(Type = as_labeller(facet_titles))) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  ylab(expression("Relative Abundance")) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,
                                    color = "black"),
        axis.text.x = element_text(color = "black", 
                                   size = 12, 
                                   angle = 35, hjust = 1
        ),
        strip.text = element_text(color = "black", 
                                  size = 12,
                                  face = "bold"),
        #axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) + 
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), labels=scales::percent)


################################################################

# --- 6) relative abund bar plot small vs large copepods  -----
cop_l_top <- cop_l_top %>%
  mutate(size = case_when(TAXA_NAME == "Calanus finmarchicus" ~ "Large",
                          TAXA_NAME == "Centropages hamatus" ~ "Small",
                          TAXA_NAME == "Centropages typicus" ~ "Small",
                          #Taxon == "Clausocalanus arcuicornis" ~ "Small",
                          TAXA_NAME == "Metridia lucens" ~ "Small",
                          #Taxon == "Nannocalanus minor" ~ "Small",
                          TAXA_NAME == "Paracalanus parvus" ~ "Small",
                          TAXA_NAME == "Pseudocalanus spp." ~ "Small",
                          TAXA_NAME == "Temora longicornis" ~ "Small",
                          TAXA_NAME == "Acartia spp." ~ "Small",
                          TAXA_NAME == "Oithona spp." ~ "Small"))


ggplot(cop_l_top, aes(year, ind_10m2, fill = size, color = size)) +
  geom_col(position = "fill") +
  facet_grid(Type~season, labeller = labeller(Type = as_labeller(facet_titles))) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  ylab(expression("Relative Abundance")) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,
                                    color = "black"),
        axis.text.x = element_text(color = "black", 
                                   size = 12, 
                                   angle = 35, hjust = 1
        ),
        strip.text = element_text(color = "black", 
                                  size = 12,
                                  face = "bold"),
        #axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) + 
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), labels=scales::percent)
################################################################

# --- 7) relative abund bar plot small vs large copepods season  -----
ggplot(cop_l_top, aes(year, ind_10m2, fill = size, color = size)) +
  geom_col(position = "fill") +
  facet_grid(~season) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  ylab(expression("Relative Abundance")) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,
                                    color = "black"),
        axis.text.x = element_text(color = "black", 
                                   size = 12, 
                                   angle = 35, hjust = 1
        ),
        strip.text = element_text(color = "black", 
                                  size = 12,
                                  face = "bold"),
        #axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) + 
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), labels=scales::percent)
################################################################

################################################################
# top 4 
cop_l_top4 <- cop_l %>%
  filter(taxa == "ctyp" | taxa== "calfin" | taxa == "pseudo" |
         taxa == "tlong")

################################################################
# top 20 overall all spp
zp_o_top20 <- zp_o %>%
  filter(taxa == "ctyp" | taxa == "calfin" | taxa == "pseudo" |
         taxa == "echino" | taxa == "penilia" | taxa == "tlong" |
         taxa == "gas" | taxa == "larvaceans" | taxa == "para" |
         taxa == "mlucens" | taxa == "chaeto" | taxa == "salps" |
         taxa == "spirspp" | taxa == "oithspp" | taxa == "thecos" |
         taxa == "evadnespp" | taxa == "cirr" | taxa == "acarspp" |
         taxa == "cham" | taxa == "hyper")

# --- 8) scatter plot by type color by season -----
taxas <- unique(zp_o_top20$TAXA_NAME) 

cop_scatter <- list()

for(TAXA_NAME_ in taxas) {
  cop_scatter[[TAXA_NAME_]] = ggplot(zp_o_top20 %>% filter(TAXA_NAME == TAXA_NAME_),
                                       aes(x=year, y = ind_10m2,
                                           color = season)) +
    geom_point(size = 1.8, alpha = 0.7) +
    ylab(expression("Ind/" ~ m^{2})) +
    scale_color_manual(values = mycolors) +
    facet_wrap(~Type, scales = "free", labeller = labeller(Type = as_labeller(facet_titles))) +
    ggtitle(paste0(" ", TAXA_NAME_)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          legend.text = element_text(size = 12), 
          plot.title = element_text(hjust = 0.5),
          strip.text = element_text(size = 14), 
          axis.text.x = element_text(color = "black",
                                     size = 15),
          axis.text.y = element_text(color = "black", size = 14), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = "black", face = "bold", 
                                      size = 12)) +
    scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
  print(cop_scatter[[TAXA_NAME_]])
  #ggsave(cop_scatter[[TAXA_NAME_]], file=paste0("plot_", TAXA_NAME_, ".png"))
}
################################################################

################################################################
zp_o_top20a <- zp_o_top20 %>%
  group_by(TAXA_NAME, year, season, Type) %>%
  summarize(MeanT = mean(ind_10m2, na.rm = TRUE))

# --- 9) line plot by type color by season -----
for(TAXA_NAME_ in taxas) {
  cop_scatter[[TAXA_NAME_]] = ggplot(zp_o_top20a %>% filter(TAXA_NAME == TAXA_NAME_),
                                     aes(x=year, y = MeanT,
                                         color = season)) +
    geom_line(size = 1, alpha = 0.7) +
    ylab(expression("Ind/" ~ m^{2})) +
    scale_color_manual(values = mycolors) +
    facet_wrap(~Type, scales = "free", labeller = labeller(Type = as_labeller(facet_titles))) +
    ggtitle(paste0(" ", TAXA_NAME_)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          legend.text = element_text(size = 14), 
          plot.title = element_text(hjust = 0.5),
          strip.text = element_text(size = 14), 
          axis.text.x = element_text(color = "black",
                                     size = 15),
          axis.text.y = element_text(color = "black", size = 14), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = "black", face = "bold", 
                                      size = 12)) +
    scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
  print(cop_scatter[[TAXA_NAME_]])
  #ggsave(cop_scatter[[TAXA_NAME_]], file=paste0("plot_", TAXA_NAME_, ".png"))
}
################################################################

################################################################
zp_o_top20b <- zp_o_top20 %>%
  group_by(TAXA_NAME, year, season, Type, Name) %>%
  summarize(MeanT = mean(ind_10m2, na.rm = TRUE))

# --- 10) needs help -----
taxas1 <- unique(zp_o_top20b$TAXA_NAME) 
zp_line <- list()

for(TAXA_NAME_ in taxas1) {
  zp_line[[TAXA_NAME_]] = ggplot(zp_o_top20b %>% 
                                   filter(TAXA_NAME == TAXA_NAME_),
                                     aes(x=year, y = MeanT,
                                         color = season)) +
    #geom_line(size = 1, alpha = 0.7) +
    geom_bar(stat = "identity") + 
    ylab(expression("Ind/" ~ m^{2})) +
    scale_color_manual(values = mycolors) +
    facet_wrap(~Name, scales = "free") +
    ggtitle(paste0(" ", TAXA_NAME_)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          legend.text = element_text(size = 14), 
          plot.title = element_text(hjust = 0.5),
          strip.text = element_text(size = 14), 
          axis.text.x = element_text(color = "black",
                                     size = 15),
          axis.text.y = element_text(color = "black", size = 14), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = "black", face = "bold", 
                                      size = 12)) +
    scale_x_continuous(expand = expansion(mult = c(0.01,0.01)))
  print(zp_line[[TAXA_NAME_]])
  #ggsave(zp_line[[TAXA_NAME_]], file=paste0("plot_", TAXA_NAME_, ".png"))
}

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
library(ggOceanMaps)
basemap(data = cop_l_top, bathymetry = TRUE) +
  geom_point(data = cop_l_top, aes(x = lon, y=lat,
                             color = as.factor(taxa)))

species_to_plot <- "ctyp"
cop_one <- cop_l_top %>%
  filter(taxa == species_to_plot, year == 2015)

basemap(data = zp, bathymetry = TRUE) +
  geom_point(data = cop_one, aes(x = lon, y = lat,
                                 size = ind_10m2,  # use actual abundance column
                                 color = season),
             alpha = 0.7) +
  scale_size_continuous(name = "Ind/10m²", range = c(1, 8)) +
  ggtitle(paste("Spatial distribution of", species_to_plot, "in 2015")) +
  theme_minimal()

###########################
cop_l_topNAN <- cop_l_top %>% filter(ind_10m2 > 0)

basemap(data = cop_l_topNAN, bathymetry = TRUE) +
  geom_point(data = cop_l_topNAN, aes(x = lon, y=lat,
                                   color = as.factor(taxa))) +
  scale_color_manual(values = mycolors)
###########################

species_to_plot <- "larvaceans"
zp_larvacean <- zp_o %>%
  filter(taxa == species_to_plot, year == 2018)

basemap(data = zp_o, bathymetry = TRUE) +
  geom_point(data = zp_larvacean, aes(x = lon, y = lat,
                                 size = ind_10m2,  # use actual abundance column
                                 color = season),
             alpha = 0.7) +
  scale_size_continuous(name = "Ind/10m²", range = c(1, 8)) +
  ggtitle(paste("Spatial distribution of", species_to_plot, "in 2015")) +
  theme_minimal()
###########################




unique_species <- unique(cop_l_topNAN$taxa)

# Loop through each species and create a plot
for (species in unique_species) {
  # Subset data for the current species
  species_data <- subset(cop_l_topNAN, taxa == species)
  
  # Create the plot
  plot <- basemap(data = cop_l_topNAN, bathymetry = TRUE) +
    geom_point(data = species_data, aes(x = lon, y = lat,
                                        color = as.factor(taxa),
                                        size = volume_1m2)) +
    scale_color_manual(values = mycolors) +
    ggtitle(paste("Species:", species)) +
    theme_minimal()
  
  # Print the plot (or save it to a file)
  print(plot)
}

oith <- cop_l_topNAN %>%
  filter(taxa == "oithspp")

basemap(data = oith, bathymetry = TRUE) +
  geom_point(data = oith, aes(x = lon, y=lat, size = ind_10m2,
                              color = year))

years <- unique(oith$year)
# I DONT THINK THIS IS PLOTTING RIGHT
# Loop over each year
for (year in years) {
  # Subset the data for the current year
  data_year <- subset(oith, year == year)
  
  # Check if the data for the current year is empty
  if(nrow(data_year) == 0) {
    print(paste("No data for year", year))
    next
  }
  
  print(paste("Year:", year, "Number of rows:", nrow(data_year)))
  print(summary(data_year))
  
  # Create the plot for the current year
  p <- basemap(data = data_year, bathymetry = TRUE) +
    geom_point(data = data_year, aes(x = lon, y = lat, size = ind_10m2)) +
    labs(title = paste("Year:", year))
  
  # Print the plot
  print(p)
}

oith12 <- oith %>% filter(year == 2012)
basemap(data = oith12, bathymetry = TRUE) +
  geom_point(data = oith12, aes(x = lon, y = lat, size = ind_10m2))


basemap(data = oith, bathymetry = TRUE) +
  geom_point(data = oith, aes(x = lon, y=lat,
                                      color = as.factor(year))) +
  scale_color_manual(values = mycolors)

basemap(data = oith, bathymetry = TRUE) +
  geom_point(data = oith, aes(x = lon, y=lat))




ggplot(cop_l, aes(year, ind_10m2, fill = taxa, color = taxa)) +
  geom_col(position = "fill") +
  facet_wrap(~Type) +
  ylab(expression("Relative Abundance")) +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14,
                                    color = "black"),
        axis.text.x = element_text(color = "black", 
                                   size = 12, 
                                   angle = 45, hjust = 1
        ),
        strip.text = element_text(color = "black", 
                                  size = 12,
                                  face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2,"cm")) + 
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), labels=scales::percent)
#############################################################


library(maps)
US <- map_data("usa") 

zp_o %>% 
  #distinct(Location, Longitude, Latitude) %>% 
  ggplot() +
  geom_polygon(data = US, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.3, colour = "black") +
  geom_point(aes(x = lon, y = lat), colour = "steelblue") +
  coord_map() +
  labs(x = NULL, y = NULL, size = NULL, title = "Study sites", 
       subtitle = "(2002 - 2020)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

