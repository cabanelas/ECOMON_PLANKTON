### TRENDS


## Packages
library(tidyverse)
library(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 8), 
             brewer.pal(name="Paired", n = 6))

## Data
abu <- read.csv("raw/EcoMon_v3_8_wDateStrata.csv")
################################################################################
# add season to df
abu <- abu %>%
  mutate(season = case_when(between(month, 3, 5) ~ "spring",
                            between(month, 6, 8) ~ "summer",
                            between(month, 9, 11) ~ "fall",
                            TRUE ~ "winter"))
# add region to df
abu <- abu %>%
  mutate(Region = case_when(region == 1 ~ "MAB",
                            region == 2 ~ "SNE",
                            region == 3 ~ "GB",
                            region == 4 ~ "GOM",
                            TRUE ~ "Outside"))
################################################################################
abu <- abu %>% filter(Region != "Outside")

#remove all m3 columns
abu <- abu[, -grep("_100m3$", names(abu))]
names(abu)<-gsub("_10m2","",names(abu)) #to get rid of _10m2 in colnames

# m2
fish <- abu[, c(1:22, 54,116:162)]
zp <- abu[, c(1:53, 55:113, 161:162)]

#change data from wide to long
fish_long <- fish %>%
  pivot_longer(cols = fish:lopame,
               names_to = "taxa", values_to = "abundance")

zp_long <- zp %>%
  pivot_longer(cols = ctyp:pnepau,
               names_to = "taxa", values_to = "abundance")


# add taxa names
tnames <- read.csv("raw/EcoMon_TaxaNames_addedNames.csv")
tnames <- tnames %>% filter(unit == "10m2" | unit == "abnd")

# add names
fish_long1 <- left_join(fish_long, tnames, by = "taxa")
zp_long1 <- left_join(zp_long, tnames, by = "taxa")


################################################################################


################################################################################
################################################################################
#######       ZOOPLANKTON 

## summaries 
zp_long1 %>% 
  group_by(Region, TAXA_NAME, season) %>%
  summarise(meantax = mean(abundance, na.rm = T)) %>%
  distinct(Region, TAXA_NAME, season, .keep_all = T) %>%
  top_n(1, meantax) %>%
  arrange(desc(meantax)) %>%
  print(n= 16)

zp_long1 %>% 
  group_by(Region, TAXA_NAME) %>%
  summarise(meantax = mean(abundance, na.rm = T)) %>%
  distinct(Region, TAXA_NAME, .keep_all = T) %>%
  top_n(1, meantax) %>%
  arrange(desc(meantax)) 


zp_long1 %>% 
  group_by(season, TAXA_NAME) %>%
  summarise(meantax = mean(abundance, na.rm = T)) %>%
  distinct(season, TAXA_NAME, .keep_all = T) %>%
  top_n(1, meantax) %>%
  arrange(desc(meantax)) 

zp_long1 %>% 
  group_by(TAXA_NAME) %>%
  summarise(meantax = mean(abundance, na.rm = T)) %>%
  arrange(desc(meantax)) 

zp_long1 %>% 
  group_by(Region, TAXA_NAME) %>%
  summarise(meantax = mean(abundance, na.rm = T)) %>%
  distinct(Region, TAXA_NAME, .keep_all = T) %>%
  top_n(3, meantax) %>%
  arrange(desc(Region)) %>%
  print(n=48)

zp_long1 %>%
  group_by(Region, season,TAXA_NAME) %>%
  summarise(total_rows = n(),
            zero_count = sum(abundance == 0, na.rm = TRUE)) %>%
  filter(total_rows == zero_count)

## break it up
copepods <- zp_long1 %>%
  filter(group == "Calanoid" | group == "Cyclopoid")

euph <- zp_long1 %>%
  filter(group == "Euphausiid")

pterop <- zp_long1 %>%
  filter(group == "Pteropod")

other <- zp_long1 %>%
  filter(group != "Calanoid" & group != "Cyclopoid" & group != "Euphausiid" &
         group != "Pteropod")
###############################################################################
n_distinct(zp_long1$cruise_name)

cruise_counts <- zp_long1 %>%
  group_by(month, season) %>%
  summarise(num_cruises = n_distinct(cruise_name)) %>%
  ungroup() %>% #double counting some that span 2 months 
  mutate(sumtot = sum(num_cruises))

zp_long1 %>%
  group_by(season) %>%
  summarise(num_cruises = n_distinct(cruise_name)) %>%
  ungroup() %>% #double counting some that span 2 months 
  mutate(sumtot = sum(num_cruises))

cruise_season_counts <- zp_long1 %>%
  group_by(cruise_name) %>%
  summarise(distinct_season_count = n_distinct(season))

multi_season_cruises <- cruise_season_counts %>%
  filter(distinct_season_count > 1)

# Plot the number of cruises done each month, colored by season
ggplot(data = cruise_counts, aes(x = month, y = num_cruises, fill = season)) + 
  geom_bar(stat = "identity") +
  ggtitle("BADNumber of Cruises per Month") +
  labs(x = "Month", y = "Number of Cruises") +
  theme_bw()

season_counts <- zp_long1 %>% #double counting
  group_by(season, cruise_name) %>%
  summarise(num_cruises = n_distinct(month)) %>%
  group_by(season) %>%
  summarise(num_cruises = sum(num_cruises)) %>%
  ungroup()

# Plot the number of cruises per season
ggplot(data = season_counts, aes(x = season, y = num_cruises, fill = season)) + 
  geom_bar(stat = "identity") +
  ggtitle("badNumber of Cruises per Season") +
  labs(x = "Season", y = "Number of Cruises") +
  theme_bw()

region_season_counts <- zp_long1 %>%
  group_by(Region, season, cruise_name) %>%
  summarise(num_cruises = n_distinct(month)) %>%
  group_by(Region, season) %>%
  summarise(num_cruises = sum(num_cruises)) %>%
  ungroup()

# Plot the number of cruises per season for each region
ggplot(data = region_season_counts, aes(x = season, y = num_cruises, fill = season)) + 
  geom_bar(stat = "identity") +
  ggtitle("DOUBLE COUNTS Number of Cruises per Season") +
  labs(x = "Season", y = "Number of Cruises") +
  theme_bw() +
  facet_wrap(~ Region, scales = "free")

## same cruise might span multiple regions 
cruise_regions <- zp_long1 %>%
  group_by(cruise_name) %>%
  summarise(num_regions = n_distinct(region)) %>%
  ungroup()

# Check for cruises spanning multiple regions
cruises_spanning_multiple_regions <- cruise_regions %>%
  filter(num_regions > 1)


cruise_seasons <- zp_long1 %>%
  group_by(cruise_name) %>%
  summarise(num_seasons = n_distinct(season)) %>%
  ungroup()

# Check for cruises spanning multiple seasons
cruises_spanning_multiple_seasons <- cruise_seasons %>%
  filter(num_seasons > 1)

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

copepods <- copepods[, c(1,11:12, 14, 23:32)]

zp_long1 %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(total_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = total_abundance) %>%
  ungroup() %>%
  print(n=32)

#takes a while messy but somewhat helpful
ggplot(data=copepods, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)

ggplot(data=copepods, 
       aes(x=year, y=abundance, fill = group)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  #scale_fill_manual(values = mycolors) +
  facet_grid(Region~season)
################################################################################
taxa3 <- copepods %>%
  filter(taxa == "ctyp" | taxa == "calfin" | taxa == "pseudo")


ggplot(data=taxa3, 
       aes(x=year, y=abundance, fill = taxa)) + 
  geom_bar(stat = "identity") +
  facet_grid(Region~season)

ggplot(data=taxa3, 
       aes(x=year, y=abundance, fill = taxa)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~season) 

ggplot(data=taxa3, 
       aes(x=year, y=abundance, fill = taxa)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Region) 

taxa3_summarized <- taxa3 %>%
  group_by(year, Region, season, taxa) %>%
  summarize(mean_abundance = mean(abundance, na.rm = TRUE))

ggplot(data=taxa3_summarized, 
       aes(x=year, y=mean_abundance, color=taxa, group=taxa)) + 
  geom_line() +
  facet_grid(Region ~ season) +
  theme_minimal()

library(reshape2)

taxa3_tot <- taxa3 %>%
  group_by(year, Region, season, taxa) %>%
  summarize(tot_abundance = sum(abundance, na.rm = TRUE))

# Assuming taxa3 is a data frame
taxa3_melt <- melt(taxa3_tot, id.vars = c("year", "Region", "season", "taxa"), 
                   measure.vars = "tot_abundance")

ggplot(taxa3_melt, 
       aes(x=year, y=taxa, fill=value)) + 
  geom_tile() +
  facet_grid(Region~season) +
  scale_fill_gradient(low="white", high="blue") +
  theme_minimal() #this is not correct; should do it with anomalies 

pseudoc <- taxa3 %>%
  filter(taxa == "pseudo")

ctypi <- taxa3 %>%
  filter(taxa == "ctyp")

calafin <- taxa3 %>%
  filter(taxa == "calfin")

ggplot(data=pseudoc, 
       aes(x=year, y=abundance, fill = taxa)) + 
  geom_bar(stat = "identity") +
  facet_grid(Region~season)

ggplot(data=ctypi, 
       aes(x=year, y=abundance, fill = taxa)) + 
  geom_bar(stat = "identity") +
  facet_grid(Region~season)

ggplot(data=calafin, 
       aes(x=year, y=abundance, fill = taxa)) + 
  geom_bar(stat = "identity") +
  facet_grid(Region~season)

################################################################################

#mean
aggregated_data <- copepods %>%
  group_by(TAXA_NAME, year, Region, season) %>%
  summarise(mean_abundance = mean(abundance, na.rm = TRUE)) %>%
  ungroup()

unique_taxa <- unique(aggregated_data$TAXA_NAME)

# Create an empty list to store ggplot objects
plot_list <- list()

# Iterate over unique TAXA_NAME
for (taxa in unique_taxa) {
  # Subset data for the current TAXA_NAME
  taxa_data <- filter(aggregated_data, TAXA_NAME == taxa)
  
  # Create ggplot object
  gg <- ggplot(data = taxa_data, aes(x = year, y = mean_abundance)) +
    geom_line() +
    facet_grid(Region ~ season) +
    ggtitle(paste("MEAN Abundance of", taxa))  # Title with TAXA_NAME
  
  plot_list[[taxa]] <- gg
}

# Plot all ggplot objects
for (gg_plot in plot_list) {
  print(gg_plot)
}

#save
if (!dir.exists("copepodLineplots")) {
  dir.create("copepodLineplots")
}

# Iterate over the plot_list and save each plot
for (taxa in names(plot_list)) {
  # Generate the file name for the plot
  filename <- paste("copepodLineplots/", taxa, ".png", sep="")
  
  # Save the plot as PNG file
  ggsave(filename, plot_list[[taxa]], width = 10, height = 6, units = "in")
}

################################################################################
#sum
sumaggregated_data <- copepods %>%
  group_by(TAXA_NAME, year, Region, season) %>%
  summarise(sum_abundance = sum(abundance, na.rm = TRUE)) %>%
  ungroup()

unique_taxa <- unique(sumaggregated_data$TAXA_NAME)

# Create an empty list to store ggplot objects
plot_list <- list()

# Iterate over unique TAXA_NAME
for (taxa in unique_taxa) {
  # Subset data for the current TAXA_NAME
  taxa_data <- filter(sumaggregated_data, TAXA_NAME == taxa)
  
  # Create ggplot object
  gg <- ggplot(data = taxa_data, aes(x = year, y = sum_abundance)) +
    geom_line() +
    facet_grid(Region ~ season) +
    ggtitle(paste("MEAN Abundance of", taxa))  # Title with TAXA_NAME
  
  plot_list[[taxa]] <- gg
}

# Plot all ggplot objects
for (gg_plot in plot_list) {
  print(gg_plot)
}

#save
if (!dir.exists("copepodSUMLineplots")) {
  dir.create("copepodSUMLineplots")
}

# Iterate over the plot_list and save each plot
for (taxa in names(plot_list)) {
  # Generate the file name for the plot
  filename <- paste("copepodSUMLineplots/", taxa, ".png", sep="")
  
  # Save the plot as PNG file
  ggsave(filename, plot_list[[taxa]], width = 10, height = 6, units = "in")
}
################################################################################

# DONT RUN
aggregated_data_cruise <- copepods %>%
  group_by(TAXA_NAME, year, Region, season, cruise_name) %>%
  summarise(mean_abundance = mean(abundance, na.rm = TRUE)) %>%
  ungroup()

copepods_noNA <- aggregated_data_cruise %>% drop_na(mean_abundance)

plot_list <- list()
unique_taxa <- unique(copepods_noNA$TAXA_NAME)
# Iterate over unique TAXA_NAME
for (taxa in unique_taxa) {
  # Subset data for the current TAXA_NAME
  taxa_data <- filter(copepods_noNA, TAXA_NAME == taxa)
  
  # Create a bar plot
  gg <- ggplot(data = taxa_data, aes(x = year, y = mean_abundance, fill = cruise_name)) +
    geom_bar(stat = "identity", position = "fill") +
    labs(y = "Relative Abundance (%)") +
    facet_grid(Region ~ season) +
    ggtitle(paste("Abundance of", taxa))  # Title with TAXA_NAME
  
  plot_list[[taxa]] <- gg
}

# Plot all ggplot objects
for (gg_plot in plot_list) {
  print(gg_plot)
}

################################################################################
copepods_filtered <- copepods %>% #something wrong??
  group_by(TAXA_NAME, year, Region, season) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(TAXA_NAME, Region, season) %>%
  mutate(proportion = total_abundance / sum(total_abundance, na.rm = TRUE)) %>%
  filter(proportion >= 0.10) %>%
  ungroup()

ggplot(data = copepods_filtered, 
       aes(x = year, y = total_abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)") +
  facet_grid(Region ~ season)

################################################################################

copepods %>%
  group_by(TAXA_NAME, Region, season) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  arrange(Region, season, desc(total_abundance)) %>%
  slice_head(n = 5) %>%
  ungroup()

#relative abundance overall
v <- copepods %>%
  group_by(Region, season) %>%
  mutate(total_abundance = sum(abundance, na.rm = TRUE),
         relative_abundance = abundance / total_abundance) %>%
  ungroup() %>%
  select(-total_abundance)

ggplot(data = v, 
       aes(x = year, y = relative_abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill", width = 2) +
  labs(y = "Relative Abundance (%)") +
  facet_grid(Region ~ season)


## good - top taxa for each region-season - sum
copepods %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 1, wt = total_abundance) %>%
  ungroup()
## good - top taxa for each region-season - mean
copepods %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(m_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 1, wt = m_abundance) %>%
  ungroup()

## good - top 4 
top_four_taxa <- copepods %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 4, wt = total_abundance) %>%
  arrange(Region, season, desc(total_abundance)) %>%
  ungroup()

ggplot(top_four_taxa, aes(x = TAXA_NAME, y = total_abundance, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  facet_grid(Region ~ season) +
  labs(title = "Top Four Taxa by Year, Region, and Season",
       x = "Year",
       y = "Total Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##
top_four_taxa_y <- copepods %>%
  group_by(year, Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(year, Region, season) %>%
  top_n(n = 4, wt = total_abundance) %>%
  arrange(year, Region, season, desc(total_abundance)) %>%
  ungroup()

ggplot(top_four_taxa_y, aes(x = year, y = total_abundance, fill = TAXA_NAME)) +
  geom_bar(stat = "identity", position = "fill", width = 1) +
  facet_grid(Region ~ season) +
  labs(title = "Top Four Taxa by Year, Region, and Season",
       x = "Year",
       y = "Total Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##

top_2_taxa_y <- copepods %>%
  group_by(year, Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(year, Region, season) %>%
  top_n(n = 2, wt = total_abundance) %>%
  arrange(year, Region, season, desc(total_abundance)) %>%
  ungroup()

ggplot(top_2_taxa_y, aes(x = year, y = total_abundance, fill = TAXA_NAME)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(Region ~ season) +
  labs(title = "Top Four Taxa by Year, Region, and Season",
       x = "Year",
       y = "Total Abundance") + #WRONG
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 
mycolors = c(brewer.pal(name="Dark2", n = 8), 
             brewer.pal(name="Paired", n = 6))

cop3 <- copepods %>%
  filter(taxa == "ctyp" | taxa == "calfin" | taxa == "pseudo")

cop3$season <- factor(cop3$season, levels = c("winter", "spring", "summer", "fall"))

cop3$Region <- factor(cop3$Region, levels = c("GOM", "GB", "SNE", "MAB"))

ggplot(cop3, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(Region ~ season) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(
       x = "Year",
       y = "Relative Abundance") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16,
                                    color = "black"),
        strip.text = element_text(color = "black", 
                                  size = 14,
                                  face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.2,"cm"),
        axis.text.x = element_text(angle = 40, hjust = 1, color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) + 
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), labels=scales::percent)


library(scales)

# Custom function to remove "0%" label
custom_percent <- function(x) {
  percent_labels <- percent(x)
  percent_labels[x == 0] <- ""
  return(percent_labels)
}

ggplot(cop3, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_col(stat = "identity", position = "fill", width = 1) +
  facet_grid(Region ~ season) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(color = "black", size = 1),  # Thicker borders
    panel.spacing = unit(0.8, "lines"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, color = "black", face = "bold"),
    strip.text = element_text(color = "black", size = 15, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black"), 
    #axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 15),
    axis.text.y = element_text(color = "black", size = 13)
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = custom_percent)


ggplot(cop3, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_col(stat = "identity", position = "fill", width = 1) +
  facet_grid(season~Region) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(color = "black", size = 1),  # Thicker borders
    panel.spacing = unit(0.8, "lines"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, color = "black", face = "bold"),
    strip.text = element_text(color = "black", size = 15, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black"), 
    #axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 15),
    axis.text.y = element_text(color = "black", size = 13)
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = custom_percent)


#to save without seeing all the individual "cols" 
cop3_summarized <- cop3 %>%
  group_by(year, Region, season, TAXA_NAME) %>%
  summarize(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop')

# Plot the summarized data
p2a <- ggplot(cop3_summarized, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_col(stat = "identity", position = "fill", width = 1.2) +
  facet_grid(season~Region) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(color = "black", size = 1),  # Thicker borders
    panel.spacing = unit(0.8, "lines"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, color = "black", face = "bold"),
    strip.text = element_text(color = "black", size = 15, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black"), 
    #axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 15),
    axis.text.y = element_text(color = "black", size = 13)
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = custom_percent)
#ggsave("seasonbyregionrelativeabundance_V2.png", plot = p2a, width = 10, height = 8, dpi = 300)


p2b <- ggplot(cop3_summarized, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_col(stat = "identity", position = "fill", width = 1.2) +
  facet_grid(Region~season) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(color = "black", size = 1),  # Thicker borders
    panel.spacing = unit(0.8, "lines"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, color = "black", face = "bold"),
    strip.text = element_text(color = "black", size = 15, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black"), 
    #axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 15),
    axis.text.y = element_text(color = "black", size = 13)
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = custom_percent)
#ggsave("regionbyseasonrelativeabundance_v2.png", plot = p2b, width = 10, height = 8, dpi = 300)

################################################################################
copepods1 <- copepods %>%
  mutate(TAXA_NAME = ifelse(TAXA_NAME %in% c("Centropages typicus", 
                                             "Calanus finmarchicus", 
                                             "Pseudocalanus spp."), TAXA_NAME, 
                            "other copepods"))

cop1_summarized1 <- copepods1 %>%
  group_by(year, Region, season, TAXA_NAME) %>%
  summarize(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop')

cop1_summarized1$season <- factor(cop1_summarized1$season, 
                                  levels = c("winter", "spring", "summer", "fall"))

cop1_summarized1$TAXA_NAME <- factor(cop1_summarized1$TAXA_NAME, 
                                  levels = c("Calanus finmarchicus", 
                                             "Centropages typicus", 
                                             "Pseudocalanus spp.",
                                             "other copepods"))

cop1_summarized1$Region <- factor(cop1_summarized1$Region, 
                                  levels = c("GOM", "GB", "SNE", "MAB"))


p3a <- ggplot(cop1_summarized1, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_col(stat = "identity", position = "fill", width = 1.2) +
  facet_grid(season~Region) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(color = "black", size = 1),  # Thicker borders
    panel.spacing = unit(0.8, "lines"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, color = "black", face = "bold"),
    strip.text = element_text(color = "black", size = 15, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black"), 
    #axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 15),
    axis.text.y = element_text(color = "black", size = 13)
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = custom_percent)
#ggsave("seasonbyregionrelativeabundance_withother.png", plot = p3a, width = 10, height = 8, dpi = 300)


p3b <- ggplot(cop1_summarized1, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_col(stat = "identity", position = "fill", width = 1.2) +
  facet_grid(Region~season) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(color = "black", size = 1),  # Thicker borders
    panel.spacing = unit(0.8, "lines"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, color = "black", face = "bold"),
    strip.text = element_text(color = "black", size = 15, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black"), 
    #axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 15),
    axis.text.y = element_text(color = "black", size = 13)
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = custom_percent)
#ggsave("regionbyseasonrelativeabundance_withother2a.png", plot = p3b, width = 10, height = 8, dpi = 300)

#change color
dark2_colors <- brewer.pal(name = "Dark2", n = 8) 
paired_colors <- brewer.pal(name = "Paired", n = 6)  
mycolors2 <- c("Calanus finmarchicus" = dark2_colors[1],
               "Centropages typicus" = dark2_colors[2],
              "Pseudocalanus spp." = dark2_colors[3],
              "other copepods" = "#666666"
                #dark2_colors[8]
              )
mycolors2 <- c(
  "Calanus finmarchicus" = "#1B9E77",  # teal
  "Centropages typicus" = "#D95F02",  # orange
  "Pseudocalanus spp." = "#7570B3",  # purple
  "other copepods" = "#A6BD8A"  
)

(p3bcolo <- ggplot(cop1_summarized1, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_col(stat = "identity", position = "fill", width = 1.2) +
  facet_grid(Region~season) +
  scale_fill_manual(values = mycolors2) +
  scale_color_manual(values = mycolors2) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(color = "black", size = 1),  # Thicker borders
    panel.spacing = unit(0.8, "lines"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, color = "black", face = "bold"),
    strip.text = element_text(color = "black", size = 15, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black"), 
    #axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 15),
    axis.text.y = element_text(color = "black", size = 13)
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = custom_percent)
)
#ggsave("regionbyseasonrelativeabundance_withother2acolor.png", plot = p3bcolo, width = 10, height = 8, dpi = 300)


cop1_summarized12 <- copepods1 %>%
  group_by(year, TAXA_NAME) %>%
  summarize(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop')
cop1_summarized12$TAXA_NAME <- factor(cop1_summarized12$TAXA_NAME, 
                                     levels = c("Calanus finmarchicus", 
                                                "Centropages typicus", 
                                                "Pseudocalanus spp.",
                                                "other copepods"))

overallcopplot <- ggplot(cop1_summarized12, aes(x = year, y = abundance, fill = TAXA_NAME)) +
  geom_col(stat = "identity", position = "fill", width = 1.2) +
  scale_fill_manual(values = mycolors2) +
  scale_color_manual(values = mycolors2) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, color = "black", face = "bold"),
    #axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    #axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 15),
    axis.text.y = element_text(color = "black", size = 13)
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.001))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.001)), labels = custom_percent)
#ggsave("relativeabundance_overallcopepods2.png", plot = overallcopplot, width = 10, height = 8, dpi = 300)

################################################################################
### FREQUENCY OF OCCURRENCE - i think its wrong
total_surveys <- n_distinct(copepods$cruise_name)

copepods1 <- copepods %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

Present <- copepods1 %>%
  group_by(cruise_name, TAXA_NAME, year, Region, season) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_year <- Present %>%
  group_by(year, Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_year, aes(year, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(x = "Year", fill = "Prey Name", y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season) +
  theme_bw() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))
##

######
occurrences <- copepods %>%
  filter(abundance > 0) %>%  # Consider only rows where abundance is greater than zero
  group_by(TAXA_NAME) %>%
  summarise(num_occurrences = n_distinct(cruise_name)) %>%
  ungroup()

occurrences$frequency_of_occurrence <- occurrences$num_occurrences / n_distinct(zp_long1$cruise_name)


######

top_taxa <- FO %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(mean_FOi = mean(FOi)) %>%
  top_n(5, wt = mean_FOi) %>%
  arrange(Region, season, desc(mean_FOi))

Present %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the number of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup() %>%
  top_n(5, wt = Nfi) %>%
  arrange(Region, season, desc(Nfi))
################################################################################

FO_overall <- Present %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

FO_overall$TAXA_NAME <- with(FO_overall, reorder(TAXA_NAME, -FOi))


ggplot(FO_overall, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(x = "Year", fill = "Prey Name", y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season) +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))


#### FO all

occurrencesall <- zp_long1 %>%
  filter(abundance > 0) %>%  
  group_by(TAXA_NAME) %>%
  summarise(num_occurrences = n_distinct(cruise_name)) %>%
  ungroup()

occurrencesall$FOi <- occurrencesall$num_occurrences / n_distinct(zp_long1$cruise_name)

occurrencesall$TAXA_NAME <- factor(occurrencesall$TAXA_NAME, 
                                   levels = occurrencesall$TAXA_NAME[order(occurrencesall$FOi, decreasing = TRUE)])

ggplot(occurrencesall, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence")

ggplot(subset(occurrencesall,FOi > 0.89 & TAXA_NAME != "Euphausiacea1"), 
       aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence")

# by region
occurrencesallR <- zp_long1 %>%
  filter(abundance > 0) %>%  
  group_by(TAXA_NAME, Region) %>%
  summarise(num_occurrences = n_distinct(cruise_name)) %>%
  ungroup()

occurrencesallR$FOi <- occurrencesallR$num_occurrences / n_distinct(zp_long1$cruise_name)

occurrencesallR$TAXA_NAME <- factor(occurrencesallR$TAXA_NAME, 
                                   levels = occurrencesallR$TAXA_NAME[order(occurrencesallR$FOi, decreasing = TRUE)])

ggplot(occurrencesallR, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  facet_wrap(~Region)

ggplot(subset(occurrencesallR,FOi > 0.59 & TAXA_NAME != "Euphausiacea1"), 
       aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  facet_wrap(~Region)

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
## most frequent
taxa_of_interest <- c("Centropages typicus", "Calanus finmarchicus", "Pseudocalanus spp.", 
                      "Tortanus discaudatus", "Oithona spp.", "Metridia lucens", 
                      "Centropages hamatus", "Paracalanus parvus", "Clausocalanus arcuicornis",
                      "Acartia spp.")

# Filter the data frame to include only the specified TAXA_NAME values
top_cope <- copepods %>%
  filter(TAXA_NAME %in% taxa_of_interest)

ggplot(data=top_cope, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season) +
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

#centropages typicus most abundant in fall 
# paracalanus parvus present in fall and then it looks like its replaced
# by pseudocalanus in spring, summer and a bit in winter
#GOM and GB have the most C.finmarchicus
# Acartia spp. relevant in MAB and SNE not much in GOM and GB
# centropages hamatus (abundant mainly in GB in summer of some yrs)
##

# remove tortanus discaudatus
# remove oithona spp
# remove clausocalanus arcuicornis 
taxa_of_interest2 <- c("Centropages typicus", "Calanus finmarchicus", "Pseudocalanus spp.", "Metridia lucens", 
                      "Centropages hamatus", "Paracalanus parvus",
                      "Acartia spp.")

# Filter the data frame to include only the specified TAXA_NAME values
top_cope2 <- copepods %>%
  filter(TAXA_NAME %in% taxa_of_interest2)

ggplot(data=top_cope2, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season) +
  theme_bw() + 
  #scale_fill_manual(values = mycolors) +
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


ggplot(data=top_cope2, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(season~Region) +
  theme_bw() + 
  #scale_fill_manual(values = mycolors) +
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
###
## centropges and calanus
taxa_of_interest3 <- c("Centropages typicus", "Calanus finmarchicus",
                       "Pseudocalanus spp.")

# Filter the data frame to include only the specified TAXA_NAME values
top_cope3 <- copepods %>%
  filter(TAXA_NAME %in% taxa_of_interest3)

ggplot(data=top_cope3, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season) +
  theme_bw() + 
  #scale_fill_manual(values = mycolors) +
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
###############################################################################
###############################################################################
###############################################################################
aggregated_data_top_cope3 <- top_cope3 %>%
  group_by(TAXA_NAME, year, Region, season) %>%
  summarise(mean_abundance = mean(abundance, na.rm = TRUE)) %>%
  ungroup()

ggplot(data=aggregated_data_top_cope3, 
       aes(x=year, y=mean_abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season) +
  theme_bw() + 
  #scale_fill_manual(values = mycolors) +
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


###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
#euph


ggplot(data=euph, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)

eupha <- euph %>% filter(taxa != "euph" & taxa != "euph1")
euphb <- euph %>% filter(taxa != "euph")

ggplot(data=eupha, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)


euphamean <- eupha %>%
  group_by(TAXA_NAME, Region, season) %>%
  summarise(mean_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  arrange(Region, season, desc(mean_abundance)) %>%
  slice_head(n = 5) %>%
  ungroup()

ggplot(euphamean, aes(x=TAXA_NAME, y=mean_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank())

#not very abundant
#nematocelis megalops, thysanoessa spp, thysanoessa raschii,
#thysanoessa inermis, 


eupha %>%
  group_by(TAXA_NAME, Region, season) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  arrange(Region, season, desc(total_abundance)) %>%
  slice_head(n = 5) %>%
  ungroup()



### FREQUENCY OF OCCURRENCE
#total_surveys_euph <- n_distinct(euph$cruise_name)

euphAbun <- eupha %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

Presenteuph <- euphAbun %>%
  group_by(cruise_name, TAXA_NAME, Region, season) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_euph <- Presenteuph %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_euph, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(x = "Year", fill = "Prey Name", y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season, scales = "free") +
  theme_bw() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))
##

## good - top taxa for each region-season
sumeupha <- eupha %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = total_abundance) %>%
  ungroup()
ggplot(sumeupha, aes(x=TAXA_NAME, y=total_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")
## good - top taxa for each region-season - mean
meaneupha <- eupha %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(m_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = m_abundance) %>%
  ungroup()
ggplot(meaneupha, aes(x=TAXA_NAME, y=m_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")


## top are 
#euphausia krohnii 
#meganyctiphanes norvegice
#thysanoessa gregaria 


krillAbun <- eupha %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

Presentkrill <- krillAbun %>%
  group_by(cruise_name, TAXA_NAME, Region, season) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_krill <- Presentkrill %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_krill, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(x = "Year", fill = "Prey Name", y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season, scales = "free") +
  theme_bw() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))

###############################################################################
###############################################################################
#pterop
ggplot(data=pterop, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)

pterop_1 <- pterop %>%
  filter(taxa != "thecos")

### FREQUENCY OF OCCURRENCE
#total_surveys_pter <- n_distinct(pterop$cruise_name)

pteropAbun <- pterop %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

Presentpterop <- pteropAbun %>%
  group_by(cruise_name, TAXA_NAME, Region, season) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_pterop <- Presentpterop %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_pterop, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(x = "Year", fill = "Prey Name", y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season, scales = "free") +
  theme_bw() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))


## good - top taxa for each region-season
sumpterop <- pterop %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = total_abundance) %>%
  ungroup()
ggplot(sumpterop, aes(x=TAXA_NAME, y=total_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")
## good - top taxa for each region-season - mean
meanpterop <- pterop %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(m_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = m_abundance) %>%
  ungroup()
ggplot(meanpterop, aes(x=TAXA_NAME, y=m_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")

#spioratella spp, thecomosomata

###############################################################################
###############################################################################
#other
ggplot(data=other, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season) 


### FREQUENCY OF OCCURRENCE
#total_surveys_pter <- n_distinct(other$cruise_name)

otherAbun <- other %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

Presentother <- otherAbun %>%
  group_by(cruise_name, TAXA_NAME, Region, season) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_other <- Presentother %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_other, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(x = "Year", fill = "Prey Name", y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season, scales = "free") +
  theme_bw() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        axis.ticks.length = unit(0.2,"cm")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))



## good - top taxa for each region-season
sumother <- other %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = total_abundance) %>%
  ungroup() %>%
  print(n=32)
ggplot(sumother, aes(x=TAXA_NAME, y=total_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")
## good - top taxa for each region-season - mean
meanother <- other %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(m_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = m_abundance) %>%
  ungroup()
ggplot(meanother, aes(x=TAXA_NAME, y=m_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")
#gastropoda
#appendicularians
#echidorm
#penilia

###############################################################################
###############################################################################

## comparing top from diff groups
toptaxa <- c("ctyp", "calfin", "euphkr", "thysgr", "megan", "spirspp",
             "thecos", "gas", "echino", "penilia", "larvaceans", "pseudo")

# Filter the data frame to include only the specified TAXA_NAME values
tog <- zp_long1 %>%
  filter(taxa %in% toptaxa)


## good - top taxa for each region-season
sumtog <- tog %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = total_abundance) %>%
  ungroup() %>%
  print(n=32)
ggplot(sumtog, aes(x=TAXA_NAME, y=total_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")
## good - top taxa for each region-season - mean
meantog <- tog %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(m_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = m_abundance) %>%
  ungroup()
ggplot(meantog, aes(x=TAXA_NAME, y=m_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")



### FREQUENCY OF OCCURRENCE
#total_surveys_pter <- n_distinct(tog$cruise_name)

togAbun <- tog %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

Presenttog <- togAbun %>%
  group_by(cruise_name, TAXA_NAME, Region, season) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_tog <- Presenttog %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_tog, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))


FO_togor <- FO_tog %>%
  group_by(Region, season) %>%
  mutate(TAXA_NAME = factor(TAXA_NAME, levels = unique(TAXA_NAME[order(-FOi)]))) %>%
  ungroup()

# Plot the data with reordered bars
ggplot(FO_togor, aes(x = TAXA_NAME, y = FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5, size = 18),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 16),
        strip.background = element_rect(fill = "white"),  # Set facet label background to white
        strip.text = element_text(size = 14)) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
  guides(
    fill = guide_legend(
      nrow = 2  
    )
  )


ggplot(subset(FO_togor, FOi > 14), 
       aes(x = TAXA_NAME, y = FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5, size = 18),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 16),
        strip.background = element_rect(fill = "white"),  # Set facet label background to white
        strip.text = element_text(size = 14)) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
  guides(
    fill = guide_legend(
      nrow = 2  
    )
  )



chat <- zp_long1 %>% filter(taxa=="chaeto")
ggplot(chat, aes(x=year, y=abundance)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
## fish
ggplot(data=fish_long1, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)

fish_long2 <- fish_long1 %>% filter(taxa != "fish")

fish_long2 <- fish_long2 %>% filter(abundance > 0)

ggplot(data=fish_long2, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)

ggplot(data=fish_long2, 
       aes(x=year, y=abundance, fill = group1)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)

fish_pelagic <- fish_long1 %>% filter(group1 == "pelagic")
fish_demersal <- fish_long1 %>% filter(group1 == "demersal")

ggplot(data=fish_pelagic, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)

ggplot(data=fish_demersal, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Relative Abundance (%)")+
  facet_grid(Region~season)

fish_long2 %>%
  group_by(Region, TAXA_NAME) %>%
  summarise(total_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region) %>%
  top_n(n = 2, wt = total_abundance) %>%
  ungroup() %>%
  print(n=32)

fish_long2 %>%
  group_by(TAXA_NAME) %>%
  summarise(total_abundance = mean(abundance, na.rm = TRUE)) %>%
  top_n(n = 4, wt = total_abundance) %>%
  ungroup() %>%
  print(n=32)

fish_long2 %>%
  group_by(season, TAXA_NAME) %>%
  summarise(total_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(season) %>%
  top_n(n = 2, wt = total_abundance) %>%
  ungroup() %>%
  print(n=32)

## good - top taxa for each region-season
sumfish <- fish_long2 %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = total_abundance) %>%
  ungroup() %>%
  print(n=32)
ggplot(sumfish, aes(x=TAXA_NAME, y=total_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")
## good - top taxa for each region-season - mean
meanfish <- fish_long2 %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(m_abundance = mean(abundance, na.rm = TRUE)) %>%
  group_by(Region, season) %>%
  top_n(n = 2, wt = m_abundance) %>%
  ungroup()
ggplot(meanfish, aes(x=TAXA_NAME, y=m_abundance, fill = TAXA_NAME)) +
  geom_bar(stat="identity") +
  facet_grid(Region~season, scales = "free")



## comparing top from diff groups
topfishtaxa <- c("ammspp","cluhar", "urospp","micund"
                 )

# Filter the data frame to include only the specified TAXA_NAME values
topfish <- fish_long1 %>%
  filter(taxa %in% topfishtaxa)


### FREQUENCY OF OCCURRENCE
#total_surveys_pter <- n_distinct(topfish$cruise_name)

fishAbun <- topfish %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

PresentFish <- fishAbun %>%
  group_by(cruise_name, TAXA_NAME, Region, season) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_fish <- PresentFish %>%
  group_by(Region, season, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_fish, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))



FO_fishor <- FO_fish %>%
  group_by(Region, season) %>%
  mutate(TAXA_NAME = factor(TAXA_NAME, levels = unique(TAXA_NAME[order(-FOi)]))) %>%
  ungroup()

# Plot the data with reordered bars
ggplot(FO_fishor, aes(x = TAXA_NAME, y = FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  facet_grid(Region ~ season) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5, size = 18),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 16),
        strip.background = element_rect(fill = "white"),  # Set facet label background to white
        strip.text = element_text(size = 14)) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
  guides(
    fill = guide_legend(
      nrow = 1  
    )
  )

## fo by region - NOT SEASON

PresentFishREG <- fishAbun %>%
  group_by(cruise_name, TAXA_NAME, Region) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_fishREG <- PresentFishREG %>%
  group_by(Region, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_fishREG, aes(x = TAXA_NAME, y = FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  facet_wrap(~Region) +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5, size = 18),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 16),
        strip.background = element_rect(fill = "white"),  # Set facet label background to white
        strip.text = element_text(size = 14)) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
  guides(
    fill = guide_legend(
      nrow = 1  
    )
  )





fishAbun2 <- fish_long2 %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

PresentFish2 <- fishAbun2 %>%
  group_by(cruise_name, TAXA_NAME, Region) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_fish2 <- PresentFish2 %>%
  group_by(Region, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_fish2, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  facet_wrap(~Region) +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))







fishAbun3 <- fish_long2 %>%
  mutate(abundance = ifelse(is.na(abundance), 0, abundance))

PresentFish3 <- fishAbun3 %>%
  group_by(cruise_name, TAXA_NAME) %>%
  summarise(presence = sum(abundance > 0) > 0) %>%
  ungroup()

FO_fish3 <- PresentFish3 %>%
  group_by(TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

ggplot(FO_fish3, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))








#not sure if i need to group by cruise name in step 1
# Step 1: Calculate FO for each taxa in each region
FO_fish4 <- PresentFish2 %>%
  group_by(Region, TAXA_NAME) %>%
  summarise(Nfi = sum(presence),  # Calculate the # of cruises where the taxa is present
            FOi = (Nfi / total_surveys) * 100) %>%  # Calculate FO using the formula
  ungroup()

# Step 2: Sort the taxa based on their FO in each region
FO_sorted4 <- FO_fish4 %>%
  group_by(Region) %>%
  arrange(desc(FOi)) %>%
  ungroup()

# Step 3: Keep only the top 20 taxa with the highest FO in each region
top_taxa4 <- FO_sorted4 %>%
  group_by(Region) %>%
  top_n(20, FOi) %>%
  ungroup()

# Step 4: Visualization with the filtered data
ggplot(top_taxa4, aes(TAXA_NAME, FOi, fill = TAXA_NAME)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of Occurrence") +
  labs(y = "Frequency of Occurrence (%)") + 
  facet_wrap(~Region) +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust=0.5),
        legend.background = element_rect(fill = "white", color = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

#To determine which "taxa" are present in > 25% of quarterly samples
data<-abu
# Create a new column representing the quarters
data <- data %>%
  mutate(quarter1 = quarter(paste(year, month, sep = "-")))

# Step 1: Calculate presence within each quarter
presence_quarterly <- data %>%
  group_by(taxa, quarter) %>%
  summarise(presence = any(quartly_samples > 0)) %>%
  ungroup()

# Step 2: Aggregate presence information across quarters
presence_aggregated <- presence_quarterly %>%
  group_by(taxa) %>%
  summarise(presence_rate = mean(presence))

# Step 3: Filter taxa present in more than 25% of quarters
taxa_present_25percent <- presence_aggregated %>%
  filter(presence_rate > 0.25)

# Display taxa present in more than 25% of quarters
print(taxa_present_25percent)




####
# Step 1: Group by "taxa", "region", and "season", and summarize the presence of each taxa within each season-region combination
presence_summary <- zp_long1 %>%
  group_by(taxa, Region, season) %>%
  summarise(total_samples = n_distinct(date),  # Count distinct survey dates
            present_samples = sum(presence))




##### samples present in >25% of unique cruises

zp_long1 %>%
  group_by(Region, season) %>%
  summarise(
    total_samples = n_distinct(cruise_name),  # Count total number of distinct samples
    taxa_present_samples = sum(!is.na(abundance) & abundance > 0)  # Count samples where taxa is present
  )

presence_summ <- zp_long1 %>%
  group_by(Region, season,taxa) %>%
  summarise(
    total_samples = n_distinct(cruise_name),  # Count total number of distinct samples
    taxa_present_samples = sum(!is.na(abundance) & abundance > 0),  # Count samples where taxa is present
    taxa_present = unique(taxa[abundance > 0])  # List of unique taxa present
  ) %>%
  mutate(
    presence_proportion = taxa_present_samples / total_samples  # Calculate proportion of samples where taxa is present
  ) %>%
  filter(presence_proportion > 0.90)
library(tidyr)
presence_summary <- presence_summ %>%
  unnest(taxa_present)  # Flatten the list column

unique_taxa <- unique(presence_summary$taxa_present)

print(unique_taxa)





zp_long1 %>%
  group_by(Region, season, taxa) %>%
  summarise(
    total_samples = n_distinct(cruise_name),  # Count total number of distinct samples
    taxa_present_samples = sum(!is.na(abundance) & abundance > 0),  # Count samples where taxa is present
    presence_proportion = taxa_present_samples / total_samples  # Calculate proportion of samples where taxa is present
  ) %>%
  filter(presence_proportion > 25) %>%
  mutate(
    taxa_present = na.omit(taxa)  # Remove NAs from taxa_present
  )


zp_long1 %>%
  group_by(Region, season, taxa) %>%
  summarise(
    total_samples = n_distinct(cruise_name),  # Count total number of distinct samples
    taxa_present_samples = sum(!is.na(abundance) & abundance > 0),  # Count samples where taxa is present
    presence_proportion = taxa_present_samples / total_samples  # Calculate proportion of samples where taxa is present
  ) %>%
  top_n(1, presence_proportion) %>%# Select top 3 taxa for each combination of region and season
  print(n=32)

zp_long1 %>%
  group_by(Region, taxa) %>%
  summarise(
    total_samples = n_distinct(cruise_name),  # Count total number of distinct samples
    taxa_present_samples = sum(!is.na(abundance) & abundance > 0),  # Count samples where taxa is present
    presence_proportion = taxa_present_samples / total_samples  # Calculate proportion of samples where taxa is present
  ) %>%
  top_n(2, presence_proportion) %>%# Select top 3 taxa for each combination of region and season
  print(n=32)







fish_long1 %>%
  group_by(Region, season, taxa) %>%
  summarise(
    total_samples = n_distinct(cruise_name),  # Count total number of distinct samples
    taxa_present_samples = sum(!is.na(abundance) & abundance > 0),  # Count samples where taxa is present
    presence_proportion = taxa_present_samples / total_samples  # Calculate proportion of samples where taxa is present
  ) %>%
  top_n(1, presence_proportion) %>%# Select top 3 taxa for each combination of region and season
  print(n=32)


fish_long2 %>%
  group_by(Region, taxa) %>%
  summarise(
    total_samples = n_distinct(cruise_name),  # Count total number of distinct samples
    taxa_present_samples = sum(!is.na(abundance) & abundance > 0),  # Count samples where taxa is present
    presence_proportion = taxa_present_samples / total_samples  # Calculate proportion of samples where taxa is present
  ) %>%
  top_n(1, presence_proportion) %>%# Select top 3 taxa for each combination of region and season
  print(n=32)












## plots - relative abundances 
ggplot(data=zp_long1, 
       aes(x=year, y=abundance, fill = TAXA_NAME)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "RelatieAbundance (%)")+
  #scale_fill_manual(values = mycolors) +
  facet_grid(Region~season) +
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

## frequency of occurrence 
