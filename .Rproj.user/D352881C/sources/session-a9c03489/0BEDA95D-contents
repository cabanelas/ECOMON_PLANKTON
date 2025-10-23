################################################################################
#############             EcoMon ZP DATA           #############################
################################################################################
## by: Alexandra Cabanelas 
################################################################################
## Creating long format csv's of georeferenced EcoMon data
## also creating csv's w euphausiid data only (18 taxa)
## files contain abundance in either 100m3 or 10m2 only

## created JUN 2023; updated OCT 2025

# version 3.8 was from ~2023
# now using version 3.10

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)

#to make all columns viewable
#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
eco <- read.csv(file.path("raw",
                          "EcoMon_Plankton_Data_v3_10_wStrataMeta.csv"),
                       header = T)

taxa_names <- read.csv(file.path("raw",
                                 "EcoMon_TaxaNames_v3_10_cleaned.csv"),
                       header = T)

## ------------------------------------------ ##
#      Volumetric abundance 100m3 -----
## ------------------------------------------ ##
## ------------------------------------------ ##
#      Export CSVs: long format w 100m3 -----
## ------------------------------------------ ##
## creating csv with just abundance in volume m3

eco_100m3 <- eco %>%
  select(
    cruise_name:btm_salt,  # metadata range
    ends_with("_100m3")
  )

# clean col names
names(eco_100m3) <- gsub("_100m3", "", names(eco_100m3)) 

# pivot longer
eco_100m3 <- eco_100m3 %>%
  pivot_longer(
    cols = ctyp:lopame,  
    names_to = "taxa",
    values_to = "ind_100m3"
  )

t_names <- taxa_names %>%
  filter(unit == "100m3") %>%
  select(taxa, TAXA_NAME)

abund_100m3 <- left_join(eco_100m3, t_names, by = "taxa")

anti_join(eco_100m3, t_names, by = "taxa") %>%
  distinct(taxa) # should be empty tibble

#write.csv(abund_100m3, "output/EcoMon_Plankton_v3_10_abund100m3_long.csv", row.names = FALSE)
# readr::write_csv should be faster 
# maybe could also use saveRDS(NAME, "FILENAME.rds")

# remove NAs - to make file smaller
abund_100m3_NAremoved <- abund_100m3[!is.na(abund_100m3$ind_100m3), ]
#write.csv(abund_100m3_NAremoved, "output/EcoMon_Plankton_v3_10_abund100m3_long_NAsRemoved.csv",
#          row.names = FALSE)

## ------------------------------------------ ##
# Export CSVs: long format w 100m3 EUPHAUSIIDS only -----
## ------------------------------------------ ##
eco_l <- abund_100m3

euph1 <- eco_l %>%
  filter(TAXA_NAME %in% c("EuphausiaceaMany", "Euphausiacea1", 
                          "Thysanoessa inermis", "Meganyctiphanes norvegica", 
                          "Thysanoessa raschii", "Thysanoessa longicaudata", 
                          "Euphausia americana", "Euphausia krohnii", 
                          "Euphausia spp.", "Thysanoessa gregaria", 
                          "Nematoscelis spp.", "Stylocheiron spp.", 
                          "Stylocheiron elongatum", "Nematoscelis megalops", 
                          "Thysanoessa spp.", "Thysanopoda acutifrons",
                          "Thysanoessa spinifera", "Nematobrachion boopis")) #18

#euph %>% distinct(Region)
### --- BY REGION
### --- SOUTHERN NEW ENGLAND REGION ----- 
SNE_l <- euph1 %>% filter(Region == "SNE")
#SNE_l %>% distinct(TAXA_NAME)
#write.csv(SNE_l, "output/EcoMon_v3_10_Euphausiid_SNE_m3_long.csv", row.names = FALSE)

### --- MID ATLANTIC BIGHT REGION ----- 
MAB_l <- euph1 %>% filter(Region == "MAB")
MAB_l %>% distinct(TAXA_NAME)
#write.csv(MAB_l, "output/EcoMon_v3_10_Euphausiid_MAB_m3_long.csv", row.names = FALSE)

### --- GULF OF MAINE REGION ----- 
GOM_l <- euph1 %>% filter(Region == "GOM")
GOM_l %>% distinct(TAXA_NAME)
#write.csv(GOM_l, "output/EcoMon_v3_10_Euphausiid_GOM_m3_long.csv",
#          row.names = FALSE) 

###  --- GEORGES BANK REGION ----- 
GB_l <- euph1 %>% filter(Region == "GB")
GB_l %>% distinct(TAXA_NAME)
#write.csv(GB_l, "output/EcoMon_v3_10_Euphausiid_GB_m3_long.csv", row.names = FALSE)

## ------------------------------------------ ##
# Export CSVs: wide format w 100m3 EUPHAUSIIDS only -----
## ------------------------------------------ ##
euph_taxa <- c("euph", "euph1", "thysin", "megan", "thysra", "thyslo", "eupham", 
               "euphkr", "euphspp", "thysgr", "nemaspp", "stylspp", "stylel", 
               "nemame", "thysspp", "shysac", "thypsp", "nemabo")
euph_cols <- paste0(euph_taxa, "_100m3")

### --- SOUTHERN NEW ENGLAND REGION ----- 
euph_wide_SNE <- eco %>%
  select(
    cruise_name:volume_1m2,          
    all_of(euph_cols)  
  ) %>%
  filter(Region == "SNE")

#write.csv(euph_wide_SNE, "output/EcoMon_v3_10_Euphausiid_SNE_m3_wide.csv", row.names = FALSE)

## ------------------------------------------ ##
#      Areal abundance 10m2 -----
## ------------------------------------------ ##
## ------------------------------------------ ##
#      Export CSVs: long format w 10m2 -----
## ------------------------------------------ ##
eco_10m2 <- eco %>%
  select(
    cruise_name:btm_salt,  # metadata range
    ends_with("_10m2")
  )

# clean col names
names(eco_10m2) <- gsub("_10m2", "", names(eco_10m2)) 

# pivot longer
eco_10m2 <- eco_10m2 %>%
  pivot_longer(
    cols = ctyp:lopame,
    names_to = "taxa",
    values_to = "ind_10m2"
  )

t_m2_names <- taxa_names %>%
  filter(unit == "10m2") %>%
  select(taxa, TAXA_NAME)

abund_10m2 <- left_join(eco_10m2, t_m2_names, by = "taxa")
anti_join(eco_10m2, t_m2_names, by = "taxa") %>%
  distinct(taxa) # should be empty tibble
#write.csv(abund_10m2, "output/EcoMon_Plankton_v3_10_abund10m2_long.csv", row.names = FALSE)


# remove NAs
abund_10m2 <- abund_10m2[!is.na(abund_10m2$ind_10m2),]

### --- BY REGION
### --- SOUTHERN NEW ENGLAND REGION ----- 
abund_10m2_SNE <- abund_10m2 %>% filter(Region == "SNE")
#write.csv(abund_10m2_SNE, "output/EcoMon_Plankton_v3_10_SNE_m2_long.csv", row.names = FALSE)

### --- MID ATLANTIC BIGHT REGION ----- 
abund_10m2_MAB <- abund_10m2 %>% filter(Region == "MAB")
#write.csv(abund_10m2_MAB, "output/EcoMon_Plankton_v3_10_MAB_m2_long.csv", row.names = FALSE)

### --- GULF OF MAINE REGION ----- 
abund_10m2_GOM <- abund_10m2 %>% filter(abund_10m2 == "GOM")
#write.csv(abund_10m2_GOM, "output/EcoMon_Plankton_v3_10_GOM_m2_long.csv", row.names = FALSE)

###  --- GEORGES BANK REGION ----- 
abund_10m2_GB <- abund_10m2 %>% filter(Region == "GB")
#write.csv(abund_10m2_GB, "output/EcoMon_Plankton_v3_10_GB_m2_long.csv", row.names = FALSE)
