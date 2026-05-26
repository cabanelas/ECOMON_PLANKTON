################################################################################
#############             EcoMon ZP DATA           #############################
################################################################################
## by: Alexandra Cabanelas 
################################################################################
## Adding regions to data from polygon shapefile data
## Adding full taxa names to data

## created JUN 2023; updated OCT 2025

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)
library(sf)
library(ggOceanMaps)

# to make all columns viewable
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
eco <- read.csv(file.path("raw",
                          "EcoMon_Plankton_Data_v3_10.csv"), #EcoMon_Plankton_Data_v3_8
                header = T) #Data available for cruises through 2023

## ------------------------------------------ ##
#            Tidy -----
## ------------------------------------------ ##
# in v3_8 (when I created this script), colnames were lowercase
#so change v3_10 accordingly
colnames(eco) <- tolower(colnames(eco))

# tidy date info
eco <- eco %>%
  # create day, month, year cols based on date col 
  separate(date, into = c("day", "month", "year"), sep = "-", 
           remove = FALSE) %>%
  mutate(
    # create numeric month col
    monthNum = match(month, month.abb),
    year = as.numeric(year),
    # add 4 digit yr to year column
    year = ifelse(year > 50, year + 1900, year + 2000),
    # create season col
    season = case_when(
      between(monthNum, 3, 5) ~ "Spring",
      between(monthNum, 6, 8) ~ "Summer",
      between(monthNum, 9, 11) ~ "Fall",
      TRUE ~ "Winter"
    )
  ) %>%
  # reorder columns: monthNum after month
  relocate(monthNum, .after = month)

## ------------------------------------------ ##
#   Add region information from shapefiles -----
## ------------------------------------------ ##
# EcoMon data comes with 2 shapefiles: EcomonStrata_v4 & EcomonStrata_v4b

# the main difference I see is that v4b contains just NES relevant locations 
#whereas v4 extends down to florida and includes more areas offshelf and to the
#north/northeast of Gulf of Maine
#P-town coast not included in v4b
#in general v4 includes more coastal zones 

# There currently are 47 ECOMON strata used to randomly select plankton stations
#from Cape Hatteras, North Carolina, to Cape Sable, Nova Scotia

#Stratum 1-13 = MAB; Stratum 14-25 = SNE; Stratum 26-32 = GB; Stratum 32+ = GOM

# ------  EcomonStrata_v4 = 
# An additional 59 strata were created to encompass sampling from past programs 
#and potential future sampling work along the entire US east coast.
# These strata extend from Miami, Florida to the Scotian Shelf, and off the shelf edge
#out to 3000-m of depth.
# Naming of the strata = numeric 1 to 106, except for two strata that are split
#into two polygons. 
# Strata 38 and 48 are split and the names were changed to distinguish E & W polygons.
#381 = 38 east
#382 = 38 west
#481 = 48 east
#482 = 48 west
# 108 total polygons
ecomap1 <- st_read(file.path("raw", 
                             "EcomonStrata_v4.shp")) 
st_layers(file.path("raw", "EcomonStrata_v4.shp"))
!st_is_valid(ecomap1)
#invalid geometries
plot(ecomap1[!st_is_valid(ecomap1), ])
#valid geometries
plot(ecomap1[st_is_valid(ecomap1), ])

# ------  EcomonStrata_v4b = 
# Naming of the strata = numeric 1 to 47, except for one stratum that is split into
#two polygons. 
# Strata 38 is split and the name was changed to distinguish the E & W polygons.
#381 = 38 east
#382 = 38 west
# 48 total polygons
ecomap2 <- st_read(file.path("raw", 
                             "EcomonStrata_v4b.shp")) 
st_layers(file.path("raw", "EcomonStrata_v4b.shp"))

# combine shapefiles
ecomap <- rbind(ecomap1, ecomap2)

names(ecomap)
# set crs, none provided in shpfl
st_crs(ecomap) <- 4326

ecomap_valid <- st_make_valid(ecomap)
# convert plankton data to sf object (creates geometry col)
eco_sf <- st_as_sf(eco, 
                   coords = c("lon", "lat"), 
                   crs = 4326)  # WGS84

plot(st_geometry(ecomap_valid), col = "lightgray")
plot(st_geometry(eco_sf), add = TRUE, col = "blue", pch = 16) #can also use eco_joined

#eco_joined <- st_join(eco_sf, ecomap_valid[, "Region"], left = FALSE)
#eco_joined <- eco_sf %>% mutate(Region = ecomap_valid$Region[st_nearest_feature(eco_sf, ecomap_valid)])
#eco_joined <- eco_sf %>% bind_cols(ecomap_valid[st_nearest_feature(eco_sf, ecomap_valid), ])

# spatially join each plankton sample to its nearest stratum polygon
eco_joined <- eco_sf %>%
  bind_cols(
    ecomap_valid %>%
      st_drop_geometry() %>% # drop geometry to avoid duplication
      slice(st_nearest_feature(eco_sf, ecomap_valid))
  )

sum(is.na(eco_joined$Region)) # check for any unmatched samples (should be 0)
table(eco_joined$Region)

# regain lat and lon cols and remove geometry col
eco_final <- eco_joined %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>% # drop geometry for tabular output
  relocate(season:lat, .after = depth) # reorder columns

## ------------------------------------------ ##
#   Export CSV -----
## ------------------------------------------ ##

## keeping all info from shapefile
#write.csv(eco_final, "raw/EcoMon_Plankton_Data_v3_10_wStrataMeta.csv", row.names = FALSE)

## ------------------------------------------ ##
## ------------------------------------------ ##
#   Maps of NES -----
## ------------------------------------------ ##
# map: regions colored, black points
ggplot() +
  geom_sf(data = ecomap_valid, aes(fill = Region), 
          color = "white", alpha = 0.4) +
  geom_sf(data = eco_joined, color = "black", size = 0.5) +
  theme_minimal()
# map: regions gray, points colored by region, zoomed-in
ggplot() +
  geom_sf(data = ecomap_valid, fill = "grey90", color = "white") +  
  geom_sf(data = eco_joined, aes(color = Region), size = 2.2) +     
  #scale_color_viridis_d(option = "C") +          
  coord_sf(ylim = c(34, 45)) + 
  theme_minimal() +
  labs(color = "Region")

# condense regions to plot 1 general polygon 
region_polygons <- ecomap_valid %>%
  group_by(Region) %>%
  summarise(.groups = "drop")
# map: regions merged colored gray, points colored by region, zoomed-in
ggplot() +
  geom_sf(data = region_polygons, fill = "grey90", color = "white") +
  geom_sf(data = eco_joined, aes(color = Region), size = 2.2) +
  coord_sf(ylim = c(34, 45)) + 
  theme_minimal() +
  labs(color = "Region")

# map: points colored by region, U.S. land included, bathymetry 
basemap(data = eco_final, bathymetry = TRUE) +
  geom_point(data = eco_final, aes(x = lon, y = lat, color = Region))

# map: NES regions with EcoMon strata
NESstrata <- ecomap_valid %>%
  filter(Region %in% c("SNE", "GOM", "GB", "MAB"))

basemap(data = NESstrata, bathymetry = FALSE) +
  geom_sf(data = NESstrata, aes(color = Region), fill = NA, linewidth = 0.9) +
  geom_sf_text(data = NESstrata, aes(label = Name), size = 3) + 
  theme_minimal()

# map: EcoMon INSHORE strata
inshorestrata <- eco_final %>% filter(Name == 482 |
                                      Name == 481 | 
                                      Name == 45  |
                                      Name == 46)

basemap(limits = c(-75, -60, 39, 47)) +
  geom_point(data = inshorestrata, aes(x = lon, y=lat,
                              color = as.factor(Name)),
             size = 2,
             alpha = 0.2) + 
  theme_minimal()

# map: EcoMon OFFSHELF strata
offshelfstrata <- eco_final %>% filter(Name > 47 &
                          Name < 200)

basemap(data = offshelfstrata) +
  geom_point(data = offshelfstrata, aes(x = lon, y=lat,
                              color = as.factor(Name)),
             size = 2)

# map: comparing EcoMon stations onshelf vs offshelf
onvsoff <- eco_final %>%
  mutate(gr = case_when(Name > 47 & Name < 200 ~ "b",
                        TRUE ~ "a"))
onvsoff$gr <- factor(onvsoff$gr,levels=c("a","b"))

basemap(data = onvsoff) +
  geom_point(data = onvsoff, aes(x = lon, y=lat,
                                color = as.factor(gr),
                                alpha = gr),
             size = 2
  ) +
  scale_alpha_manual(values=c(0.01,0.8))

################################################################
# examining strange MAB & CC boundary polygon
eco_joined2 <- eco_joined %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

eco_check <- eco_joined2 %>%
  filter(lon > -80, lon < -73, lat > 33, lat < 39)   

eco_check %>%
  st_drop_geometry() %>%
  count(Region)

ggplot() +
  geom_sf(data = ecomap_valid, fill = "grey90", color = "grey40") +
  geom_sf(data = eco_check, aes(color = Region), size = 2) +
  coord_sf(xlim = c(-80, -73), ylim = c(33, 39), expand = FALSE) +
  theme_minimal() +
  scale_color_manual(values = c("MAB" = "cyan", "CC" = "magenta")) +
  theme(legend.position = "bottom")

cc_pts <- eco_joined %>% filter(Region == "CC")
idx <- st_intersects(cc_pts, ecomap_valid, sparse = TRUE)

poly_names <- sapply(idx, function(i) paste(unique(ecomap_valid$Region[i]), collapse = ", "))
data.frame(
  point_id = seq_along(poly_names),
  assigned_region = cc_pts$Region,
  polygon_region  = poly_names,
  lon = st_coordinates(cc_pts)[,1],
  lat = st_coordinates(cc_pts)[,2]
)
table(assigned = cc_pts$Region, actual = poly_names)

st_crs(ecomap1) <- 4326
st_crs(ecomap2) <- 4326
sf::sf_use_s2(FALSE)

ggplot() +
  geom_sf(data = ecomap2, fill = "grey90", color = "black", alpha = 0.5) +
  geom_sf(data = cc_pts, color = "deeppink", size = 2) +
  coord_sf(xlim = c(-76, -74.5), ylim = c(34, 36)) +
  theme_minimal() +
  labs(title = "EcomonStrata_v4b Polygons")

ecomap2_subset <- ecomap2 %>% filter(Region %in% c("CC", "MAB"))

ggplot() +
  geom_sf(data = ecomap2_subset, aes(fill = Region), color = "white", alpha = 0.4) +
  geom_sf(data = cc_pts, color = "deeppink", size = 2) +
  coord_sf(xlim = c(-76, -74.5), ylim = c(34, 36)) +
  theme_minimal() +
  labs(title = "EcomonStrata_v4b Polygons")

ggplot() +
  geom_sf(data = ecomap1, fill = "grey90", color = "black", alpha = 0.5) +
  geom_sf(data = cc_pts, color = "deeppink", size = 2) +
  coord_sf(xlim = c(-76, -74.5), ylim = c(34, 36)) +
  theme_minimal() +
  labs(title = "EcomonStrata_v4 Polygons")

ecomap1_subset <- ecomap1 %>% filter(Region %in% c("CC", "MAB"))

ggplot() +
  geom_sf(data = ecomap1_subset, aes(fill = Region), color = "white", alpha = 0.4) +
  geom_sf(data = cc_pts, color = "deeppink", size = 2) +
  coord_sf(xlim = c(-76, -74.5), ylim = c(34, 36)) +
  theme_minimal() +
  labs(title = "EcomonStrata_v4 Polygons")

eco_mab <- eco_joined %>% filter(Region == "MAB")
eco_other <- eco_joined %>% filter(Region == "CC")

ggplot() +
  geom_sf(data = region_polygons, fill = "grey90", color = "white") +
  geom_sf(data = eco_other, aes(color = Region), size = 1.2, alpha = 0.6) +
  geom_sf(data = eco_mab, color = "blue", size = 1.2) +
  coord_sf(xlim = c(-78, -74), ylim = c(33, 37)) +
  theme_minimal()

# find CC points within 10 km of any MAB point
near_mab <- st_is_within_distance(eco_other, eco_mab, dist = 10000)  # 10 km

# extract points
eco_near_mab <- eco_other[lengths(near_mab) > 0, ]

ggplot() +
  geom_sf(data = region_polygons, fill = "grey90", color = "white") +
  geom_sf(data = eco_mab, color = "blue", size = 1.2) +
  geom_sf(data = eco_near_mab, aes(color = Region), size = 1.2) +
  geom_sf_text(data = eco_near_mab, aes(label = station), size = 3, check_overlap = TRUE) +
  coord_sf(xlim = c(-78, -74), ylim = c(33, 37)) +
  theme_minimal()