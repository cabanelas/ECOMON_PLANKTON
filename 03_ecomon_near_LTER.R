################################################################################
#############             EcoMon ZP DATA           #############################
################################################################################
## by: Alexandra Cabanelas
################################################################################
## Identifying EcoMon samples collected near the NES-LTER line (2018–present)
##
## Approach: spatial proximity filter
##   - Uses NES-LTER station coordinates as reference points
##   - Finds EcoMon samples within a defined radius of ANY LTER station
##   - Saves matched samples to output CSV
##
## Input:  raw/EcoMon_Plankton_Data_v3_10_wStrataMeta.csv  (from script 01)
##         raw/NES_LTER_station_coordinates.csv
## Output: output/EcoMon_near_LTER_2018present_long.csv
##
## created MAY 2026

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)
library(sf)

## ------------------------------------------ ##
#            Parameters -----
## ------------------------------------------ ##
# Search radius around each LTER station (meters)
# ~50 km = nearby EcoMon stations without pulling GB or MAB
RADIUS_M <- 50000  # 50 km

# Year cutoff: NES-LTER program started 2018
YEAR_MIN  <- 2018

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
eco <- read.csv(
  file.path("raw", "EcoMon_Plankton_Data_v3_10_wStrataMeta.csv"),
  header = TRUE
)

lter <- read.csv(
  file.path("raw", "NES_LTER_station_coordinates.csv"),
  header = TRUE
)

## ------------------------------------------ ##
#            Tidy / Checks -----
## ------------------------------------------ ##
# Check expected columns are present
stopifnot(all(c("lat", "lon", "year", "Region") %in% names(eco)))
stopifnot(all(c("Lat_DD", "Lon_DD", "Station") %in% names(lter)))

# Fix LTER longitudes: stored as positive in the CSV, need to be negative
if (all(lter$Lon_DD > 0)) {
  message("Negating LTER longitudes (converting to degrees West)")
  lter <- lter %>% mutate(Lon_DD = -Lon_DD)
}

# Filter EcoMon to 2018–present
eco_recent <- eco %>% filter(year >= YEAR_MIN)
message(glue::glue(
  "EcoMon rows (all years):    {nrow(eco)}\n",
  "EcoMon rows ({YEAR_MIN}+): {nrow(eco_recent)}"
))

## ------------------------------------------ ##
#            Convert to sf objects -----
## ------------------------------------------ ##
# Project to a meters-based CRS for distance calculations
# EPSG:32619 = UTM Zone 19N, appropriate for NW Atlantic
CRS_M <- 32619

eco_sf <- st_as_sf(eco_recent,
                   coords = c("lon", "lat"),
                   crs = 4326) %>%
  st_transform(CRS_M)

lter_sf <- st_as_sf(lter,
                    coords = c("Lon_DD", "Lat_DD"),
                    crs = 4326) %>%
  st_transform(CRS_M)

## ------------------------------------------ ##
#   Find EcoMon samples within radius of LTER -----
## ------------------------------------------ ##
# For each EcoMon sample, find which (if any) LTER stations are within radius
# Returns a sparse list: index i has the LTER station indices near eco sample i
near_lter <- st_is_within_distance(eco_sf, lter_sf, dist = RADIUS_M)

# Logical index: EcoMon samples that are near at least one LTER station
eco_near_idx <- lengths(near_lter) > 0
message(glue::glue(
  "{sum(eco_near_idx)} EcoMon samples within {RADIUS_M/1000} km of any LTER station\n",
  "({nrow(eco_recent) - sum(eco_near_idx)} outside radius)"
))

# Subset EcoMon data
eco_near_lter <- eco_recent[eco_near_idx, ]

## ------------------------------------------ ##
#   Add distance to nearest LTER station -----
## ------------------------------------------ ##
# Useful for QC and downstream filtering
nearest_idx <- st_nearest_feature(eco_sf[eco_near_idx, ], lter_sf)

eco_near_lter <- eco_near_lter %>%
  mutate(
    nearest_LTER_station = lter$Station[nearest_idx],
    nearest_LTER_name    = lter$Name[nearest_idx],
    dist_to_nearest_LTER_km = as.numeric(
      st_distance(
        eco_sf[eco_near_idx, ],
        lter_sf[nearest_idx, ],
        by_element = TRUE
      )
    ) / 1000
  )

## ------------------------------------------ ##
#   Optional: check overlap with SNE region -----
## ------------------------------------------ ##
# LTER line is within SNE, so most matches should be SNE
# Flag any non-SNE matches for review
region_check <- eco_near_lter %>%
  count(Region, sort = TRUE)

message("\nRegion breakdown of matched EcoMon samples:")
print(region_check)

non_SNE <- eco_near_lter %>% filter(Region != "SNE")
if (nrow(non_SNE) > 0) {
  message(glue::glue(
    "\n{nrow(non_SNE)} non-SNE samples within {RADIUS_M/1000} km of LTER — ",
    "check 'output/EcoMon_near_LTER_nonSNE_review.csv'"
  ))
  write.csv(non_SNE,
            "output/EcoMon_near_LTER_nonSNE_review.csv",
            row.names = FALSE)
}

## ------------------------------------------ ##
#   Summary -----
## ------------------------------------------ ##
message("\n--- Summary ---")
message(glue::glue("Year range:   {min(eco_near_lter$year)} – {max(eco_near_lter$year)}"))
message(glue::glue("Unique cruises: {n_distinct(eco_near_lter$cruise_name)}"))
message(glue::glue(
  "Distance range to nearest LTER: ",
  "{round(min(eco_near_lter$dist_to_nearest_LTER_km), 1)} – ",
  "{round(max(eco_near_lter$dist_to_nearest_LTER_km), 1)} km"
))
message(glue::glue("Total matched rows: {nrow(eco_near_lter)}"))

eco_near_lter %>%
  count(nearest_LTER_station, nearest_LTER_name) %>%
  arrange(nearest_LTER_station)

## ------------------------------------------ ##
#   Quick diagnostic map -----
## ------------------------------------------ ##
library(ggOceanMaps)

# Re-attach lat/lon for mapping (eco_near_lter is still a plain df)
plot_df <- eco_near_lter  # already has lat/lon cols from eco_recent

basemap(limits = c(-73, -68, 38.5, 42.5), bathymetry = TRUE) +
  geom_point(data = plot_df,
             aes(x = lon, y = lat, color = dist_to_nearest_LTER_km),
             size = 1.8, alpha = 0.7) +
  geom_point(data = lter %>% mutate(Lon_DD_plot = Lon_DD),  # already negated
             aes(x = Lon_DD_plot, y = Lat_DD),
             shape = 4, size = 3, color = "red", stroke = 1.2) +
  scale_color_viridis_c(name = "Dist to\nLTER (km)") +
  labs(
    title = glue::glue("EcoMon samples within {RADIUS_M/1000} km of NES-LTER ({YEAR_MIN}+)"),
    subtitle = glue::glue("n = {nrow(plot_df)} samples | Red X = LTER stations"),
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

## ------------------------------------------ ##
#   Export -----
## ------------------------------------------ ##
write.csv(
  eco_near_lter,
  glue::glue("output/EcoMon_near_LTER_{RADIUS_M/1000}km_{YEAR_MIN}present.csv"),
  row.names = FALSE
)

message(glue::glue(
  "\nSaved: output/EcoMon_near_LTER_{RADIUS_M/1000}km_{YEAR_MIN}present.csv"
))
