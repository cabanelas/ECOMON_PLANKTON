C:\Users\alexc\OneDrive\Desktop\MSI Backup 06072023\Documents\MITWHOI\1. RESEARCH\NES-LTER\01_20230531_ECOMON_Data\EcoMon_Euphausiid\ECOMON_JUN23 

downloaded EcoMon data version 3.8 on 31-MAY-2023
downloaded EcoMon data version 3.10 on SEP-2025

https://www.nodc.noaa.gov/archive/arc0143/0187513/3.3/data/0-data/: instructions on how to access this is on OneNote (search "ECOMON") 

raw:
- EcoMon_Planton_Data_v3_8
- EcoMon_TaxaNames_addedNames
- EcoMon_v3_7_wDateStrata_Isabel
- Ecomon shapefiles - there are two: EcomonStrata_v4 and EcomonStrata_v4b
- EMstrata_v4.mat

output:
* OCT 2025, I have reduced the num of output files and improved naming convention
- EcoMon_Planktn_Data_v3_8_withREGION.csv: created in ecomon_tidy_files.r
- EcoMon_Planktn_Data_v3_8_withRegionType.csv: created in ecomon_tidy_files.r
- EcoMon_Plankton_Data_v3_8_withALLSPF_INFO.csv: created in ecomon_ecomon_tidy_files.r ; has all shapefiles and all shapefile variables 
- EcoMon_Plank_Abu_m2_long_MAB.csv: created in ecomon_m2_csv_byRegion_NAsRm.r; has NAs removed 
- EcoMon_Plank_Abu_m2_long_SNE.csv: created in ecomon_m2_csv_byRegion_NAsRm.r; has NAs removed 
- EcoMon_Plank_Abu_m2_long_GOM.csv: created in ecomon_m2_csv_byRegion_NAsRm.r; has NAs removed 
- EcoMon_Plank_Abu_m2_long_GB.csv: created in ecomon_m2_csv_byRegion_NAsRm.r; has NAs removed 
- EcoMon_Plank_Abu_m3_wide.csv: created in ecomon_tidy_files.r
- EcoMon_Plank_Abu_m3_long.csv: created in ecomon_tidy_files.r
- EcoMon_Plank_Abu_m3_long_NAsRemoved.csv
- EcoMon_EUPHAUSIID_SNE_m3_long.csv: created in ecomon_tidy_files_cont.r
- EcoMon_EUPHAUSIID_MAB_m3_long.csv: created in ecomon_tidy_files_cont.r
- EcoMon_EUPHAUSIID_GOM_m3_long.csv: created in ecomon_tidy_files_cont.r
- EcoMon_EUPHAUSIID_GB_m3_long.csv: created in ecomon_tidy_files_cont.r 
- EcoMon_EUPHAUSIID_SNE_m3_wide.csv: created in ecomon_tidy_files_cont.r 
- EcoMon_Plank_Abu_m2_wide.csv: created in ecomon_tidy_files_m2.r
- EcoMon_Plank_Abu_m2_long.csv: created in ecomon_tidy_files_m2.r


R scripts:

1- 01_ecomon_preprocess_spatial.r (formerly: ecomon_tidy_files.r)
#added shapefile/polygon info to raw data; added full taxa names to data; fixed dates, added seasons 
Read csv (3files): EcoMon_Plankton_Data_v3_10.csv (raw ecomon data); EcomonStrata_v4.shp (raw ecomon shapefilesx2)
Output csv (1file) **update, now the output is simpler. just one output file with ALL the shapefile information**: EcoMon_Plankton_Data_v3_10_wStrataMeta.csv (located in raw folder)

Old version output csv: EcoMon_Plankton_Data_v3_8_withREGION.csv; EcoMon_Plankton_Data_v3_8_withRegionType.csv; EcoMon_Plankton_Data_v3_8_withALLSPF_INFO.csv; EcoMon_Plank_Abu_m3_long.csv; EcoMon_Plank_Abu_m3_wide.csv; EcoMon_Plank_Abu_m3_long_NAsRemoved.csv; (all in output folder)


2- 02_ecomon_tidy_subsets (formerly: ecomon_tidy_files_cont.r)
#creating csvs to use for eda and plot making; csvs creation; created long and wide format csvs; 
Read csv (2files): EcoMon_Plankton_Data_v3_10_wStrataMeta.csv; EcoMon_TaxaNames_v3_10_cleaned.csv
Output csv (manyfiles): EcoMon_Plankton_v3_10_abund100m3_long.csv; EcoMon_Plankton_v3_10_abund100m3_long_NAsRemoved.csv; 
EcoMon_v3_10_Euphausiid_SNE_m3_long.csv; and also euphausiid 100m3 ones by region; EcoMon_Plankton_v3_10_abund10m2_long.csv; 10m2 csvs by region 

Old version output csv (5): creation: EcoMon_EUPHAUSIID_SNE_m3_long.csv, EcoMon_EUPHAUSIID_MAB_m3_long.csv, EcoMon_EUPHAUSIID_GOM_m3_long.csv, EcoMon_EUPHAUSIID_GB_m3_long.csv, EcoMon_EUPHAUSIID_SNE_m3_wide.csv.  


DELETED -- 3- ecomon_tidy_files_m2.r -- DELETED
creating long and wide csv formats. CHANGED TO CSV WITH ALL SHAPEFILE STUFF
Uses following csv: EcoMon_Plankton_Data_v3_8_withALLSPF_INFO.csv (output), EcoMon_TaxaNames_addedNames.csv (raw) 
Output csv (2): EcoMon_Plank_Abu_m2_wide.csv; EcoMon_Plank_Abu_m2_long.csv 


DELETED -- 4- ecomon_eda_100m3.r: random EDA, not super helpful idont think -- DELETED
Uses following csv: EcoMon_Plank_Abu_m3_long.csv (output), EcoMon_Plank_Abu_m3_wide.csv (output)
No csv created 


DELETED -- 5- ecomon_m2_csv_byRegion_NAsRm.r -- DELETED
create csv with plankton abundance long format by region m2; NAs removed 
Uses following csv: EcoMon_Plank_Abu_m2_long.csv 
Output csv (4): EcoMon_Plank_Abu_m2_long_SNE.csv; EcoMon_Plank_Abu_m2_long_MAB.csv, EcoMon_Plank_Abu_m2_long_GOM.csv, EcoMon_Plank_Abu_m2_long_GB.csv


6- eda_euphausiid_SNE_100m3 (formerly: ecomon_euphaus_100m3_Cleaned: nice plots)
deleted the original non cleaned script of this


DELETED -- 7- ecomon_m2_EDA.R: nice plots -- DELETED


DELETED -- 8- m2_euphausiid_subset_csv_06JUL.r -- DELETED
creating csv with subset (removed low abund euphausiid); all regions; NAs removed; long format
Uses following csv: EcoMon_Plank_Abu_me_wide.csv, EcoMon_TaxaNames_addedNames.csv
Output csv: EcoMon_SubEUPHAU_AllRegions_m2_long.csv


MOVED -- 9- ecomon_map.R -- MOVED TO ECOMON_MAPS folder


DELETED -- 10- zooplankton_analysis_NOAAscript_ecoReport.R -- DELETED 
code from the ecomon report; untouched/raw as they published; could be helpful for anomalies, but have issues running script because their shapefiles arent available 

DELETED -- 11- megan_m2.R: most likely wrong anomalies. not very helpful script -- DELETED 

12- euphausiids_allregions_plot_10m2



#### STILL NEED TO CHECK::
anomalies_m2.R: most likely wrong..
anomalies_m2_v2.R: more anomalies code, maybe better than anomalies_m2.R
seasonal_anomalies
seasonal_anomalies_copy
- not sure what the difference is between these two 

EDA_SNE_ZP somewhat updated but could use some more tidying 
EDA_