####  SEAMAP COPUE Raster Creation
####  10/17/2019 - Works, but not used in paper
####  Adam A. Kemberling


####  Load Packages  ####
library(lubridate)
library(raster)
library(tidyverse)
library(rgdal)
library(sf)
library(here)
theme_set(theme_minimal())

#Path to dropbox data
db_path <- "/Users/akemberling/Dropbox/csap_studies/tagging_analyses/Data"

seamap <- read_csv(str_c(db_path, "/final_report_datasets", "/seamap_cpue_2019_analysis.csv"))

#Summary Stat Bins
cpue_bins <- seamap %>% 
  mutate(
   Start_Long = plyr::round_any(Start_Long, accuracy = .25),
    Start_Lat  = plyr::round_any(Start_Lat, accuracy  = .25)
         ) %>% 
  group_by(Start_Long, Start_Lat) %>% 
  summarise(station_number = n(),
            total_catch = sum(Sapidus_Catch, na.rm = T),
            catch_per_tow = sum(Sapidus_Catch, na.rm = T) / station_number) 

#Effort
seamap_effort <- rasterFromXYZ(cpue_bins[, c("Start_Long", "Start_Lat", "station_number")], 
              crs = "+proj=longlat +datum=WGS84 +no_defs") 
plot(seamap_effort)

#Catch / Effort
catch_per_effort <- rasterFromXYZ(cpue_bins[, c("Start_Long", "Start_Lat", "catch_per_tow")], 
              crs = "+proj=longlat +datum=WGS84 +no_defs") 
plot(catch_per_effort)

#Total Catch
total_catch <- rasterFromXYZ(cpue_bins[, c("Start_Long", "Start_Lat", "total_catch")], 
                                  crs = "+proj=longlat +datum=WGS84 +no_defs") 
plot(total_catch)


#Export Rasters
writeRaster(seamap_effort, 
            filename = str_c(db_path, "/spatial_files", "/seamap_rasters", "/seamap_effort_09_to_18"),
            overwrite = TRUE)
writeRaster(catch_per_effort, 
            filename = str_c(db_path, "/spatial_files", "/seamap_rasters", "/seamap_cpue_09_to_18"),
            overwrite = TRUE)
writeRaster(total_catch, 
            filename = str_c(db_path, "/spatial_files", "/seamap_rasters", "/seamap_total_crabs_09_to_18"),
            overwrite = TRUE)

