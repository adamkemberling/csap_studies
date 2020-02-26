####  Tagging Region Assignment Script
####  10/16/2019
####  Adam A. Kemberling

####  This script combines the new regions we want (added using spatial intersect in arcmap) to the 
# csv files we want to use for analyses


####  Load Packages  ####
library(tidyverse)
library(readxl)
library(here)
library(rgdal)
library(sf)
library(rnaturalearth)
theme_set(theme_minimal())

#Path to dropbox data
db_path <- "/Users/akemberling/Dropbox/csap_studies/tagging_analyses/Data"

####  Load Data  ####
#These excel files are created in arcmap
# They are the product of doing a spatial intersect between good tagging and recapture locations with the all_regions shapefile
# They are missing locations outside of those polygons that we want to keep and label as gulf Crabs

#Tagging Locations
tags_full <- read.csv(str_c(db_path, "/processed_data", "/tags_noduplicates.csv"))
tags_regions <- read.csv(str_c(db_path, "/processed_data", "/spatial_checks", "/goodtags_regions_intersect.csv")) %>% 
  select(tag_number, tag_region = region)

#Recapture start/end locations
#both_full <- read.csv(here::here("Data", "processed_data", "Merge_Update.csv"))
both_full <- read.csv(str_c(db_path, "/processed_data", "/allstate_update.csv")) #With least cost distances*****
both_region_starts <- read.csv(str_c(db_path, "/processed_data", "/spatial_checks", "/recaps_ends_regions_intersect.csv")) %>% 
  select(tag_number, tag_region = region)
both_region_ends <- read.csv(str_c(db_path, "/processed_data", "/spatial_checks", "/recaps_ends_regions_intersect.csv")) %>% 
  select(tag_number, recapture_region = region)

#Add the region information
tags_full <- tags_full %>% 
  left_join(tags_regions, by = "tag_number") %>% 
  mutate(tag_region = as.character(tag_region),
         tag_region = ifelse(is.na(tag_region) == T, "Gulf Crab", tag_region),
         tag_region = ifelse(long_decdeg >= -84, "Steinhatchee", tag_region)
         )

both_full <- both_full %>% 
  left_join(both_region_starts, by = "tag_number") %>% 
  left_join(both_region_ends, by = "tag_number") %>% 
  mutate(
    tag_region = as.character(tag_region),
    tag_region = ifelse(long_decdeg >= -84, "Steinhatchee", tag_region),
    recapture_region = as.character(recapture_region),
    tag_region = ifelse(is.na(tag_region) == T, "Gulf Crab", tag_region),
    recapture_region = ifelse(is.na(recapture_region) == T, "Gulf Crab", recapture_region)
  )
  
  
#Plot them to check
tags_full %>% ggplot(aes(long_decdeg, lat_decdeg, color = tag_region)) + geom_point() + ggtitle("Tagging Starting Locations")
both_full %>% ggplot(aes(long_decdeg, lat_decdeg, color = tag_region)) + geom_point() + ggtitle("Recapture Starting Locations")
both_full %>% ggplot(aes(Rlong, Rlat, color = recapture_region)) + geom_point() + ggtitle("Recapture Ending Locations")




#Add the sector information to the recapture data
both_full <- both_full %>% 
  mutate(
    Tag_state = fct_relevel(Tag_state, c("Texas", "Louisiana", "Mississippi", "Alabama", "Florida")),
    sector    = tolower(sector), 
    Sector    = if_else(str_detect(sector, "commercial")   == TRUE, "commercial",   "miscellaneous"),            
    Sector    = if_else(str_detect(sector, "comercial")    == TRUE, "commercial",   Sector),            
    Sector    = if_else(str_detect(sector, "commerical")   == TRUE, "commercial",   Sector),            
    Sector    = if_else(str_detect(sector, "commmercial")  == TRUE, "commercial",   Sector),            
    Sector    = if_else(str_detect(sector, "recreational") == TRUE, "recreational", Sector),
    Sector    = if_else(str_detect(sector, "recreatoinal") == TRUE, "recreational", Sector),
    Sector    = if_else(is.na(Sector) == TRUE, "unable to determine", Sector),
    Sector    = fct_relevel(Sector, c("commercial", "recreational", "miscellaneous", "unable to determine")),
    `Recapture Source` = if_else(Sector == "commercial"   & str_detect(sector, "shrimp") == TRUE, "commercial shrimper",   "miscellaneous"),
    `Recapture Source` = if_else(Sector == "commercial"   & str_detect(sector, "crab")   == TRUE, "commercial crabber",    `Recapture Source`),
    `Recapture Source` = if_else(Sector == "commercial"   & str_detect(sector, "fish")   == TRUE, "commercial fisherman",  `Recapture Source`),
    `Recapture Source` = if_else(Sector == "recreational" & str_detect(sector, "crab")   == TRUE, "recreational crabber",    `Recapture Source`),
    `Recapture Source` = if_else(Sector == "recreational" & str_detect(sector, "shrimp") == TRUE, "recreational shrimper",   `Recapture Source`),
    `Recapture Source` = if_else(Sector == "recreational" & str_detect(sector, "fish")   == TRUE, "recreational fisherman",  `Recapture Source`),
    `Recapture Source` = if_else(is.na(sector) == TRUE, "no information given", `Recapture Source`),
    `Recapture Source` = fct_relevel(`Recapture Source`, 
                                     c("commercial crabber", "commercial shrimper", "commercial fisherman", 
                                       "recreational crabber", "recreational shrimper", "recreational fisherman", 
                                       "miscellaneous", "no information given"))
  ) %>% select(-sector)
  
  
  
#Get rid of capitalization in the column names...
tags_full <- tags_full %>% rename_all(tolower)
both_full <- both_full %>% rename_all(tolower)


#Remove bad tag and recapture locations
####  Bad locations  ####
bad_tags <- tribble(
  ~tag_number,
  "P11457",
  "P11466",
  "P11448",
  "P11475",
  "P11477",
  "P11472",
  "P11455",
  "P11471",
  "P11493",
  "F18357"
  
)

bad_recaps <- tribble(
  ~tag_number,
  "X29921",
  "R31909",
  "Y22756",
  "Y22549",
  "R31297",
  "A16889",
  "M09981",
  "K27407",
  "K27648",
  "G25573",
  "G25520",
  "G25514",
  "E08360"  #Pontchartrain to Apalachicola
)
  

`%notin%` <- purrr::negate(`%in%`)

tags_clean <-tags_full %>% filter(tag_number %notin% bad_tags$tag_number)
tags_clean %>% ggplot(aes(long_decdeg, lat_decdeg, color = tag_region)) + geom_point() + ggtitle("Tagging Starting Locations")


recaps_clean <- both_full %>% filter(tag_number %notin% bad_recaps)
recaps_clean %>% ggplot(aes(long_decdeg, lat_decdeg, color = tag_region)) + geom_point() + ggtitle("Recapture Starting Locations")
  
#Export final working sets
write_csv(tags_clean, str_c(db_path, "/processed_data", "/tags_clean_final.csv"))
write_csv(recaps_clean, str_c(db_path, "/processed_data", "/tags_recaps_clean_final.csv"))


#Export to the final working set also because I'm tired of getting them mixed up 11/10/2019 AAK
write_csv(tags_clean, str_c(db_path, "/final_report_datasets", "/tags_clean_final.csv"))
write_csv(recaps_clean, str_c(db_path, "/final_report_datasets", "/tags_recaps_clean_final.csv"))

  
  
  
  
  
  
  