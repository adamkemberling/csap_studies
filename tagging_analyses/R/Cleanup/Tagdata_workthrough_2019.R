# 2019 Blue Crab Tagging Data Analysis
# Redone with project directory and all relevant file paths within this directory 
# Adam A. Kemberling
# 9/18/2019


####  Packages  ####
library(raster)
library(readxl)
library(lubridate)
library(tidyverse)
library(here)


#### Load Data  ####

# IMPORT MOST RECENT TAGGING AND RECAPTURE DATA
tag    <- read_excel("L:/Dropbox (The Craboratory)/The Craboratory/projects_shared/tagging/tagging_data_master.xlsx")
recaps <- read_excel("L:/Dropbox (The Craboratory)/The Craboratory/projects_shared/tagging/Recapture_Reward_Documents/recapture_data.xlsx",
                     range = cell_rows(c(2, NA)))


####  Run Cleanup Function  ####
# THIS CREATES NEW CSV FILES CONTAINING TAG, RECAPTURE, MERGED RECAPTURES, NEGATIVE TIME AT LARGE CRABS, 
# DUPLICATE TAG NUMBERS, AND MULTIPLE RECAPTURES

source(here::here("Code", "Cleanup", "tagdata_cleanup_function_2019.R"))
starttime = Sys.time()
tag_cleanup(tag = tag, recaps = recaps)
Sys.time() - starttime


# Files are saved in project folder "2019_tagging_analysis/Data/processed_data" 
# 2019 Update no longer saves them with date so the last run is the last run



#-------------STEP THREE, IMPORT THE NEW, FORMATTED CSV FILES FOR ANALYSIS AND PLOT MAKING---------------
rm(list=ls()) #Removes everything from the environment to start fresh

tag    <- read.csv(here::here("Data", "processed_data", "tags_noduplicates.csv"))
recaps <- read.csv(here::here("Data", "processed_data", "recaps_no_duplicates.csv"))
both   <- read.csv(here::here("Data", "processed_data", "Merge_Update.csv"))



# Get summary statistics, and ggplot for visualizations  ----------------------------------------------------------

#number tagged, and number recaptured
tag  %>% count(Tag_state) #number tagged in each state
tag  %>% count(Tag_Basin) #number tagged in each basin
both %>% count(Tag_state) #number of recaptures from each tagging state 
both %>% count(Tag_state) #number of recaptures from each tagging state 

#What where recaptures took place based on tagging state 
both %>% filter(Tag_state == "Texas")       %>% count(Recap_state)          #TX
both %>% filter(Tag_state == "Louisiana")   %>% count(Recap_state)          #LA
both %>% filter(Tag_state == "Mississippi") %>% count(Recap_state)          #MS
both %>% filter(Tag_state == "Alabama")     %>% count(Recap_state)          #AL
both %>% filter(Tag_state == "Florida")     %>% count(Recap_state)          #FL

#check out travel rates, remove completely implausible ones i.e. >25km/day
summary(both$travelrate)
both <- both[which(both$travelrate < 25),]



#-------------------------------------------------- Least cost distances done by functions -------

library(gdistance)
library(rgdal)
library(maptools)
library(GISTools)
library(sp)
library(sf)
library(geosphere)
library(rnaturalearth)

# These are the functions to calculate lc distances for each geographic region
source(here("Code", "lc_distances", "AL_leastcost_function_2019.R"))
source(here("Code", "lc_distances", "FL_leastcost_function_2019.R"))
source(here("Code", "lc_distances", "LA_leastcost_function_2019.R"))
source(here("Code", "lc_distances", "MS_leastcost_function_2019.R"))
source(here("Code", "lc_distances", "TX_leastcost_function_2019.R"))

#start with Florida
starttime = Sys.time()
florida_lc(merge_dat = both)
Sys.time() - starttime

#now LA
#bring in new dataframe
fl <- read.csv(here::here("Data", "lc_distance_processing", "fl_update.csv"))
starttime = Sys.time()
louisiana_lc(merge_dat = fl)
Sys.time() - starttime

#now MS
#bring in new dataframe
la <- read.csv(here::here("Data", "lc_distance_processing", "la_update.csv"))
starttime = Sys.time()
mississippi_lc(merge_dat = la)
Sys.time() - starttime

#now AL
ms <- read.csv(here::here("Data", "lc_distance_processing", "ms_update.csv"))
starttime = Sys.time()
alabama_lc(merge_dat = ms)
Sys.time() - starttime

#lastly, TX
al <- read.csv(here::here("Data", "lc_distance_processing", "al_update.csv"))
starttime = Sys.time()
texas_lc(merge_dat = al)
Sys.time() - starttime



#put them all together
fl <- read.csv(here::here("Data", "lc_distance_processing", "fl_update.csv"))
la <- read.csv(here::here("Data", "lc_distance_processing", "la_update.csv"))
ms <- read.csv(here::here("Data", "lc_distance_processing", "ms_update.csv"))
al <- read.csv(here::here("Data", "lc_distance_processing", "al_update.csv"))
tx <- read.csv(here::here("Data", "lc_distance_processing", "tx_update.csv"))

fl <- fl[,c('tag_number','lc_distance')]
la <- la[,c('tag_number','lc_distance')]
ms <- ms[,c('tag_number','lc_distance')]
al <- al[,c('tag_number','lc_distance')]
tx <- tx[,c('tag_number','lc_distance')]

head(al[,'lc_distance']);head(la[,'lc_distance']);head(ms[,'lc_distance']); head(fl[,'lc_distance']);head(tx[,'lc_distance'])

fl[is.na(fl)] <- 0
la[is.na(la)] <- 0
ms[is.na(ms)] <- 0
al[is.na(al)] <- 0
tx[is.na(tx)] <- 0

lc_dist <- al[,'lc_distance'] + ms[,'lc_distance'] +la[,'lc_distance'] + fl[,'lc_distance'] +tx[,'lc_distance']


merge_dat <-  read.csv(here::here("Data", "processed_data", "Merge_Update.csv"))
merge_dat <-  merge_dat[which(merge_dat$travelrate < 25),]

merge_dat$lc_distance <- lc_dist
plot(lc_distance ~ Displacement_meters, data = merge_dat)



write_csv(merge_dat, here::here("Data", "processed_data", "allstate_update.csv"))
rm(list=ls())








