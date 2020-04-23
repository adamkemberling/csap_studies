# Getting to Sex Ratios from GSMFC data

####  Load Packages  ####
library(here)
library(tidyverse)
library(readxl)
library(odbc)
library(dbplyr)
library(ggridges)

name_match <- function(df1, df2) {names(df1)[which(names(df1) %in% names(df2))]}

# #Load and format data - Access Database Method
# mypath <- "/Users/adamkemberling/Dropbox/csap_studies/seamap_analyses/data/GSMFC_raw/ms_access_db/public_seamap.mdb"
# mycon <- dbConnect(odbc::odbc(),
#                    #DBQ = here("data/ms_access_db", "public_seamap.mdb"),
#                    DBQ = mypath,
#                    Driver = "{Microsoft Access Driver (*.mdb, *.accdb)};")
# 
# # # To disconnect database :
# # dbDisconnect(mycon)
# 
# # Catch Data
# bgsrec <- tbl(mycon, "BGSREC") %>% collect() %>%  rename_all(tolower)
# bgsrec <- bgsrec %>% rename(biocode = bio_bgs)
# 
# # Cruise Data
# cruises <- tbl(mycon, "CRUISES") %>% collect() %>% rename_all(tolower)
# cruises <- cruises %>%
#   select(vessel, cruiseid, cruise_no,  yr, title, source) %>% 
#   filter(str_detect(tolower(title), "groundfish"), 
#          str_detect(tolower(title), "fall") | str_detect(tolower(title), "summer"))
# 
# # Indv. Measurements
# glfrec <- tbl(mycon, "GLFREC") %>% collect() %>% rename_all(tolower)
# glfrec <- glfrec %>% 
#   select(vessel,
#          cruise_no,
#          stationid,
#          p_sta_no,
#          biocode = bio_glf,
#          indvl_wt,
#          meas_cd = meascd_glf,
#          indvl_len = len_glf,
#          indvl_sex = sex_glf)
# 
# new_biocodes <- tbl(mycon, "NEWBIOCODESBIG") %>% collect() %>% rename_all(tolower)
# new_biocodes <- new_biocodes %>% select(
#   biocode = code,
#   taxonomic,
#   common_name
# )

####  Load Data  ####

#Data import, data updated March 31st 2020 from GSMFC, downloaded again April, 2020
bgsrec <- read_csv("~/Dropbox/csap_studies/seamap_analyses/data/GSMFC_raw/BGSREC.csv", 
                   col_types = cols(), 
                   guess_max = 1e5) %>%  
  rename_all(tolower) %>% 
  rename(biocode = bio_bgs)

cruises <- read_csv("~/Dropbox/csap_studies/seamap_analyses/data/GSMFC_raw/CRUISES.csv", 
                    col_types = cols(), 
                    guess_max = 1e5) %>%
  rename_all(tolower) %>% 
  select(vessel, cruiseid, cruise_no,  yr, title, source) %>% 
  filter(str_detect(tolower(title), "groundfish"), 
         str_detect(tolower(title), "fall") | str_detect(tolower(title), "summer"))

#GLFREC has SEX of catch
glfrec <- read_csv("~/Dropbox/csap_studies/seamap_analyses/data/GSMFC_raw/GLFREC.csv", 
                   col_types = cols(), guess_max = 1e5) %>% 
  rename_all(tolower)  %>% 
    select(vessel,
           cruise_no,
           stationid,
           p_sta_no,
           biocode = bio_glf,
           indvl_wt,
           meas_cd = meascd_glf,
           indvl_len = len_glf,
           indvl_sex = sex_glf)

# Biocodes
new_biocodes <- read_csv("~/Dropbox/csap_studies/seamap_analyses/data/GSMFC_raw/NEWBIOCODESBIG.csv", 
                         col_types = cols(), guess_max = 1e5) %>% 
  rename_all(tolower) %>% 
  select(biocode = code, taxonomic, common_name)





# Match biocodes to stations
name_match(bgsrec, new_biocodes)
bgsrec <- left_join(bgsrec, new_biocodes, by = "biocode")




####  Perform Joins  ####
name_match(glfrec, bgsrec)
bgs_glf <- bgsrec %>% 
  full_join(glfrec, by = c("vessel", "cruise_no",  "stationid", "p_sta_no", "biocode")) %>% 
  select(vessel, cruiseid, cruise_no, stationid, p_sta_no,
         biocode, genus_bgs, spec_bgs, common_name, bgscode, cnt, cntexp, sample_bgs, select_bgs, nodc_bgs, is_sample,
         invrecid, indvl_wt, indvl_len, meas_cd, indvl_sex) %>% 
  mutate(cruiseid = as.character(cruiseid),
         vessel = as.character(vessel),
         cruise_no = as.character(cruise_no))

# Contains all species
name_match(bgs_glf, cruises)
cruises <- cruises %>% mutate(cruiseid = as.character(cruiseid), cruise_no = as.character(cruise_no))
bgs_glf_cruises <- bgs_glf %>% right_join(cruises, by = c("vessel", "cruiseid", "cruise_no")) %>% 
  mutate(yr = forcats::fct_rev(as.character(yr)))





#### Filter Stations  ####

seamap_west <- read_csv(str_c("~/Dropbox/csap_studies/seamap_analyses/data/offshore/", 
                              "seamapwest_manuscriptmods_full.csv"), 
                        guess_max = 1e4,
                        col_types = cols())


# Filter to stations in model
bgs_glf_analysis <- bgs_glf_cruises %>% 
  filter(stationid %in% seamap_west$stationid,
         spec_bgs == "SAPIDU") %>% 
  mutate(yr = as.numeric(as.character(yr)))



####  Summary Statistics - Western Gulf  ####
library(magrittr)

#Sex Ratios for analysis data set
bgs_glf_analysis <- bgs_glf_analysis %>% 
  mutate(indvl_sex = ifelse(indvl_sex %in% c("F", "f"), "Female", indvl_sex),
         indvl_sex = ifelse(indvl_sex %in% c("M", "m"), "Male", indvl_sex),
         indvl_sex = ifelse(indvl_sex %in% c("Female", "Male"), indvl_sex, NA))

# Sex rations
bgs_glf_analysis %>%  
  count(indvl_sex)

# percent of known* females and males
bgs_glf_analysis %>% 
  summarise(n_total = n(),
            n_sexed = sum(is.na(indvl_sex) == F),
            Females = sum(indvl_sex == "Female", na.rm = T),
            Males = sum(indvl_sex == "Male", na.rm = T),
            `Perc F of total` = (Females/n_total) * 100,
            `Perc F of n_sexed` = (Females/n_sexed) * 100,
            `Perc M of total` = (Males/n_total) * 100,
            `Perc M of n_sexed` = (Males/n_sexed) * 100,
            )




# quartile ranges
quantile(bgs_glf_analysis$indvl_len, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)



# Yearly ridgeplots
bgs_glf_analysis %>% 
  ggplot(aes(y = yr, 
             x = indvl_len, 
             fill = factor(..quantile..))) +
    stat_density_ridges(geom = "density_ridges_gradient", 
                        calc_ecdf = TRUE, 
                        quantiles = c(0.025, 0.975)) +
    scale_fill_manual(
      name = "Probability", 
      values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
      labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))  +
    labs(title= "C. sapidus - Carapace Width frequencies", 
         y = "",
         x = "")
