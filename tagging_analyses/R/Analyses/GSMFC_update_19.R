####  GSMFC Tagging Update
####  10/10/2019
####  Adam A. Kemberling


####  Load Packages  ####
library(lubridate)
library(tidyverse)
library(here)
library(ggforce)
library(scales)
library(rgdal)
library(sf)
library(rnaturalearth)
theme_set(theme_minimal())

# Spatial Polygons for Maps
usa_poly    <- ne_states(country = "united states of america") %>% st_as_sf()
mexico_poly <- ne_states(country = "mexico") %>% st_as_sf()

#Path to dropbox data
db_path <- "/Users/akemberling/Dropbox/csap_studies/tagging_analyses/Data"

####  Load Cleaned Data  ####
tag       <- read.csv(str_c(db_path, "/processed_data", "/tags_noduplicates.csv"))
recaps    <- read.csv(str_c(db_path, "/processed_data", "/recaps_no_duplicates.csv"))
both      <- read.csv(str_c(db_path, "/processed_data", "/Merge_Update.csv"))

#Least cost distance data
both_lc   <- read.csv(str_c(db_path, "/processed_data", "/allstate_update.csv"))

both_lc %>% ggplot(aes(lc_distance/1000)) + 
  geom_histogram() + 
  labs(x = "Distance Travelled Avoiding Land (km)", y = "Count")

both_lc %>% ggplot(aes(time_at_large)) + 
  geom_histogram() + 
  labs(x = "Time at Liberty (days)", y = "Count")

both_lc %>% mutate(movement_rate = (lc_distance/1000) / time_at_large) %>% 
  ggplot(aes(movement_rate)) + 
  geom_histogram() + 
  labs(x = "Movement Rate (km/day)", y = "Count")


####  Data prep  ####
both <- both_lc  %>%  
  filter(Tag_state != 0) %>% 
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
    )



####__####
####  Summary Statistics  ####


#What `Recapture Source` caught the crabs from each state
source_recaps <- both %>% 
  group_by(Tag_state, `Recapture Source`) %>%
  summarise(n = n())


source_recaps  %>% 
  ggplot(aes(x = Tag_state, y = n, fill = `Recapture Source`)) +
  geom_col(position = "fill") +
  labs(x = "Tag State", y = "Proportion of Recaptures") +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#source_recaps %>% write.table("clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

#Recreational or commercial for each state
sector_recaps <- both %>% 
  group_by(Tag_state, Sector) %>%
  summarise(n = n()) 

#sector_recaps %>% write.table("clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

sector_recaps %>% 
  ggplot(aes(Tag_state, y = n, fill = Sector)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Tag State", y = "Proportion of Recaptures") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Overall for each as a doughnut plot
both %>%
  group_by(Tag_state, Sector) %>% 
  count(Sector) %>%
  mutate(focus = ifelse(Sector == "recreational", 0.2, 0)) %>% 
  rename(Sector = Sector) %>% 
  ggplot() +
    geom_arc_bar(aes(x0 = 0, 
                     y0 = 0, 
                     r0 = 0.7, 
                     r = 1, 
                     amount = n, 
                     explode = focus,
                     fill = Sector), 
                 stat = "pie") +
    facet_wrap(~Tag_state, nrow = 3) +
    theme_void() + theme(legend.position = c(0.8, 0.175)) +
    labs(x = "", y = "")

#Alluvial Plot
both %>%
  gather_set_data(c("Sector", "Recapture Source")) %>%
  mutate(x = fct_relevel(x, c("Sector", "Recapture Source"))) %>% 
  ggplot(aes(x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = Sector), show.legend = FALSE, alpha = 0.3)  +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0) +
  theme_no_axes()



####  Region Shapes  ####
apalach    <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/apalachicola.shp"))    %>% mutate(region = "Apalachicola")
aransas    <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/aransas_bay.shp"))     %>% mutate(region = "Aransas/Copano Bays")
barataria  <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/barataria_bay.shp"))   %>% mutate(region = "Barataria")
calcasieu  <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/calcasieu_lake.shp"))  %>% mutate(region = "Lake Calcasieu")
cc_bay     <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/cc_bay.shp"))          %>% mutate(region = "Corpus Christi Bay/ULM")
galveston  <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/galveston_bay.shp"))   %>% mutate(region = "Galveston Bay")
sabine     <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/lake_sabine.shp"))     %>% mutate(region = "Sabine Lake")
lower_lm   <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/lower_lm.shp"))        %>% mutate(region = "Lower Laguna Madre")
matagorda  <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/matagorda_bay.shp"))   %>% mutate(region = "Matagorda Bay")
mobile     <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/mobile_bay.shp"))      %>% mutate(region = "Mobile Bay-EMSS")
msound     <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/ms_sound.shp"))        %>% mutate(region = "Pontchartrain-WMSS")
pcity      <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/panama_city.shp"))     %>% mutate(region = "Panama City")
sa_bay     <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/san_antonio_bay.shp")) %>% mutate(region = "San Antonio Bay")
stein      <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/steinhatchee.shp"))    %>% mutate(region = "Steinhatchee")
terrebonne <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/terrebonne_bay.shp"))  %>% mutate(region = "Terrebonne")
west_la    <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/western_la.shp"))      %>% mutate(region = "Vermilion-Teche")
wolf_bay   <- st_read(str_c(db_path, "/spatial_files", "/2019_area_extents", "/wolf_bay.shp"))        %>% mutate(region = "Perdido-Wolf-Pensacola")



# Join them into one object and then intersect with the data to give you the region they are associated with
# because they have the same column names (fields) you use rbind so the columns stack instead of adding new ones
all_regions <- rbind(
  apalach, aransas, barataria, calcasieu, cc_bay, galveston, 
  sabine, lower_lm, matagorda, mobile, msound, pcity, sa_bay, 
  stein, terrebonne, west_la, wolf_bay)


# # Export Shapefile
# sf::st_write(all_regions, dsn = str_c(db_path,  "/spatial_files", "/2019_area_extents", "/all_regions.shp"))

#Plot to check
ggplot() +
  geom_sf(data = usa_poly) +
  geom_sf(data = mexico_poly) +
  geom_sf(data = all_regions, aes(fill = region), alpha = 0.3) +
  coord_sf(xlim = c(-97.4, -83), ylim = c(26, 31)) +
  theme(legend.position = "bottom")


#Remove the individual polygons
rm(apalach, aransas, barataria, calcasieu, cc_bay, galveston, sabine, lower_lm, matagorda, 
   mobile, msound, pcity, sa_bay, stein, terrebonne, west_la, wolf_bay)






#### WGS 1984 Region Assignment - Fails for Steinhatchee region  ####

####  NOTE: st_intersection only adds regions to the ones that do overlap and removes records that don't (offshore crabs)
# Make sf_objects for the tagging locations
# Use intersection to label add region info to the ones that overlap
# Merge back to the full set to keep all records


#Label tagging Data
tag_starts <- st_as_sf(tag, coords = c("long_decdeg", "lat_decdeg"), crs = 4326, remove = FALSE) %>% 
  st_intersection(all_regions) 

#Label tagging region for recapture set
both_starts <- st_as_sf(both, coords = c("long_decdeg", "lat_decdeg"), crs = 4326, remove = FALSE) %>% 
  st_intersection(all_regions)

#Pull that dataframe to then add recapture regions
both_stops <- sf::st_drop_geometry(both_starts) %>% dplyr::select(-Name.1)


#Add recapture regions
all_regions_recaps <- all_regions %>% rename(recapture_region = region)
both_stops <- st_as_sf(both_stops, coords = c("Rlong", "Rlat"), crs = 4326, remove = FALSE) %>% st_intersection(all_regions_recaps)

#pull out dataframe for exporting
tags_labelled <- st_drop_geometry(tag_starts) %>% 
  dplyr::select(tag_number, tag_region = region)

tags_labelled <- left_join(tag, tags_labelled, by = "tag_number") %>% 
  mutate(tag_region = if_else(is.na(tag_region) ==  TRUE, "Gulf Crabs", tag_region),
         tag_region       = if_else(long_decdeg >= -84, "Steinhatchee", tag_region),  )


#Check tagging locations
ggplot(tags_labelled, aes(long_decdeg, lat_decdeg, color = tag_region)) +
  geom_point()


#Now do the same for the recapture set
both_labelled <- st_drop_geometry(both_stops) %>% 
  dplyr::select(tag_number, tag_region = region, recapture_region)

both_labelled <- left_join(both, both_labelled, by = "tag_number") %>% 
  mutate(
    tag_region       = if_else(is.na(tag_region) ==  TRUE,"Gulf Crabs", tag_region),
    #tag_region       = if_else(long_decdeg >= -84, "Steinhatchee", tag_region),                             
    recapture_region = if_else(is.na(recapture_region) ==  TRUE, "Gulf Crabs", recapture_region)
    )





#### Checking Steinhatchee region assignment  ####
both_labelled %>% filter(tag_region == "Gulf Crabs") %>% 
  st_as_sf(coords = c("long_decdeg", "lat_decdeg"), crs = 4326, remove = FALSE) %>% 
  ggplot() +
    geom_sf(aes(color = tag_region, fill = tag_region)) +
    geom_sf(data = all_regions, aes(fill = region), alpha = 0.3) +
    coord_sf(xlim = c(-98, -96), ylim = c(27, 28)) +
    ggtitle("Release Locations of Recaptured Crabs - WGS1984")

#WTf
both_labelled %>% 
  filter(tag_region == "Gulf Crabs") %>% 
  st_as_sf(coords = c("long_decdeg", "lat_decdeg"), crs = 4326, remove = FALSE) %>% 
  ggplot() + 
    geom_sf(data = all_regions, aes(fill = region), alpha = 0.3) +
    geom_sf(aes(color = tag_region, fill = tag_region)) +
    theme(legend.position = "bottom")


####__####
####  Troubleshooting Regions  ####

####  Albers Equal Area Conical Projection  ####
# Reference To Albers proj4: https://spatialreference.org/ref/esri/north-america-albers-equal-area-conic/
# Modified for gulf of Mexico using: http://gsp.humboldt.edu/OLM/Courses/GSP_418/PPT/X%20Choosing%20a%20Spatial%20Reference%20for%20the%20Gulf%20of%20Mexico.pptx
# This is also helpful

albers_gom <- "+proj=aea +lat_1=23 +lat_2=28 +lat_0=16 +lon_0=-88 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
all_regions_albers <- all_regions %>% st_transform(crs = albers_gom)
ggplot() + geom_sf(data = all_regions_albers, aes(fill = region))

#Tag locations
tag_albers <- st_as_sf(tag, coords = c("long_decdeg", "lat_decdeg"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = albers_gom) %>% 
  st_intersection(all_regions_albers) 

#Recapture Locations
both_starts_albers <- st_as_sf(both, coords = c("long_decdeg", "lat_decdeg"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = albers_gom) %>% 
  st_intersection(all_regions_albers)


#Chack start location assignment for tag and recapture sets
ggplot() + geom_sf(data = tag_albers, aes(color = region, fill = region))
ggplot() + geom_sf(data = both_starts_albers, aes(color = region, fill = region))


#Pull that dataframe to then add recapture regions
both_stops_albers <- sf::st_drop_geometry(both_starts_albers) %>% dplyr::select(-Name.1)

#Add recapture regions
all_regions_recaps_albers <- all_regions_albers %>% rename(recapture_region = region)

#
both_stops_albers <- st_as_sf(both_stops, coords = c("Rlong", "Rlat"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = albers_gom) %>% 
  st_intersection(all_regions_recaps_albers)

#pull out dataframe for exporting
tags_labelled_albers <- st_drop_geometry(tag_albers) %>% 
  dplyr::select(tag_number, tag_region = region)

tags_labelled_albers <- left_join(tag, tags_labelled_albers, by = "tag_number") %>% 
  mutate(tag_region = if_else(is.na(tag_region) ==  TRUE, "Gulf Crabs", tag_region),
         tag_region       = if_else(long_decdeg >= -84, "Steinhatchee", tag_region),  )


#Check tagging locations
ggplot(tags_labelled_albers, aes(long_decdeg, lat_decdeg, color = tag_region)) +
  geom_point()


#Now do the same for the recapture set
both_labelled_albers <- st_drop_geometry(both_stops_albers) %>% 
  dplyr::select(tag_number, tag_region = region, recapture_region)

both_labelled_albers <- left_join(both, both_labelled_albers, by = "tag_number") %>% 
  mutate(
    tag_region       = if_else(is.na(tag_region) ==  TRUE,       "Gulf Crabs", tag_region),
    tag_region       = if_else(long_decdeg >= -84, "Steinhatchee", tag_region),  
    recapture_region = if_else(is.na(recapture_region) ==  TRUE, "Gulf Crabs", recapture_region)
  )


####  Check Regions  ####
both_labelled_albers %>% 
  st_as_sf(coords = c("long_decdeg", "lat_decdeg"), crs = 4326, remove = FALSE) %>% 
  ggplot() +
  geom_sf(aes(color = tag_region, fill = tag_region)) +
  geom_sf(data = all_regions, aes(fill = region), alpha = 0.3) +
  ggtitle("Albers Conical - Tag Location Assignments")

#And also recapture region
both_labelled_albers %>% 
  st_as_sf(coords = c("Rlong", "Rlat"), crs = 4326, remove = FALSE) %>% 
  ggplot() +
  geom_sf(aes(color = recapture_region, fill = recapture_region)) +
  geom_sf(data = all_regions_recaps, aes(fill = recapture_region), alpha = 0.3) +
  ggtitle("Albers Conical - Recapture Location Assignments")


####__####

####  Final Checks  ####

#Tally tagging locations
tags_labelled %>% as.data.frame() %>% count(tag_region) %>% knitr::kable()
both_labelled %>% as.data.frame() %>% count(tag_region) %>% knitr::kable()
tags_labelled_albers %>% as.data.frame() %>% count(tag_region) %>% knitr::kable()
both_labelled_albers %>% as.data.frame() %>% count(tag_region) %>% knitr::kable()

#Pair all the recaptures with a line segment
ggplot() +
  #geom_sf(data = usa_poly) +
  #geom_sf(data = mexico_poly) +
  geom_point(data = both_labelled, aes(long_decdeg, lat_decdeg, color = tag_region, shape = "release location")) +
  geom_point(data = both_labelled, aes(Rlong, Rlat, color = recapture_region, shape = "recapture location")) +
  geom_segment(data = both_labelled, aes(x = long_decdeg, y = lat_decdeg, xend = Rlong, yend = Rlat), alpha = 0.1) +
  #coord_sf(xlim = c(-91, -87), ylim = c(29, 31)) +
  coord_sf(xlim = c(-98, -78), ylim = c(25, 31.25)) +
  ggtitle("Start and End Locations - WGS1984")


ggplot() +
  #geom_sf(data = usa_poly) +
  #geom_sf(data = mexico_poly) +
  geom_point(data = both_labelled_albers, aes(long_decdeg, lat_decdeg, color = tag_region, shape = "release location")) +
  geom_point(data = both_labelled_albers, aes(Rlong, Rlat, color = recapture_region, shape = "recapture location")) +
  geom_segment(data = both_labelled_albers, aes(x = long_decdeg, y = lat_decdeg, xend = Rlong, yend = Rlat), alpha = 0.1) +
  #coord_sf(xlim = c(-91, -87), ylim = c(29, 31)) +     #MS Sound
  coord_sf(xlim = c(-98, -78), ylim = c(25, 31.25)) +   #Full Gulf
  #coord_sf(xlim = c(-85.1, -83), ylim = c(29, 30.1)) +   #Stein
  ggtitle("Start and End Locations - Custom Albers")





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


bad_tags %>% 
  inner_join(both_labelled, by = "tag_number")



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


bad_recaps %>% 
  inner_join(both_labelled, by = "tag_number") %>% 
  ggplot() +
    #geom_sf(data = usa_poly) +
    #geom_sf(data = mexico_poly) +
    geom_point(aes(long_decdeg, lat_decdeg, color = "tag location", shape = tag_region)) +
    geom_point(aes(Rlong, Rlat, color = "recapture", shape = tag_region)) +
    geom_segment(aes(x = long_decdeg, y = lat_decdeg, xend = Rlong, yend = Rlat), alpha = 0.4, 
                 arrow = arrow(angle = 30, length = unit(.2, "cm"), type = "closed")) +
    #coord_sf(xlim = c(-91, -87), ylim = c(29, 31)) +
    coord_sf(xlim = c(-90, -80), ylim = c(29, 31)) +
    ggtitle("Highly Suspect Start and End Locations")
    

####__####



####  VERDICT  ####
# Both of these intersecitons are failing
# Once all-regions is exported as a single polygon the arcmap intersect works correctly
# do that, then load the data in here to assign "Gulf crabs" to the ones that don't intersect correctly.
# New Script loads in the data, merges it to full set and labels gulf crabs
# That script is titled "tagging_region_assignment.R"



# ####  Save csv files for mapping  ####
# write_csv(tags_labelled, str_c(db_path, "/processed_data", "/tags_clean_labelled.csv"))
# write_csv(both_labelled, str_c(db_path, "/processed_data", "/both_clean_labelled.csv"))
