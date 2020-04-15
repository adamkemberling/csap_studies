####  Exploring the use of colorist to map tagging and recapture 
####  distributions over time
####  4/6/2020


####  Load Packages  ####
library(here)
library(raster)
library(sf)
library(stars)
library(tidyverse)
library(colorist)
library(rnaturalearth)



####  Tagging Data  ####
#Path to dropbox data
db_path <- "~/Dropbox/csap_studies/tagging_analyses/Data"

####  Data  ####

# These files have had the regions we are interested in added and have had bad tags removed
# See the tagging_region_assignment.R script for details
tagging <- read_csv(str_c(db_path, "/final_report_datasets", "/tags_clean_final.csv"), 
                    col_types = cols(),
                    guess_max = 1e5) %>% rename_all(tolower)
both <- read_csv(str_c(db_path, "/final_report_datasets", "/tags_recaps_clean_final.csv"), 
                 col_types = cols(),
                 guess_max = 1e5) %>% rename_all(tolower)

# Spatial Polygons
usa_poly <- ne_states("united states of america") %>% st_as_sf() %>% filter(region == "South")
mexico_poly <- ne_states("mexico") %>% st_as_sf()



####  Data Setup  ####
# Colorist works on Raster Stack Objects.
# The demo vignette works on a single year

####  1. Making The Gulf Seasonal Raster Stack
# Going to use the same setup as the GOM Raster

#Custom grid for rasterize
test_bbox <- st_bbox(c(xmin = -97.5, xmax = -81,
                       ymin = 24.3, ymax = 30.5),
                     crs = st_crs(4326))

num_x <- seq(from = -97.4, to = -81.3, by = 0.1)
num_y <- seq(from = 24.4, to = 30.4, by = 0.1)

####  Aggregation  ####
#Aggregating the seamap model into bins
tagging_binned <- tagging %>% 
  mutate(lon_bin = round(long_decdeg, 1),
         lat_bin = round(lat_decdeg, 1),
         tag_year = lubridate::year(date),
         tag_month = lubridate::month(date),
         tag_month = month.abb[as.numeric(tag_month)]) %>% 
  group_by(tag_month, lon_bin, lat_bin) %>% 
  summarise(n_tagged = n()) %>% 
  split(.$tag_month) 
  
# Make them sf objects
tagging_binned <- map(tagging_binned, st_as_sf, coords = c("lon_bin", "lat_bin"), crs = 4326, remove = FALSE)


#Convert to Raster and plot 
tagging_ras_function <- function(agg_data){
  raster_out <- st_rasterize(
    agg_data["n_tagged"],
    template = st_as_stars(
      test_bbox,
      values = NA_real_,
      nx = length(num_x),
      ny = length(num_y)))
}

# Now do all of them
tagging_rasters <- map(tagging_binned, tagging_ras_function)
tag_stack <- stack(tagging_rasters)

# Test Plot
plot(tagging_rasters$Feb, axes = T, main = "Total Tagging in February")
