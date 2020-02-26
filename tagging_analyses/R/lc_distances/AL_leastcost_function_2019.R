##### alabama leastcost distances automatic calculation function
#created 11/28/2017, Adam Kemberling
#calls on a dataframe with merged tagging locations and recapture locations
#saves as an updated dataframe with leastcost distances for alabama
alabama_lc <- function(merge_dat) {
  library(raster)
  library(gdistance)
  library(rgdal)
  library(maptools)
  library(GISTools)
  library(sp)
  library(geosphere)
  
  ################################################## CRS ##################################################
  degree.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84") # 
  meters.crs <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")
  
  
  ######################################## starting and ending coordinates ####################
  merge_start <- merge_dat
  merge_end <- merge_dat
  
  #starting coordinates
  coordinates(merge_start)<- ~ long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(merge_start) <-crs(degree.crs) # sets the crs to degrees
  merge_start <-spTransform(merge_start, crs(meters.crs)) # transforms to meters
  
  #ending coordinates
  coordinates(merge_end)<- ~ Rlong + Rlat # converts the file to a spatialPoints object
  crs(merge_end) <-crs(degree.crs) # sets the crs to degrees
  merge_end <-spTransform(merge_end, crs(meters.crs)) # transforms to meters
  
  ######################################### Raster File ########################################
  gulf_ras  <- raster(here::here("Data", "spatial_files", "gulf_rasters", "fullgulf_m_cropped.tif"))
  
  plot(gulf_ras, maxpixels = 10000)
  points(merge_start)
  plot(gulf_ras, maxpixels = 10000)
  points(merge_end)
  
  
  ########################################## subset to working area ####################
  
  #AL
  al_starts <- as.data.frame(merge_start)
  al_stops <- as.data.frame(merge_end)
  
  #match coordinates
  coordinates(al_starts) <- ~long_decdeg + lat_decdeg
  coordinates(al_stops) <- ~Rlong + Rlat
  crs(al_starts) <- crs(meters.crs)
  crs(al_stops) <- crs(meters.crs)
  plot(gulf_ras, maxpixels = 100000);points(al_starts); points(al_stops)
  
  #crop it to working area
  #locator()
  e1 <- extent(-9873798, -9701220, 3343913, 3428634) #extent for crop
  plot(as(e1, 'SpatialPolygons'), border = 'black', add = TRUE) #check that things fit
  area_crop <- crop(gulf_ras, e1)
  plot(area_crop, maxpixels = 100000);points(al_starts, col = 'blue');points(al_stops, col = 'orange')
  
  ######################################### make sure they all fit ####################################
  
  #start locations within extent
  test.pol <- as(extent(area_crop), 'SpatialPolygons')
  plot(gulf_ras, maxpixels = 100000)
  plot(test.pol, border = 'black', add = TRUE)
  crs(test.pol) <- meters.crs
  inside.poly <- !is.na(over(al_starts, test.pol)) #for project shapefile
  
  #Make sure all starting locations fit
  al_starts <-  merge_dat[which(inside.poly == TRUE),]
  #al_starts <- al_starts[which(al_starts$Tag_state == 'Alabama'),]
  al_stops <- al_starts #this will stay a dataframe to subset in next step
  
  #now check the recaptures from that subset
  coordinates(al_starts)<- ~ Rlong + Rlat # make spatial object ***using recapture coordinates ***
  crs(al_starts) <-crs(degree.crs) # sets the crs to degrees
  al_starts <-spTransform(al_starts, crs(meters.crs)) # transforms to meters
  
  inside.poly <- !is.na(over(al_starts, test.pol)) #for project shapefile
  
  al_stops <- al_stops[which(inside.poly == TRUE),]
  al_starts <- al_stops
  
  coordinates(al_starts)<- ~ long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(al_starts) <-crs(degree.crs) # sets the crs to degrees
  al_starts <-spTransform(al_starts, crs(meters.crs)) # transforms to meters
  
  coordinates(al_stops)<- ~ Rlong + Rlat # converts the file to a spatialPoints object
  crs(al_stops) <-crs(degree.crs) # sets the crs to degrees
  al_stops <-spTransform(al_stops, crs(meters.crs)) # transforms to meters
  
  #check to see if they are contained
  plot(gulf_ras, maxpixels = 100000);points(al_starts, col = 'blue'); points(al_stops, col = 'orange')
  plot(as(e1, 'SpatialPolygons'), border = 'black', add = TRUE) #check that things fit
  
  
  
  
  #-------------------------------- create transition layers and distance matrix
  #aggregate and make transition layers, need to remove points that distances are smaller than resolution
  area_crop <- aggregate(area_crop, fact = 4, fun = mean)
  res(area_crop)[1]
  
  
  #create transition layer
  Sys.time()
  transition.layer <- transition(1/area_crop, mean, directions=16)
  Sys.time()
  transition.layer <- geoCorrection(transition.layer, type = "c") # correct for map distortion, as well as for diagonal connections between grid cells
  Sys.time()
  
  #remove small travel
  al_starts <- as.data.frame(al_starts)
  al_starts <- al_starts[which(al_starts$Displacement_meters > res(area_crop)[1]),] #res(area_crop) raster resolution
  al_starts <- al_starts[which(al_starts$Displacement_meters > res(area_crop)[2]),] #res(area_crop) raster resolution
  coordinates(al_starts) <- ~long_decdeg +lat_decdeg
  
  al_stops <- as.data.frame(al_stops)
  al_stops <- al_stops[which(al_stops$Displacement_meters > res(area_crop)[1]),]
  al_stops <- al_stops[which(al_stops$Displacement_meters > res(area_crop)[2]),]
  coordinates(al_stops) <- ~Rlong+Rlat
  
  #----------------------------------------- Test Line
  plot(area_crop)
  points(al_starts[67,],pch = 21, col = 'black', bg = 'blue')
  points(al_stops[67,], pch = 21, col = 'black', bg = 'orange')
  lines(shortestPath(transition.layer,al_starts[67,],al_stops[67,], output = "SpatialLines"))
  #scalebar(20000, xy = click(), type = 'bar', divs = 2, below = "Kilometers", lonlat = NULL,  label = c(20,10,0), adj=c(0.5, -1.1), lwd = 2)
  
  
  ######################################### Distance Traveled ############################
  lc_dist <- rep(NA, nrow(al_starts))
  
  ################################################## Spatial points method ################################
  
  Sys.time()
  for(i in 1:nrow(al_starts)){
    lc_dist[i] <- SpatialLinesLengths(shortestPath(transition.layer, al_starts[i,], al_stops[i,], output = "SpatialLines"))
  }
  Sys.time()
  plot(lc_dist)
  
  crs(al_starts) <- crs(meters.crs)
  al_starts <-spTransform(al_starts, crs(degree.crs))
  al_starts <- as.data.frame(al_starts)
  
  al_starts$lc_distance <- lc_dist
  
  #should line up
  plot(al_starts$Displacement_meters,al_starts$lc_distance)
  
  ####################################### add to main dataframe #####################################
  merge_dat$lc_distance <- al_starts[match(merge_dat$tag_number, al_starts$tag_number),'lc_distance']
  
  plot(lc_distance ~ Displacement_meters, data = merge_dat)
  hist(merge_dat$lc_distance)
  
  ####################################### save csv and do another area #####################################
  write.csv(merge_dat, file = here::here("Data", "lc_distance_processing", "al_update.csv" ))
}