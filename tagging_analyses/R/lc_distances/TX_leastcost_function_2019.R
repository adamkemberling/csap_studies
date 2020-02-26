##### texas leastcost distances automatic calculation function
#created 11/28/2017, Adam Kemberling
#calls on a dataframe with merged tagging locations and recapture locations
#saves as an updated dataframe with leastcost distances for texas
texas_lc <- function(merge_dat) {

  
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
  points(merge_start, col = 'blue')
  plot(gulf_ras, maxpixels = 10000)
  points(merge_end, col = 'orange')
  
  
  ########################################## subset to working area ####################
  
  #TX
  tx_starts <- as.data.frame(merge_start)
  #tx_starts <- tx_starts[which(tx_starts$Tag_state == 'Texas'),]
  #tx_starts <- tx_starts[which(tx_starts$long_decdeg < -10400895),]
  
  tx_stops <- as.data.frame(merge_end)
  #tx_stops <- tx_stops[which(tx_stops$Tag_state == 'Texas'),]
  #tx_stops <- tx_stops[which(tx_stops$Rlong < -10400895),]
  
  #Check they fit in extent
  range(tx_starts$long_decdeg);range(tx_starts$lat_decdeg)
  range(tx_stops$Rlong);range(tx_stops$Rlat)
  
  coordinates(tx_starts) <- ~long_decdeg + lat_decdeg
  coordinates(tx_stops) <- ~Rlong + Rlat
  crs(tx_starts) <- crs(meters.crs)
  crs(tx_stops) <- crs(meters.crs)
  plot(gulf_ras, maxpixels = 100000);points(tx_starts); points(tx_stops)
  
  #crop it more, too big
  #locator()
  e1 <- extent(-10833963, -10400947, 3042685, 3337638) #extent for crop
  plot(as(e1, 'SpatialPolygons'), border = 'black', add = TRUE) #check that things fit
  area_crop <- crop(gulf_ras, e1)
  
  ############################# subest by cropped area, and make sure they fit #########################
  #start locations within extent
  test.pol <- as(extent(area_crop), 'SpatialPolygons')
  plot(gulf_ras, maxpixels = 100000)
  plot(test.pol, border = 'black', add = TRUE)
  crs(test.pol) <- meters.crs
  inside.poly <- !is.na(over(tx_starts, test.pol)) #for project shapefile
  
  #Make sure all starting locations fit
  tx_starts <-  merge_dat[which(inside.poly == TRUE),]
  #tx_starts <- tx_starts[which(tx_starts$Tag_state == 'Alabama'),]
  tx_stops <- tx_starts #this will stay a dataframe to subset in next step
  
  #now check the recaptures from that subset
  coordinates(tx_starts)<- ~ Rlong + Rlat # make spatial object ***using recapture coordinates ***
  crs(tx_starts) <-crs(degree.crs) # sets the crs to degrees
  tx_starts <-spTransform(tx_starts, crs(meters.crs)) # transforms to meters
  
  inside.poly <- !is.na(over(tx_starts, test.pol)) #for project shapefile
  
  tx_stops <- tx_stops[which(inside.poly == TRUE),]
  tx_starts <- tx_stops
  
  coordinates(tx_starts)<- ~ long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(tx_starts) <-crs(degree.crs) # sets the crs to degrees
  tx_starts <-spTransform(tx_starts, crs(meters.crs)) # transforms to meters
  
  coordinates(tx_stops)<- ~ Rlong + Rlat # converts the file to a spatialPoints object
  crs(tx_stops) <-crs(degree.crs) # sets the crs to degrees
  tx_stops <-spTransform(tx_stops, crs(meters.crs)) # transforms to meters
  
  #check to see if they are contained
  plot(gulf_ras, maxpixels = 100000);points(tx_starts, col = 'blue'); points(tx_stops, col = 'orange')
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
  tx_starts <- as.data.frame(tx_starts)
  tx_starts <- tx_starts[which(tx_starts$Displacement_meters > res(area_crop)[1]),] #res(area_crop) raster resolution
  tx_starts <- tx_starts[which(tx_starts$Displacement_meters > res(area_crop)[2]),] #res(area_crop) raster resolution
  coordinates(tx_starts) <- ~long_decdeg +lat_decdeg
  
  tx_stops <- as.data.frame(tx_stops)
  tx_stops <- tx_stops[which(tx_stops$Displacement_meters > res(area_crop)[1]),]
  tx_stops <- tx_stops[which(tx_stops$Displacement_meters > res(area_crop)[2]),]
  coordinates(tx_stops) <- ~Rlong+Rlat
  
  #----------------------------------------- Test Line
  plot(area_crop)
  points(tx_starts[2,],pch = 21, col = 'black', bg = 'blue')
  points(tx_stops[2,], pch = 21, col = 'black', bg = 'orange')
  lines(shortestPath(transition.layer,tx_starts[2,],tx_stops[2,], output = "SpatialLines"))
  #scalebar(20000, xy = click(), type = 'bar', divs = 2, below = "Kilometers", lonlat = NULL,  label = c(20,10,0), adj=c(0.5, -1.1), lwd = 2)
  
  
  ######################################### Distance Traveled ############################
  lc_dist <- rep(NA, nrow(tx_starts))
  
  #################################### Least Cost Path Plot for all #################################
  
  #fails if any points are off the transition layer
  plot(area_crop)
  for ( i in 1: nrow(tx_starts)){
    lines(shortestPath(transition.layer,tx_starts[i,],tx_stops[i,], output = "SpatialLines"))
    points(tx_starts[i],pch = 21, col = 'black', bg = 'blue')
    points(tx_stops[i], pch = 21, col = 'black', bg = 'orange')
  }
  
  #scalebar(20000, xy = click(), type = 'bar', divs = 2, below = "Kilometers", lonlat = NULL,  label = c(0,10,20), adj=c(0.5, -1.1), lwd = 2)
  
  
  #Distance of line of Interest, Same as costDistance function, verified 5/19/2017
  #SpatialLinesLengths(shortestPath(transition.layer,tx_starts[2,], tx_stops[2,], output = "SpatialLines"))
  
  
  ################################################## Spatial points method ################################
  
  Sys.time()
  for(i in 1:nrow(tx_starts)){
    lc_dist[i] <- SpatialLinesLengths(shortestPath(transition.layer, tx_starts[i,], tx_stops[i,], output = "SpatialLines"))
  }
  Sys.time()
  plot(lc_dist)
  
  crs(tx_starts) <- crs(meters.crs)
  tx_starts <-spTransform(tx_starts, crs(degree.crs))
  tx_starts <- as.data.frame(tx_starts)
  
  tx_starts$lc_distance <- lc_dist
  
  #should line up
  plot(tx_starts$Displacement_meters,tx_starts$lc_distance)
  plot(tx_starts$lc_distance - tx_starts$Displacement_meters) #should have no negatives
  
  ####################################### add to main dataframe #####################################
  #only add texas
  #merge_dat$lc_distance <- tx_starts[match(merge_dat$tag_number, tx_starts$tag_number, nomatch = 0),'lc_distance']
  
  #same way as others
  merge_dat$lc_distance <- tx_starts[match(merge_dat$tag_number, tx_starts$tag_number),'lc_distance']
  
  plot(lc_distance ~ Displacement_meters, data = merge_dat)
  
  hist(merge_dat$lc_distance)
  
  ####################################### save csv and put together #####################################
  write.csv(merge_dat, file = here::here("Data", "lc_distance_processing", "tx_update.csv" ))
  
}
  
  