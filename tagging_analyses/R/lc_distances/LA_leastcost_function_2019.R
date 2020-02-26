##### Louisiana leastcost distances automatic calculation function
#created 11/28/2017, Adam Kemberling
#calls on a dataframe with merged tagging locations and recapture locations
#saves as an updated dataframe with leastcost distances for Louisiana
louisiana_lc <- function(merge_dat) {

  
  ################################################### CRS ##################################################
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
  
  ######################################## Raster File ########################################
  gulf_ras  <- raster(here::here("Data", "spatial_files", "gulf_rasters", "fullgulf_m_cropped.tif"))
  
  plot(gulf_ras, maxpixels = 10000)
  points(merge_start, col = 'blue')
  plot(gulf_ras, maxpixels = 10000)
  points(merge_end, col = 'orange')
  
  
  #check coord
  range(merge_dat$lat_decdeg) ; range(merge_dat$long_decdeg)
  range(merge_dat$Rlong) ; range(merge_dat$Rlat)
  
  
  ########################################## subset to working area ####################
  
  #Louisiana
  la_starts <- as.data.frame(merge_start)
  la_starts <- la_starts[which(la_starts$Tag_state == 'Louisiana'),]
  
  la_stops <- as.data.frame(merge_end)
  la_stops <- la_stops[which(la_stops$Tag_state == 'Louisiana'),]
  
  
  coordinates(la_starts) <- ~long_decdeg + lat_decdeg
  coordinates(la_stops) <- ~Rlong + Rlat
  plot(gulf_ras, maxpixels = 100000);points(la_starts); points(la_stops)
  
  #crop it more, too big
  #locator()
  e1 <- extent( -10457428,  -9679255, 3218402, 3400394) #extent for crop
  plot(as(e1, 'SpatialPolygons'), border = 'black', add = TRUE) #check that things fit
  area_crop <- crop(gulf_ras, e1)
  
  
  ############################### make sure they all fit ##########################
  #start locations within extent
  test.pol <- as(extent(area_crop), 'SpatialPolygons')
  plot(gulf_ras, maxpixels = 100000)
  plot(test.pol, border = 'black', add = TRUE)
  crs(test.pol) <- meters.crs
  inside.poly <- !is.na(over(merge_start, test.pol)) #for project shapefile
  
  #Make sure all starting locations fit
  la_starts <-  merge_dat[which(inside.poly == TRUE),]
  la_stops <- la_starts #this will stay a dataframe to subset in next step
  
  #now check the recaptures from that subset
  coordinates(la_starts)<- ~ Rlong + Rlat # make spatial object ***using recapture coordinates ***
  crs(la_starts) <-crs(degree.crs) # sets the crs to degrees
  la_starts <-spTransform(la_starts, crs(meters.crs)) # transforms to meters
  
  inside.poly <- !is.na(over(la_starts, test.pol)) #for project shapefile
  
  la_stops <- la_stops[which(inside.poly == TRUE),]
  la_starts <- la_stops
  
  coordinates(la_starts)<- ~ long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(la_starts) <-crs(degree.crs) # sets the crs to degrees
  la_starts <-spTransform(la_starts, crs(meters.crs)) # transforms to meters
  
  coordinates(la_stops)<- ~ Rlong + Rlat # converts the file to a spatialPoints object
  crs(la_stops) <-crs(degree.crs) # sets the crs to degrees
  la_stops <-spTransform(la_stops, crs(meters.crs)) # transforms to meters
  
  #check to see if they are contained
  plot(gulf_ras, maxpixels = 100000);points(la_starts, col = 'blue'); points(la_stops, col = 'orange')
  plot(as(e1, 'SpatialPolygons'), border = 'black', add = TRUE) #check that things fit
  
  rm(merge_start); rm(merge_end)
  
  

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
  
  #remove crabs with distances less than raster resolution
  la_starts <- as.data.frame(la_starts)
  la_starts <- la_starts[which(la_starts$Displacement_meters > res(area_crop)[1]),] #70.6 is raster resolution
  la_starts <- la_starts[which(la_starts$Displacement_meters > res(area_crop)[2]),] #70.6 is raster resolution
  coordinates(la_starts) <- ~long_decdeg +lat_decdeg
  
  la_stops <- as.data.frame(la_stops)
  la_stops <- la_stops[which(la_stops$Displacement_meters > res(area_crop)[1]),]
  la_stops <- la_stops[which(la_stops$Displacement_meters > res(area_crop)[2]),]
  coordinates(la_stops) <- ~Rlong+Rlat
  
  
  
  #----------------------------------------- Test Line
  plot(area_crop)
  points(la_starts[10,],pch = 21, col = 'black', bg = 'blue')
  points(la_stops[10,], pch = 21, col = 'black', bg = 'orange')
  lines(shortestPath(transition.layer,la_starts[10,],la_stops[10,], output = "SpatialLines"))
  #scalebar(20000, xy = click(), type = 'bar', divs = 2, below = "Kilometers", lonlat = NULL,  label = c(20,10,0), adj=c(0.5, -1.1), lwd = 2)
  
  
  
  ######################################### Distance Traveled ############################
  lc_dist <- rep(NA, nrow(la_starts))
  
  start.mat <- matrix(nrow = nrow(la_starts), ncol = 2)
  start.mat[,1] <- as.data.frame(la_starts)[,'long_decdeg']
  start.mat[,2] <- as.data.frame(la_starts)[,'lat_decdeg']
  end.mat <- matrix(nrow = nrow(la_starts), ncol = 2)
  end.mat[,1] <- as.data.frame(la_stops)[,'Rlong']
  end.mat[,2] <- as.data.frame(la_stops)[,'Rlat']
  
  #test matrix approach, should equal spatial object distance
  #SpatialLinesLengths(shortestPath(transition.layer, start.mat[1,],end.mat[1,], output = "SpatialLines"))
  #SpatialLinesLengths(shortestPath(transition.layer, la_starts[1,],la_stops[1,], output = "SpatialLines"))
  
  Sys.time()
  for(i in 1:nrow(la_starts)){
    lc_dist[i] <- SpatialLinesLengths(shortestPath(transition.layer, start.mat[i,],end.mat[i,], output = "SpatialLines"))
  }
  Sys.time()
  plot(lc_dist)
  
  
  #change la starts coordinates back and add lc_distances
  crs(la_starts) <- crs(meters.crs)
  la_starts <-spTransform(la_starts, crs(degree.crs))
  la_starts <- as.data.frame(la_starts)
  
  la_starts$lc_distance <- lc_dist
  
  #should line up
  plot(la_starts$Displacement_meters,la_starts$lc_distance)
  
  
  ####################################### add to main dataframe #####################################
  merge_dat$lc_distance <- la_starts[match(merge_dat$tag_number, la_starts$tag_number),'lc_distance']
  
  plot(lc_distance ~ Displacement_meters, data = merge_dat)
  merge_dat[which(merge_dat$lc_distance < merge_dat$Displacement_meters),]
  
  ####################################### save csv and do another area #####################################
  
  update.save.name <- here::here("Data", "lc_distance_processing", "la_update.csv")
  write.csv(merge_dat, file = update.save.name)
}