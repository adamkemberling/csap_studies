##### florida leastcost distances automatic calculation function
#created 11/28/2017, Adam Kemberling
#calls on a dataframe named "both"
#saves as an updated dataframe with leastcost distances for Florida, straight line distances are used for other states
florida_lc <- function(merge_dat) {

  
  
  ################################################## CRS ##################################################
  degree.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84") # 
  meters.crs <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")
  
  
  ######################################## Create starting and ending coordinates ####################
  merge_start <- merge_dat
  merge_end <- merge_dat
  
  #starting coordinates
  coordinates(merge_start)<- ~ long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(merge_start) <-crs(degree.crs) # sets the crs to degrees
  
  
  #ending coordinates
  coordinates(merge_end)<- ~ Rlong + Rlat # converts the file to a spatialPoints object
  crs(merge_end) <-crs(degree.crs) # sets the crs to degrees
  
  
  #double check all straight line travel distances
  sl_dist <- c()
  for (i in 1:nrow(merge_dat)) {
    sl_dist[i] <- distCosine(merge_start[i,], merge_end[i,], r=6378137)
  }
  
  plot(merge_dat$Displacement_meters)
  plot(sl_dist)
  merge_dat$Displacement_meters <- sl_dist
  merge_dat$Displacement_km <- sl_dist/1000
  
  merge_start <- merge_dat
  merge_end   <- merge_dat
  
  #starting coordinates
  coordinates(merge_start) <- ~ long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(merge_start)         <- crs(degree.crs) # sets the crs to degrees
  merge_start              <-spTransform(merge_start, crs(meters.crs)) # transforms to meters
  
  #ending coordinates
  coordinates(merge_end)   <- ~ Rlong + Rlat # converts the file to a spatialPoints object
  crs(merge_end)           <- crs(degree.crs) # sets the crs to degrees
  merge_end                <- spTransform(merge_end, crs(meters.crs)) # transforms to meters
  
  ######################## add raster file ################
  gulf_ras  <- raster(here::here("Data", "spatial_files", "gulf_rasters", "fullgulf_m_cropped.tif"))
  gulf      <- readOGR(here::here("Data", "spatial_files", "gulf_shapefiles", "Nos80_gulfcrop.shp")) #study area shapefile
  
  
  # crs(gulf) <- crs(degree.crs)
  # gulf <- spTransform(gulf, meters.crs)
  # plot(gulf, col = 'gray 80', main = 'Tagging Locations');  points(merge_start, col = 'blue')
  # plot(gulf, col = 'gray 80', main = 'Recapture Locations');points(merge_start, col = 'orange')
  
  plot(gulf_ras, maxpixels = 10000, main = 'Tag Release')
  points(merge_start,pch = 21, col = 'black', bg = 'blue')
  plot(gulf_ras, maxpixels = 10000, main = 'Recapture Location')
  points(merge_end, pch = 21, col = 'black', bg = 'orange')

  ################## check coordinates, and crop ##############
  range(merge_dat$lat_decdeg);range(merge_dat$long_decdeg)
  range(merge_dat$Rlong);range(merge_dat$Rlat)
  
  #crop it more, too big
  e1 <- extent( -9569433, -9250000,3270000, 3365000)
  area_crop <- crop(gulf_ras, e1)
  plot(area_crop, maxpixels = 10000)
  points(merge_start,pch = 21, col = 'black', bg = 'blue')
  points(merge_end, pch = 21, col = 'black', bg = 'orange')
  ############################## Subsetting by raster Area ############################
  
  #start locations within extent
  test.pol <- as(extent(area_crop), 'SpatialPolygons')
  plot(gulf_ras, maxpixels = 100000)
  plot(test.pol, border = 'black', add = TRUE)
  crs(test.pol) <- meters.crs
  inside.poly <- !is.na(over(merge_start, test.pol)) #for project shapefile
  
  
  #Make sure all starting locations fit
  stein_starts <-  merge_dat[which(inside.poly == TRUE),]
  stein_stops <- stein_starts #this will stay a dataframe to subset in next step
  
  #now check the recaptures from that subset
  coordinates(stein_starts)<- ~ Rlong + Rlat # make spatial object ***using recapture coordinates ***
  crs(stein_starts) <-crs(degree.crs) # sets the crs to degrees
  stein_starts <-spTransform(stein_starts, crs(meters.crs)) # transforms to meters
  
  inside.poly <- !is.na(over(stein_starts, test.pol)) #for project shapefile
  
  stein_stops <- stein_stops[which(inside.poly == TRUE),]
  stein_starts <- stein_stops
  
  coordinates(stein_starts)<- ~ long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(stein_starts) <-crs(degree.crs) # sets the crs to degrees
  stein_starts <-spTransform(stein_starts, crs(meters.crs)) # transforms to meters
  
  coordinates(stein_stops)<- ~ Rlong + Rlat # converts the file to a spatialPoints object
  crs(stein_stops) <-crs(degree.crs) # sets the crs to degrees
  stein_stops <-spTransform(stein_stops, crs(meters.crs)) # transforms to meters
  
  #check to see if they are contained
  plot(gulf_ras, maxpixels = 100000)
  points(stein_starts,pch = 21, col = 'black', bg = 'blue')
  points(stein_stops,pch = 21, col = 'black', bg = 'orange')
  plot(area_crop, maxpixels = 100000)
  points(stein_starts,pch = 21, col = 'black', bg = 'blue')
  points(stein_stops,pch = 21, col = 'black', bg = 'orange')
  
  ############## Aggregate, remove crabs whose travel < raster resolution, and create transition layers and distance matrix ###############
  
  res(area_crop) #check resolution (m)
  area_crop <- aggregate(area_crop, fact = 4, fun = mean) #function mean will make aggregated edges permeable but less so than h2o
  
  transition.layer <- transition(1/area_crop, mean, directions=16)
  transition.layer <- geoCorrection(transition.layer, type = "c") # correct for map distortion, as well as for diagonal connections between grid cells
  
  res(area_crop)
  stein_starts <-as.data.frame(stein_starts)
  stein_starts <- stein_starts[which(stein_starts$Displacement_meters >= res(area_crop)[1]),] #remove crabs that are too close
  stein_starts <- stein_starts[which(stein_starts$Displacement_meters >= res(area_crop)[2]),] #remove crabs that are too close
  coordinates(stein_starts) <- ~long_decdeg + lat_decdeg
  crs(stein_starts) <- crs(meters.crs)
  
  stein_stops <-as.data.frame(stein_stops)
  stein_stops <- stein_stops[which(stein_stops$Displacement_meters >= res(area_crop)[1]),] #remove crabs that are too close 
  coordinates(stein_stops) <- ~Rlong + Rlat
  crs(stein_stops) <- crs(meters.crs)
  
  
  
  ########################################### Test Line#########################
  plot(area_crop)
  points(stein_starts[10,],pch = 21, col = 'black', bg = 'blue')
  points(stein_stops[10,], pch = 21, col = 'black', bg = 'orange')
  lines(shortestPath(transition.layer,stein_starts[10,],stein_stops[10,], output = "SpatialLines"))
  #scalebar(20000, xy = click(), type = 'bar', divs = 2, below = "Kilometers", lonlat = NULL,  label = c(20,10,0), adj=c(0.5, -1.1), lwd = 2)
  
  
  ########################################### plot all of their locations/numbers ######
  #plot(area_crop, maxpixels = 10000)
  #points(stein_starts,pch = 21, col = 'black', bg = 'blue')
  #points(stein_stops, pch = 21, col = 'black', bg = 'orange')
  #text(stein_stops, stein_stops$tag_number)
  
  
  
  
  ######################################## Least Cost Path Plot #################################
  #fails if any points are off the transition layer
  # plot(area_crop)
  # for ( i in 1: nrow(stein_starts)){
  #   lines(shortestPath(transition.layer,stein_starts[i,],stein_stops[i,], output = "SpatialLines"))
  #   points(stein_starts[i],pch = 21, col = 'black', bg = 'blue')
  #   points(stein_stops[i], pch = 21, col = 'black', bg = 'orange')
  # }
  # scalebar(20000, xy = click(), type = 'bar', divs = 2, below = "Kilometers", lonlat = NULL,  label = c(0,10,20), adj=c(0.5, -1.1), lwd = 2)
  
  
  #Distance of line of Interest, Same as costDistance function, verified 5/19/2017
  #SpatialLinesLengths(shortestPath(transition.layer,stein_starts[2,], stein_stops[2,], output = "SpatialLines"))
  
  
  
  
  ######################################### Distance Traveled ############################
  
  #make a place to store the distances with the same rows and tag numbers
  stein_crabs1 <- as.data.frame(stein_starts)
  
  nrow(stein_crabs1) == nrow(stein_starts) #needs to be true
  
  
  #calculate and store lc distances
  lc_dist <- rep(NA, nrow(stein_crabs1))
  Sys.time()
  for(i in 1: nrow(stein_crabs1)){
    lc_dist[i] <- SpatialLinesLengths(shortestPath(transition.layer,stein_starts[i,],stein_stops[i,], output = "SpatialLines"))
  }
  Sys.time()
  stein_crabs1$lc_distance <- lc_dist
  #should line up
  plot(lc_distance ~ Displacement_meters, data = stein_crabs1)
  
  #crabs that have shorter lc distances
  stein_crabs1[which(stein_crabs1$Displacement_meters > stein_crabs1$lc_distance),c('Displacement_meters','lc_distance')]
  
  
  plot(stein_crabs1$Displacement_meters,stein_crabs1$distancetraveled)
  plot(stein_crabs1$lc_distance/1000 - stein_crabs1$Displacement_meters/1000) #should have no negatives > the resolution
  
  ####################################### add to main dataframe #####################################
  #attempt 3, success, but will overwrite any old lc distances so be careful
  merge_dat$lc_distance <- merge_dat$Displacement_meters
  
  #this finally works
  merge_dat$lc_distance <- stein_crabs1[match(merge_dat$tag_number, stein_crabs1$tag_number),'lc_distance']
  plot(merge_dat$lc_distance)
  plot(lc_distance ~ Displacement_meters, data = merge_dat)
  plot(merge_dat$lc_distance - merge_dat$Displacement_meters)
  
  hist(merge_dat$lc_distance)
  
  ####################################### save csv and do another area #####################################
  update.save.name <- here::here("Data", "lc_distance_processing", "fl_update.csv")
  write.csv(merge_dat, file = update.save.name)
}