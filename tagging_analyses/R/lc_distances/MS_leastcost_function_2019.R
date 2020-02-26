##### Mississippi leastcost distances automatic calculation function
#created 11/28/2017, Adam Kemberling
#calls on a dataframe with merged tagging locations and recapture locations
#saves as an updated dataframe with leastcost distances for Mississippi
mississippi_lc <- function(merge_dat) {
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
  coordinates(merge_start) <- ~long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(merge_start)         <- crs(degree.crs) # sets the crs to degrees
  merge_start              <- spTransform(merge_start, crs(meters.crs)) # transforms to meters
  
  #ending coordinates
  coordinates(merge_end)  <- ~Rlong + Rlat # converts the file to a spatialPoints object
  crs(merge_end)          <- crs(degree.crs) # sets the crs to degrees
  merge_end               <- spTransform(merge_end, crs(meters.crs)) # transforms to meters
  
  ######################################### Raster File ########################################
  gulf_ras  <- raster(here::here("Data", "spatial_files", "gulf_rasters", "fullgulf_m_cropped.tif"))
  
  plot(gulf_ras, maxpixels = 10000)
  points(merge_start, col = 'blue')
  plot(gulf_ras, maxpixels = 10000)
  points(merge_end, col = 'orange')
  
  
  ########################################## subset to working area ####################
  
  #MS
  ms_starts <- as.data.frame(merge_start)
  ms_starts <- ms_starts[which(ms_starts$Tag_state == 'Mississippi'),]
  
  ms_stops <- as.data.frame(merge_end)
  ms_stops <- ms_stops[which(ms_stops$Tag_state == 'Mississippi'),]
  
  coordinates(ms_starts) <- ~long_decdeg + lat_decdeg
  coordinates(ms_stops)  <- ~Rlong + Rlat
  plot(gulf_ras, maxpixels = 100000);points(ms_starts); points(ms_stops)
  
  #crop it more, too big
  #locator()
  e1 <- extent(-10058928,  -9767113, 3215264, 3397256) #extent for crop
  plot(as(e1, 'SpatialPolygons'), border = 'black', add = TRUE) #check that things fit
  area_crop <- crop(gulf_ras, e1)
  plot(area_crop, maxpixels = 100000);points(ms_starts, col = 'blue');points(ms_stops, col = 'orange')
  
  
  ############################## make sure they all fit ################################
  #start locations within extent
  test.pol <- as(extent(area_crop), 'SpatialPolygons')
  plot(gulf_ras, maxpixels = 100000)
  plot(test.pol, border = 'black', add = TRUE)
  crs(test.pol) <- meters.crs
  crs(ms_starts) <- meters.crs
  inside.poly <- !is.na(over(ms_starts, test.pol)) #for project shapefile
  
  #Make sure all starting locations fit
  ms_starts <-  merge_dat[which(inside.poly == TRUE),]
  ms_starts <- ms_starts[which(ms_starts$Tag_state == 'Mississippi'),]
  ms_stops  <- ms_starts #this will stay a dataframe to subset in next step
  
  #now check the recaptures from that subset
  coordinates(ms_starts)<- ~ Rlong + Rlat # make spatial object ***using recapture coordinates ***
  crs(ms_starts) <-crs(degree.crs) # sets the crs to degrees
  ms_starts <-spTransform(ms_starts, crs(meters.crs)) # transforms to meters
  
  inside.poly <- !is.na(over(ms_starts, test.pol)) #for project shapefile
  
  ms_stops <- ms_stops[which(inside.poly == TRUE),]
  ms_starts <- ms_stops
  
  coordinates(ms_starts)<- ~ long_decdeg + lat_decdeg # converts the file to a spatialPoints object
  crs(ms_starts) <-crs(degree.crs) # sets the crs to degrees
  ms_starts <-spTransform(ms_starts, crs(meters.crs)) # transforms to meters
  
  coordinates(ms_stops)<- ~ Rlong + Rlat # converts the file to a spatialPoints object
  crs(ms_stops) <-crs(degree.crs) # sets the crs to degrees
  ms_stops <-spTransform(ms_stops, crs(meters.crs)) # transforms to meters
  
  #check to see if they are contained
  plot(gulf_ras, maxpixels = 100000);points(ms_starts, col = 'blue'); points(ms_stops, col = 'orange')
  plot(as(e1, 'SpatialPolygons'), border = 'black', add = TRUE) #check that things fit
  
  
  
  
  
  
  #-------------------------------- create transition layers and distance matrix ----------
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
  ms_starts <- as.data.frame(ms_starts)
  ms_starts <- ms_starts[which(ms_starts$Displacement_meters > res(area_crop)[1]),] #70.6 is raster resolution
  ms_starts <- ms_starts[which(ms_starts$Displacement_meters > res(area_crop)[2]),] #70.6 is raster resolution
  coordinates(ms_starts) <- ~long_decdeg +lat_decdeg
  
  ms_stops <- as.data.frame(ms_stops)
  ms_stops <- ms_stops[which(ms_stops$Displacement_meters > res(area_crop)[1]),]
  ms_stops <- ms_stops[which(ms_stops$Displacement_meters > res(area_crop)[2]),]
  coordinates(ms_stops) <- ~Rlong+Rlat
  
  #----------------------------------------- Test Line
  plot(area_crop)
  points(ms_starts[10,],pch = 21, col = 'black', bg = 'blue')
  points(ms_stops[10,], pch = 21, col = 'black', bg = 'orange')
  lines(shortestPath(transition.layer,ms_starts[10,],ms_stops[10,], output = "SpatialLines"))
  #scalebar(20000, xy = click(), type = 'bar', divs = 2, below = "Kilometers", lonlat = NULL,  label = c(20,10,0), adj=c(0.5, -1.1), lwd = 2)
  
  
  ######################################### Distance Traveled ############################
  lc_dist <- rep(NA, nrow(ms_starts))
  
  start.mat <- matrix(nrow = nrow(ms_starts), ncol = 2)
  start.mat[,1] <- as.data.frame(ms_starts)[,'long_decdeg']
  start.mat[,2] <- as.data.frame(ms_starts)[,'lat_decdeg']
  end.mat <- matrix(nrow = nrow(ms_starts), ncol = 2)
  end.mat[,1] <- as.data.frame(ms_stops)[,'Rlong']
  end.mat[,2] <- as.data.frame(ms_stops)[,'Rlat']
  
  #test matrix approach, should equal spatial object distance
  SpatialLinesLengths(shortestPath(transition.layer, start.mat[1,],end.mat[1,], output = "SpatialLines"))
  SpatialLinesLengths(shortestPath(transition.layer, ms_starts[1,],ms_stops[1,], output = "SpatialLines"))
  

  
  Sys.time()
  for(i in 1:nrow(ms_starts)){
    lc_dist[i] <- SpatialLinesLengths(shortestPath(transition.layer, start.mat[i,],end.mat[i,], output = "SpatialLines"))
  }
  Sys.time()
  
  #Plot lones
  #plot(lc_dist)
  
  #change la starts coordinates back and add lc_distances
  crs(ms_starts) <- crs(meters.crs)
  ms_starts <-spTransform(ms_starts, crs(degree.crs))
  ms_starts <- as.data.frame(ms_starts)
  
  ms_starts$lc_distance <- lc_dist
  
  #should line up
  plot(ms_starts$Displacement_meters,ms_starts$lc_distance)
  
  ####################################### add to main dataframe #####################################
  merge_dat$lc_distance <- ms_starts[match(merge_dat$tag_number, ms_starts$tag_number),'lc_distance']
  
  plot(lc_distance ~ Displacement_meters, data = merge_dat)
  hist(merge_dat$lc_distance)
  
  ####################################### save csv and do another area #####################################
  write.csv(merge_dat, file = here::here("Data", "lc_distance_processing", "ms_update.csv" ))
  
  
}