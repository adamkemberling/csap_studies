






#' Tag Cleanup Function
#'
#' @param tag Spreadsheet from shared drive that contains all tagging location data
#' @param recaps Spreadsheet from shared drive that contains all recapture information
#'
#' @return Function does not return anything to working environment. This function exports
#' clean datasets as well as any mismatches or errors as separate .csv files. The pathing for 
#' the 2019 update will export them all within the project directory folder.
#' @export
#'
#' @examples
tag_cleanup <- function(tag,recaps){
  
  # This is a cleaner re-work of the "tagdata_cleanup.R" file for cleaning the raw tagging data
  
  #### load packages ####
  library(raster)
  library(readxl)
  library(lubridate)
  library(tidyverse)
  
  
  #  Remove secretarial columns and coordinates not used for plotting---------------------------------------------------
  tag <- tag %>% 
    dplyr::select(
      tag_number, 
      date, season, 
      basin_helen = basin, 
      area_helen  = area, 
      lat_decdeg, 
      long_decdeg, 
      cw, 
      molt_stage, 
      repro_stage,
      salinity_sur, 
      temp_sur, 
      DO_sur, 
      sal_bot, 
      temp_bot, 
      DO_bot, 
      depth, 
      notes, 
      tagged_by, 
      entered_by)
  
  
  recaps <- recaps %>% 
    dplyr::select(
      tag_number, 
      date_recaptured, 
      location_type, 
      location_descriptive, 
      Rlat  = lat_decimaldeg,  #Rename recapture coordinates
      Rlong = long_decimaldeg, #Rename recapture coordinates
      eggs, 
      Name, 
      sector)
  
  
  #Capitalize any tag number documentation errors
  tag$tag_number    <- stringr::str_to_title(tag$tag_number)
  recaps$tag_number <- stringr::str_to_title(recaps$tag_number)
  
  
  
  #  Handling Duplicate tag numbers -------------------------------------------------------------------------
  #length(unique(tag$tag_number)) #number of unique tag numbers
  n_occur_tag <- data.frame(table(tag$tag_number))
  
  duplicates <- n_occur_tag %>% filter(Freq > 1)
  duplicates <- tag %>% filter(tag_number %in% duplicates$Var1)
  
  
  # #Need to find a good way to identify whether or not entries are duplicates, by tag area or date, or if they are entered incorrectly at two separate times
  # Found this technique, will extract just the rows where they first appear
  tag <- tag[match(unique(tag$tag_number), tag$tag_number),]
  
  # #this will do the same thing
  # tag %>% distinct(tag_number, .keep_all = T) %>% nrow()
  
  # #Here is the old way, which was to just remove them
  # #subset to single tagging entries
  # singles <- n_occur_tag %>% filter(Freq == 1)
  # tag <- tag %>% filter(tag_number %in% singles$Var1)
  
  rm(n_occur_tag)
  
  
  #  Crabs with multiple recapture entries  --------------------------------------------------------------------------
  n_occur_recaps <- data.frame(table(recaps$tag_number))
  
  multi_recaps <-  n_occur_recaps %>% filter(Freq > 1)
  multi_recaps <-  recaps %>% filter(tag_number %in% multi_recaps$Var1) #the rows from recaps that share the tag numbers with frequencies >2
  
  #remove multiple recaps for now, they won't merge correctly
  single_recaps <- n_occur_recaps %>% filter(Freq == 1)
  recaps        <- recaps %>% filter(tag_number %in% single_recaps$Var1)
  
  rm(single_recaps); rm(n_occur_recaps)
  
  
  #  Crabs with recaptures but no tagging information  ------------------------------------------------------------
  
  no_tag <- anti_join(recaps, tag, by = "tag_number")
  
  # #test crab
  # tag[tag$tag_number == "Z00961",]             #No entry
  # recaps[recaps$tag_number == "Z00961",]       #But a recapture entry
  
  
  
  #  Make sure cooordinates are numeric  -------------------------------------------------------
  
  # Tagging Coordinates
  if (class(tag$lat_decdeg) != "numeric")  {tag$lat_decdeg <- as.numeric(as.character(tag$lat_decdeg))}
  if (class(tag$long_decdeg) != "numeric") {tag$long_decdeg <- as.numeric(as.character(tag$long_decdeg))}
  
  # Recapture Coordinates
  if (class(recaps$Rlong) != "numeric")    {recaps$Rlong <- as.numeric(as.character(recaps$Rlong))}
  if (class(recaps$Rlat) != "numeric")     {recaps$Rlat <- as.numeric(as.character(recaps$Rlat))}
  
  class(tag$long_decdeg); class(tag$lat_decdeg)
  class(recaps$Rlong);  class(recaps$Rlat)
  
  
  
  #  Changing date formats ------------------------------
  tag$date <- as.Date(tag$date, format = "%m/%d/%Y")
  recaps$date_recaptured <- as.Date(recaps$date_recaptured, format = '%m/%d/%Y')
  table(year(tag$date))
  table(year(recaps$date_recaptured))
  
  #Double check there aren't any with years < 2000
  filter(tag, year(date) < 2000)                    
  filter(recaps, year(date_recaptured) < 2000)
  
  
  #add correct decades and millenia to dates that are missing it
  tag$date[which(year(tag$date) == 2007)] <- tag$date[year(tag$date) == 2007] + years(10)
  tag$date[which(tag$date < 2000-0-0)] <- tag$date[tag$date < 2000-0-0] + years(2000)
  
  #Check changes
  table(year(tag$date))
  table(year(recaps$date_recaptured))
  
  
  
  # Point in Polygon State Assignments  -------------------------------------
  
  #Remove any rows with unusable coordinates
  tag <- tag[complete.cases(tag$long_decdeg),]
  tag <- tag[complete.cases(tag$lat_decdeg),]
  tag <- tag[!near(tag$long_decdeg, 0),]
  tag <- tag[!near(tag$lat_decdeg, 0),]
  
  
  recaps <- recaps[complete.cases(recaps$Rlong),]
  recaps <- recaps[complete.cases(recaps$Rlat),]
  recaps <- recaps[!near(recaps$Rlong,0),]
  recaps <- recaps[!near(recaps$Rlat,0),]
  
  #Ensure longitude column is negative and latitude is positive
  tag$long_decdeg <- ifelse(tag$long_decdeg > 0, tag$long_decdeg * -1, tag$long_decdeg)
  tag$lat_decdeg  <- ifelse(tag$lat_decdeg < 0, tag$lat_decdeg * -1, tag$lat_decdeg)
  range(tag$long_decdeg); range(tag$lat_decdeg)
  
  #Check range of coordinates for tagging data
  range(tag$long_decdeg); range(tag$lat_decdeg)
  
  recaps$Rlong <- ifelse(recaps$Rlong > 0, recaps$Rlong * -1, recaps$Rlong)
  recaps$Rlat  <- ifelse(recaps$Rlat  < 0, recaps$Rlat *  -1, recaps$Rlat)
  
  #Double check range
  range(recaps$Rlong); range(recaps$Rlat)
  
  
  
  # point in polygon function -----------------------------------------
  library(sp)
  library(rgdal)
  library(raster)
  
  coordinates(tag)<- ~long_decdeg + lat_decdeg 
  coordinates(recaps)<- ~Rlong + Rlat
  
  degree.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84") #Degree coordinate reference system WGS 1984
  meters.crs <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")
  
  #PC State Source File Destinations
  tx <- readOGR(here::here("Data", "spatial_files", "state_extents", "Texas_Statebounds.shp"))
  al <- readOGR(here::here("Data", "spatial_files", "state_extents", "Alabama_Statebounds.shp"))
  ms <- readOGR(here::here("Data", "spatial_files", "state_extents", "Mississippi_Statebounds.shp"))
  la <- readOGR(here::here("Data", "spatial_files", "state_extents", "Louisiana_Statebounds.shp"))
  fl <- readOGR(here::here("Data", "spatial_files", "state_extents", "Florida_Statebounds.shp"))
  
  crs(tag)    <- crs(tx)
  crs(recaps) <- crs(tx)
  
  
  # what state are tags contained within
  inside.la <- !is.na(over(tag, as(la, "SpatialPolygons")))
  inside.ms <- !is.na(over(tag, as(ms, "SpatialPolygons")))
  inside.al <- !is.na(over(tag, as(al, "SpatialPolygons")))
  inside.tx <- !is.na(over(tag, as(tx, "SpatialPolygons")))
  inside.fl <- !is.na(over(tag, as(fl, "SpatialPolygons")))
  
  #--------------------------------Test Plot-----------------------------
  library(maps)
  plot(tx,xlim = c(-97.4, -81.2), ylim = c(25, 30.5), main = "Tag Location Assignment")
  plot(la,add = TRUE)
  plot(ms,add = TRUE)
  plot(al,add = TRUE)
  plot(fl,add = TRUE)
  map("world", region="usa", add=TRUE)
  points(tag[inside.al, ], pch=1, col="purple")
  points(tag[inside.la, ], pch=1, col="red")
  points(tag[inside.fl, ], pch=1, col="green")
  points(tag[inside.ms, ], pch=1, col="orange")
  points(tag[inside.tx, ], pch=1, col="blue")
  
  
  
  #change tag back to dataframe, assign tag state by containment
  tag <- as.data.frame(tag)
  tag$Tag_state <- rep(0,nrow(tag))
  
  #Identify which tags fall into each polygon
  alabama     <- tag[inside.al == TRUE,]                        #AL
  louisiana   <- tag[inside.la == TRUE,]                        #LA
  florida     <- tag[inside.fl == TRUE,]                        #FL
  mississippi <- tag[inside.ms == TRUE,]                        #MS
  texas       <- tag[inside.tx == TRUE,]                        #TX
  
  
  #Label them based on where they were tagged
  tag$Tag_state[tag$tag_number %in% alabama$tag_number]     <- "Alabama"
  tag$Tag_state[tag$tag_number %in% louisiana$tag_number]   <- "Louisiana"
  tag$Tag_state[tag$tag_number %in% florida$tag_number]     <- "Florida"
  tag$Tag_state[tag$tag_number %in% mississippi$tag_number] <- "Mississippi"
  tag$Tag_state[tag$tag_number %in% texas$tag_number]       <- "Texas"
  
  table(tag$Tag_state)
  odd.tag.coordinates <- tag[tag$Tag_state == 0,]
  
  #Restrict to reasonable coordinates now that the odd ones are out
  tag <- tag %>% filter(long_decdeg < -80 & long_decdeg > -98)
  tag <- tag %>% filter(lat_decdeg > 24.5 & lat_decdeg < 31)
  
  
  
  # Recapture State Assignment -----------------------------------------
  inside.la <- !is.na(over(recaps, as(la, "SpatialPolygons")))
  inside.ms <- !is.na(over(recaps, as(ms, "SpatialPolygons")))
  inside.al <- !is.na(over(recaps, as(al, "SpatialPolygons")))
  inside.tx <- !is.na(over(recaps, as(tx, "SpatialPolygons")))
  inside.fl <- !is.na(over(recaps, as(fl, "SpatialPolygons")))
  
  #plot to check
  plot(tx,xlim = c(-97.4, -81.2), ylim = c(25, 30.5), main = "Recapture Location Assignment")
  plot(la,add = TRUE)
  plot(ms,add = TRUE)
  plot(al,add = TRUE)
  plot(fl,add = TRUE)
  map("world", region="usa", add=TRUE)
  points(recaps[inside.al, ], pch=1, col="purple")
  points(recaps[inside.la, ], pch=1, col="red")
  points(recaps[inside.fl, ], pch=1, col="green")
  points(recaps[inside.ms, ], pch=1, col="orange")
  points(recaps[inside.tx, ], pch=1, col="blue")
  
  #add recapture state
  recaps <- as.data.frame(recaps)
  recaps$Recap_state <- rep(0,nrow(recaps))
  
  alabama     <- recaps[inside.al == TRUE,]
  louisiana   <- recaps[inside.la == TRUE,]
  florida     <- recaps[inside.fl == TRUE,]
  mississippi <- recaps[inside.ms == TRUE,]
  texas       <- recaps[inside.tx == TRUE,]
  
  #Label them based on where recapture occurred
  recaps$Recap_state[recaps$tag_number %in% alabama$tag_number]     <- "Alabama"
  recaps$Recap_state[recaps$tag_number %in% louisiana$tag_number]   <- "Louisiana"
  recaps$Recap_state[recaps$tag_number %in% florida$tag_number]     <- "Florida"
  recaps$Recap_state[recaps$tag_number %in% mississippi$tag_number] <- "Mississippi"
  recaps$Recap_state[recaps$tag_number %in% texas$tag_number]       <- "Texas"
  
  #summary and odd coordinates
  table(recaps$Recap_state)
  odd.recap.coordinates <- tag[recaps$Recap_state == 0,]
  
  #Restrict to reasonable coordinates to remove weird points
  recaps <- recaps %>% filter(Rlong < -80 & Rlong > -98)
  recaps <- recaps %>% filter(Rlat > 24.5 & Rlat < 31)
  
  #remove temporary things from environment
  rm(al)
  rm(alabama)
  rm(fl)
  rm(florida)
  rm(la)
  rm(louisiana)
  rm(ms)
  rm(mississippi)
  rm(tx)
  rm(texas)
  rm(inside.al)
  rm(inside.fl)
  rm(inside.la)
  rm(inside.tx)
  rm(inside.ms)
  
  
  
  # Tagging Basin Assignment --------------------------------------------
  coordinates(tag)   <- ~long_decdeg + lat_decdeg 
  coordinates(recaps)<- ~Rlong + Rlat
  
  #PC Basin Source File Destinations
  barataria   <- readOGR(here::here("Data", "spatial_files", "Basin_Shapefiles", "Barataria.shp"))
  bigbend     <- readOGR(here::here("Data", "spatial_files", "Basin_Shapefiles", "bigbend.shp"))
  panhandle   <- readOGR(here::here("Data", "spatial_files", "Basin_Shapefiles", "Destintostandrews.shp"))
  center.gulf <- readOGR(here::here("Data", "spatial_files", "Basin_Shapefiles", "PascagoulatoPerdido.shp"))
  ms_sound    <- readOGR(here::here("Data", "spatial_files", "Basin_Shapefiles", "PonchtoHornIsland.shp"))
  terrebonne  <- readOGR(here::here("Data", "spatial_files", "Basin_Shapefiles", "Terrebonne.shp"))
  
  crs(tag)    <- crs(barataria)
  crs(recaps) <- crs(barataria)
  
  
  inside.ba <- !is.na(over(tag, as(barataria, "SpatialPolygons")))
  inside.bb <- !is.na(over(tag, as(bigbend, "SpatialPolygons")))
  inside.ph <- !is.na(over(tag, as(panhandle, "SpatialPolygons")))
  inside.cg <- !is.na(over(tag, as(center.gulf, "SpatialPolygons")))
  inside.ms <- !is.na(over(tag, as(ms_sound, "SpatialPolygons")))
  inside.te <- !is.na(over(tag, as(terrebonne, "SpatialPolygons")))
  
  
  # Test Plot
  plot(barataria,xlim = c(-95,-80.02095), ylim = c(27,30.95844), main = "Tag Basin Assignment")
  plot(bigbend,add = TRUE)
  plot(panhandle,add = TRUE)
  plot(center.gulf,add = TRUE)
  plot(ms_sound,add = TRUE)
  plot(terrebonne,add = TRUE)
  map("world", region="usa", add=TRUE)
  points(tag[inside.ba, ], pch=1, col="purple")
  points(tag[inside.bb, ], pch=1, col="red")
  points(tag[inside.ph, ], pch=1, col="green")
  points(tag[inside.cg, ], pch=1, col="orange")
  points(tag[inside.ms, ], pch=1, col="blue")
  points(tag[inside.te, ], pch=1, col="light blue")
  
  #change tag back to dataframe, assign tag basin by containment
  tag <- as.data.frame(tag)
  tag$Tag_Basin <- rep(0,nrow(tag))
  
  #Identify the tags that were released in each basin/region
  Barataria    <- tag[inside.ba == TRUE,]                 #Barataria
  BigBend      <- tag[inside.bb == TRUE,]                  #Bigbend
  Panhandle    <- tag[inside.ph == TRUE,]                  #Panhandle
  Central_Gulf <- tag[inside.cg == TRUE,]                  #Mobile Bay area
  MS_Sound     <- tag[inside.ms == TRUE,]                  #Pontchartrain, MS Sound
  Terrebonne   <- tag[inside.te == TRUE,]                  #Terrebonne Bay
  
  #Label them based on the tagging location
  tag$Tag_Basin[tag$tag_number %in% Barataria$tag_number]    <- "Barataria"
  tag$Tag_Basin[tag$tag_number %in% BigBend$tag_number]      <- "BigBend"
  tag$Tag_Basin[tag$tag_number %in% Panhandle$tag_number]    <- "Panhandle"
  tag$Tag_Basin[tag$tag_number %in% Central_Gulf$tag_number] <- "Central_Gulf"
  tag$Tag_Basin[tag$tag_number %in% MS_Sound$tag_number]     <- "MS_Sound"
  tag$Tag_Basin[tag$tag_number %in% Terrebonne$tag_number]   <- "Terrebonne"
  
  table(tag$Tag_Basin)
  
  
  # Recapture Basin Assignment ---------------------------------------------------------
  inside.ba <- !is.na(over(recaps, as(barataria, "SpatialPolygons")))
  inside.bb <- !is.na(over(recaps, as(bigbend, "SpatialPolygons")))
  inside.ph <- !is.na(over(recaps, as(panhandle, "SpatialPolygons")))
  inside.cg <- !is.na(over(recaps, as(center.gulf, "SpatialPolygons")))
  inside.ms <- !is.na(over(recaps, as(ms_sound, "SpatialPolygons")))
  inside.te <- !is.na(over(recaps, as(terrebonne, "SpatialPolygons")))
  
  
  # Test Plot
  plot(barataria,xlim = c(-95,-80.02095), ylim = c(27,30.95844), main = "Recaptures Basin Assignment")
  plot(bigbend,add = TRUE)
  plot(panhandle,add = TRUE)
  plot(center.gulf,add = TRUE)
  plot(ms_sound,add = TRUE)
  plot(terrebonne,add = TRUE)
  map("world", region="usa", add=TRUE)
  points(recaps[inside.ba, ], pch=1, col="purple")
  points(recaps[inside.bb, ], pch=1, col="red")
  points(recaps[inside.ph, ], pch=1, col="green")
  points(recaps[inside.cg, ], pch=1, col="orange")
  points(recaps[inside.ms, ], pch=1, col="blue")
  points(recaps[inside.te, ], pch=1, col="light blue")
  
  #add recapture Basins
  recaps <- as.data.frame(recaps)
  recaps$Recap_Basin <- rep(0,nrow(recaps))
  
  
  #Identify where the recaptures took place
  Barataria    <- recaps[inside.ba == TRUE,]
  BigBend      <- recaps[inside.bb == TRUE,]
  Panhandle    <- recaps[inside.ph == TRUE,]
  Central_Gulf <- recaps[inside.cg == TRUE,]
  MS_Sound     <- recaps[inside.ms == TRUE,]
  Terrebonne   <- recaps[inside.te == TRUE,]
  
  #Label them based on location
  recaps$Recap_Basin[recaps$tag_number %in% Barataria$tag_number]    <- "Barataria"
  recaps$Recap_Basin[recaps$tag_number %in% BigBend$tag_number]      <- "BigBend"
  recaps$Recap_Basin[recaps$tag_number %in% Panhandle$tag_number]    <- "Panhandle"
  recaps$Recap_Basin[recaps$tag_number %in% Central_Gulf$tag_number] <- "Central_Gulf"
  recaps$Recap_Basin[recaps$tag_number %in% MS_Sound$tag_number]     <- "MS_Sound"
  recaps$Recap_Basin[recaps$tag_number %in% Terrebonne$tag_number]   <- "Terrebonne"
  
  
  table(recaps$Recap_Basin)
  
  #clean up the environment
  rm(Barataria)
  rm(barataria)
  rm(BigBend)
  rm(bigbend)
  rm(Central_Gulf)
  rm(center.gulf)
  rm(MS_Sound)
  rm(ms_sound)
  rm(Panhandle)
  rm(panhandle)
  rm(terrebonne)
  rm(Terrebonne)
  rm(inside.ba)
  rm(inside.bb)
  rm(inside.cg)
  rm(inside.ms)
  rm(inside.ph)
  rm(inside.te)
  
  
  # Joining By Tag Number --------------------------------------------------
  both <- inner_join(recaps, tag, by = 'tag_number')  
  
  #Re-order so that tagging info comes first 
  both <- both %>% 
    dplyr::select(tag_number, 
                  date, 
                  season, 
                  Tag_state, 
                  Tag_Basin, 
                  basin_helen, 
                  area_helen, 
                  lat_decdeg, 
                  long_decdeg, 
                  cw, molt_stage, 
                  repro_stage, 
                  salinity_sur, 
                  temp_sur, 
                  DO_sur,
                  sal_bot, 
                  temp_bot, 
                  DO_bot, 
                  depth, 
                  notes, 
                  tagged_by, 
                  entered_by, 
                  everything()) 
  
  
  
  # Creating Time at large column ------------------------------------------
  both$time_at_large <- as.numeric(both$date_recaptured - both$date)
  summary(both$time_at_large)
  
  #Troubleshooting Dates
  both[which(both$time_at_large > 1000), c('tag_number','date','date_recaptured')] #Wrong Years TAL
  both[which(both$time_at_large > 365),  c('tag_number','date','date_recaptured')]  #over a year TAL
  neg_tal <- both[which(both$time_at_large < 0),] #negative TAL
  
  
  #only use positive or usable time at larges
  bad_dates <- both %>% filter(time_at_large < 0 | is.na(time_at_large) == T)
  both <- both %>% filter(time_at_large >= 0)
  #both <- both[which(both$time_at_large >= 0),]
  
  hist(both$time_at_large, xlab = 'Time at large (days)', main = 'Time at Large', col = 'light blue')
  summary(both$time_at_large)
  
  
  #  Displacement and bearing of all crabs ----------------------------------------------------------
  #dataframe of beginning points and one of ending points
  #Only really useful for creating spatial lines, potentially not necessary, good for showing lines of suspicious length
  
  
  both <- both %>% filter(long_decdeg < -80 & long_decdeg > -98)
  both <- both %>% filter(lat_decdeg > 24.5 & lat_decdeg < 31)
  both <- both %>% filter(Rlong < -80 & Rlong > -98)
  both <- both %>% filter(Rlat > 24.5 & Rlat < 31)
  
  
  begin.coord <- data.frame(lon = both[,"long_decdeg"], lat = both[,"lat_decdeg"])
  end.coord   <- data.frame(lon = both[,"Rlong"], lat = both[,"Rlat"])
  
  degree.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84") #Degree coordinate reference syster WGS 1984
  meters.crs <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")
  
  
  #starting coordinates
  coordinates(begin.coord)<- ~ lon + lat # converts the file to a spatialPoints object
  crs(begin.coord) <-crs(degree.crs) # sets the crs to degrees
  begin.coord <-spTransform(begin.coord, crs(meters.crs)) # transforms to meters
  
  #Ending coordinates
  coordinates(end.coord)<- ~ lon + lat # converts the file to a spatialPoints object
  crs(end.coord) <-crs(degree.crs) # sets the crs to degrees
  end.coord <-spTransform(end.coord, crs(meters.crs)) # transforms to meters
  
  begin.coord <- as.data.frame(begin.coord)
  end.coord <- as.data.frame(end.coord)
  
  
  #  Create list of lines, using dataframe not spatial points, make sure they are in correct crs
  l <- vector("list", nrow(begin.coord))
  library(sp)
  for (i in seq_along(l)) {
    l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
  }
  
  SpatialLines(l)
  
  
  #Creation of Dataframe containing displacement distance, bearing, and Tag number for circular plotting
  #this will fail with any latitudes/longitudes that are impossible
  library(geosphere)
  dist.dir.df <- data.frame(matrix(NA, ncol = 3, nrow = length(l)))
  for(i in 1: length(l)){
    dist.dir.df[,1]  <- both[,"tag_number"]
    dist.dir.df[i,2] <- bearing(c(both$long_decdeg[i],both$lat_decdeg[i]), c(both$Rlong[i],both$Rlat[i]), f = 0) #default crs for bearing and dist cosine is WGS 1984
    dist.dir.df[i,3] <- distCosine(c(both$long_decdeg[i],both$lat_decdeg[i]), c(both$Rlong[i],both$Rlat[i])) #function for distance, answer in meters
    
  }
  
  dist.dir.df <- dist.dir.df %>% rename(tag_number = X1, Bearing = X2, Displacement_meters = X3) #Change names to things that matter
  
  
  #make sure nrow of both and new dataframe are equal, # add columns or merge by tag number
  if ( nrow(both) == nrow(dist.dir.df)){
    both <- merge(both, dist.dir.df, by = 'tag_number')
  }
  
  #Adjust bearing and get distance in km
  both$windrose_bearing <- ifelse(both$Bearing <= 0, both$Bearing + 360, both$Bearing )
  both$Displacement_km  <- both$Displacement_meters/1000
  
  #travelrate in km/day
  both$travelrate <- both$Displacement_km/both$time_at_large
  
  
  #  Crabs whose travel distance is suspiciously long, >100km  ------------------------------------------------------------
  error_lines_export <- both[both$Displacement_km > 100,]
  error_lines_export[,c("X","eggs","state","cw","molt_stage","repro_stage","salinity_sur","temp_sur","notes")] <- NULL
  #to plot them, need to plot map in meters, and need to make list of them as spatial lines
  #Check rose_diag_test.R for code
  
  
  #--------------------------------------------------Plotting the lines----------------------------------
  mosaic.aggregate <- raster(here::here("Data", "spatial_files", "gulf_rasters", "gulfmosaic_aggregate.tif"))
  
  #Plot all travel vectors
  plot(mosaic.aggregate, main = "All Displacements")
  for(i in 1:length(l))
  {lines(l[[i]])}
  
  #plotting lines of length >100km
  error_lines <- which(dist.dir.df$Displacement_meters > 100000)
  plot(mosaic.aggregate, main = ">100km displacement")
  for(i in 1:length(error_lines)){
    lines(l[[error_lines[i]]])
  }
  
  
  rm(dist.dir.df); rm(l); rm(begin.coord); rm(end.coord); rm(i)
  
  
  
  #-------------------------------------------Saving and Exporting of CSV files---------------------------------
  #PC File Destinations
  both.save.name              <- here::here("Data", "processed_data", "Merge_Update.csv")
  tag.save                    <- here::here("Data", "processed_data", "tags_noduplicates.csv")
  recap.save                  <- here::here("Data", "processed_data", "recaps_no_duplicates.csv")
  negative.timeatlarge        <- here::here("Data", "processed_data", "negative_tal.csv")
  dupli.save.name             <- here::here("Data", "processed_data", "duplicate_tagnumbers.csv")
  multicap.save               <- here::here("Data", "processed_data", "multiple_recaps.csv")
  odd.tag.save                <- here::here("Data", "processed_data", "odd_points_tag.csv")
  odd.recap.save              <- here::here("Data", "processed_data", "odd_points_recap.csv")
  notag.save                  <- here::here("Data", "processed_data", "notag.csv")
  suspicous_displacement_save <- here::here("Data", "processed_data", "suspicious_travel_dist.csv")
  
  
  
  
  
  #Save updated merge CSV-----
  write_csv(both,                  path = both.save.name)
  write_csv(tag,                   path = tag.save)
  write_csv(recaps,                path = recap.save)
  write_csv(neg_tal,               path = negative.timeatlarge)
  write_csv(duplicates,            path = dupli.save.name)
  write_csv(multi_recaps,          path = multicap.save)
  write_csv(odd.tag.coordinates,   path = odd.tag.save)
  write_csv(odd.recap.coordinates, path = odd.recap.save)
  write_csv(no_tag,                path = notag.save)
  write_csv(error_lines_export,    path = suspicous_displacement_save)
  
}