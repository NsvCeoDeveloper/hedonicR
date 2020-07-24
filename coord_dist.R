#####
centre <- data.frame(1:3529732)
centre$X1.3529732 <- NULL
centre$longitude <- as.numeric(-0.118092)
centre$latitude <- as.numeric(51.509865)
###
distance <- data.frame(1:3529732)
distance$X1.3529732 <- NULL
distance$longitude <- data_hotel$longitude
distance$latitude <- data_hotel$latitude

xy1 <- distance[,c(1,2)]

xy2 <- centre[,c(1,2)]

spdf1 <- SpatialPointsDataFrame(coords = xy1, data = distance,
                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

spdf2 <- SpatialPointsDataFrame(coords = xy2, data = centre,
                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


lonodn_dist <- geosphere::distHaversine(spdf2, spdf1, r=6378137)

data_hotel$lonodn_dist <- lonodn_dist

#####
# ward - shp
if(exists('ward')){
  ward_polygons <- list()
  ward_dist <- list()
  ##
  for(i in seq_along(ward)){
    ward_centre = gCentroid(ward)
    ward_polygons[[i]] <- ward[i,]
    ward_dist[[i]] <- gDistance(ward_centre, gCentroid(ward_polygons[[i]]))
  }
  ##
  dist_ward <- data.frame(ward = ward@data$ward, 
                          ward_dist = unlist(ward_dist))
  ##
  #rm(list=ls(pattern='ward_'))
  #rm(i)
}
####
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
####
if(exists('ward')){
  centroid_ward <- data.frame(geosphere::centroid(ward))
  colnames(centroid_ward) <- c('lon',
                               'lat')
  coordinates(centroid_ward) <- ~ lon + lat 
  proj4string(centroid_ward) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  centroid_ward <- spTransform(centroid_ward, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +units=m +no_defs"))
  centroid_ward <- data.frame(ward = ward@data$ward,
                              centroid_ward@coords)
  centroid_ward <- subset(centroid_ward,select=c(1,3,2))
  colnames(centroid_ward) <- c('ward',
                               'lat_centr_ward',
                               'lon_centr_ward')
  data_airbnb <- merge(data_airbnb,
                       centroid_ward,
                       by = 'ward')
  ##
  if(exists('data_airbnb')){
    data_airbnb$a <- geosphere::distHaversine(centroid_ward[,c('lat_centr_ward','lon_centr_ward')],
                                              data_airbnb[,c('latitude','longitude')], r=6378137)
  }
  ###
  data_airbnb$lat_centr_ward <- NULL
  data_airbnb$lon_centr_ward <- NULL
}