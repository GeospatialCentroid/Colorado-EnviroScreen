###
# processPM25 data
# carverd@colostate.edu
# 20210827
### 


#### processed census tract data 

# read in dataset 
d1 <- data.table::fread("C:/Users/carverd/Downloads/2017_ozone_daily_8hour_maximum.txt")
### filter by state geo 
# read in state data
colorado <- sf::st_read("data/state/colorado.geojson")%>%
  sf::st_bbox()

# filter by lat long from bbox of colorado 
d2 <- d1 %>%
  dplyr::filter(Longitude >= colorado[1] & Longitude <= colorado[3] & 
                  Latitude >= colorado[2] & Latitude <= colorado[4]) %>%
  dplyr::group_by(FIPS)%>%
  dplyr::summarise(medainConc = median(`ozone_daily_8hour_maximum(ppb)`))%>%
  dplyr::mutate(
    FIPS = as.character(FIPS)
  )


unique(geom$GEOID[1] %in% d2$FIPS)


# intesect with geometry feature 
geom <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")

## this should work with case_when it's just not coming together at the mement
# g1 <- geom %>%
#   dplyr::mutate(
#     ozone = case_when(
#       str_detect(GEOID, d2$FIPS) - d2$FIPS[str_detect(GEOID, d2$FIPS)]
#     )
#   )

# replace when possilbe with case_when process 
geom$ozone <- NA
for(i in 1:nrow(geom)){
  geom$ozone[i] <- d2$medainConc[str_detect(geom$GEOID[i], d2$FIPS)]
}
# subset to select features of interest 
geom1 <- geom %>%
  as.data.frame()%>%
  dplyr::select(GEOID, ozone)


#### 12k grid data 

install.packages("terra")
library(dplyr)
library(raster)
# library(terra) not working try again later
library(sf)
library(data.table)

# read in point data 
# data from EPA CMAQ is in NAD83
# convert to a raster 
# process to spatial extent 
# wrap into a function 


processPM25 <- function(filePath, geometry){
  # read in data
  d1 <- data.table::fread(input = "D:/geoSpatialCentroid/coEnvrioScreen/data/epa_CMAC/ds.input.cmaq.pm25.2017.csv")
  
  # drop by general lat long of state 
  colorado <- sf::st_read("data/state/colorado.geojson")
  # grab the bounding box
  cbb<- sf::st_bbox(colorado)

  # filter by lat long from bbox of colorado 
  d2 <- d1 %>%
    dplyr::filter(Lon >= cbb[1] & Lon <= cbb[3] )%>%
    dplyr::filter(Lat >= cbb[2] & Lat <= cbb[4]) 
  
  # this has multiple years connected to it 
  d3 <- d2 %>%
    dplyr::group_by(Lon, Lat)%>%
    dplyr::summarise(medianConc = median(Conc, na.rm = TRUE))

  #create template raster
  r <- raster(extent(colorado), ncol=length(unique(d3a$Lon)), nrow=length(unique(d3a$Lat)),
              crs = crs(colorado))

  # you need to provide a function 'fun' for when there are multiple points per cell
  x <- rasterize(x= d3[,1:2],
                 y= r,
                 field = d3[,3])
  plot(x)
# 
#   # create sp object to turn to raster 
#   coords <- d3[,c("Lon","Lat")]
#   sp1 <- sp::SpatialPointsDataFrame(coords = coords, 
#                                     data = as.data.frame(d3$medianConc),
#                                     proj4string = raster::crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#   #convert to nad83
#   sp2 <- sp::spTransform(x = sp1, CRSobj = raster::crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs "))
#   
#   d3a <- as.data.frame(sp2)
#   
# 
#   # set up an 'empty' raster, here via an extent object derived from your data
#   e <- extent(colorado)
#   
#   r <- raster(e, ncol=length(unique(d3a$Lon)), nrow=length(unique(d3a$Lat)),
#               crs = crs(colorado))
#   # or r <- raster(xmn=, xmx=,  ...
#   
#   # you need to provide a function 'fun' for when there are multiple points per cell
#   x <- raster::rasterize(x= coords,
#                  y= r,
#                  field = d3$medianConc)
#   plot(x)
#   
#   
  
  
  
  
  
  
  
  
  
  
  
  
  
  # process to spatial extent 
  goem1 <- raster::extract(x = r1 , y = geometry, method = simple, fun = median)
  
  
}





