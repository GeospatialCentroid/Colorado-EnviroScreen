###
# process wildfire data 
# carverd@colostate.edu
# 20210920
###



# dataset is available as interger values and classified product, using interger values 
# 5) whp2020_cnt_conus.tif: continuous integer WHP index values for the conterminous United States, and 

# #testing
# library(dplyr)
# library(tictoc)
# library(terra)
# options for faster extractions 
#library(exactextractr)
# install.packages("terra")
# library(terra)
# 
# filePath <- "data/wildfire/Data/whp2020_GeoTIF/whp2020_cnt_conus.tif"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# # 
# # library(tictoc)
# tic()
# d2 <- getWildfile(filePath = filePath, geometry = geometry)
# toc()
# # 143 second on county - raster 
# # 12.58 seconds on county, 15.66 seconds on census block group - terra 

getWildfile <- function(filePath, geometry){
  ###
  # processes raster dataset to various geometry
  # filePath : relative path to wildfire risk dataset
  # geometry : sf object representing spatial scale 
  ###
  
  x <- c("sf","dplyr", "terra")
  lapply(x, require, character.only = TRUE)
  
  # terra implementation 
  d1 <- terra::rast(filePath)
  
  # read in geometry features 
  g1 <- geometry %>%
    sf::st_transform(crs = raster::crs(d1))%>%
    dplyr::select(GEOID)
  
  # crop-
  d1 <- d1 %>%
    terra::crop(g1)
  
  # extract values 
  # assigning sf as spatvect for function, spatvect does not support same transformation as sf objects. 
  g2 <- terra::extract(d1, terra::vect(g1),fun = mean, na.rm = TRUE)
  # assign values to GEOID
  geom <- g1 %>%
    dplyr::mutate(
      wildfire = g2$whp2020_cnt_conus
    )%>%
    as.data.frame()%>%
    dplyr::select(GEOID, wildfire)

  return(geom) 
}


