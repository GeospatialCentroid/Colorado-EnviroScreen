# function to process pm2.5 indicator

library(dplyr)
library(vroom)
library(raster)
library(sf)

#read and process file first

# use vroom, large csv
pm25 <- vroom("data/ds.input.cmaq.pm25.2017.csv") %>% 
  group_by(Lon, Lat) %>% 
  mutate(Conc = median(Conc)) %>% distinct(Lon, Lat, .keep_all = TRUE)


#create empty raster based on num cols and rows

ncols <- pm25 %>% pull(Col) %>% unique() %>% length()
nrows <- pm25 %>% pull(Row) %>% unique() %>% length()
  

r <- raster(ncols = ncols, nrows = nrows, crs = "+init=epsg:4326",
            xmn = min(pm25_clean$Lon), xmx = max(pm25_clean$Lon),
            ymn = min(pm25_clean$Lat), ymx = max(pm25_clean$Lat))
              

#create raster
pm25_raster <- rasterize(pm25_clean[,c("Lon", "Lat")], r, field = pm25_clean[,"Conc"],
                         fun = median)



# now create function to get data for each geometry

getPM25 <- function(geometry){
  
  #get geometry using object created in runAll script
  geom <- geometry %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84")
  
  
  #get single value per block group
  
  ## clip to block group extent
  pm25_co <- 
    raster::crop(pm25_raster, raster::extent(geom))
  
  
  #extract raster values per polygon using median value
  
  geom$pm25 <- raster::extract(pm25_co,
                       geom,
                       fun = median)
  
  #returns single file with geoid and pm2.5
  geom %>% as_tibble() %>% dplyr::select(GEOID, pm25) 
  
  
  
}



