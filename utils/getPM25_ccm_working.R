# function to process pm2.5 indicator

library(dplyr)
library(vroom)
library(raster)
library(tigris)
library(sf)
library(leaflet)

# 12 km raster --------------------------------------

#read and process file first

# use vroom, large csv
pm25 <- vroom("data/ds.input.cmaq.pm25.2017.csv")

#investigate data
pm25 %>% distinct(Col, Row, .keep_all = TRUE)
#duplicate columns,rows..so duplicate values. 
pm25 %>% distinct(Date)
#value for every day of the year

#clean up so only one row per point
pm25_clean <- pm25 %>% group_by(Lon, Lat) %>% 
  mutate(pm25_mean = mean(Conc), pm25_median = median(Conc)) %>% distinct(Lon, Lat, .keep_all = TRUE)

pm25_raster <- rasterFromXYZ(pm25[,c("Lon", "Lat", "Conc")])
#not on regular grid, must use rasterize


#create empty raster based on num cols and rows

ncols <- pm25 %>% pull(Col) %>% unique() %>% length()
nrows <- pm25 %>% pull(Row) %>% unique() %>% length()
  
#Use the same crs used to develop the files (from CMAS website/metadata)
r <- raster(ncols = ncols, nrows = nrows, crs = "+init=epsg:4326",
            xmn = min(pm25_clean$Lon), xmx = max(pm25_clean$Lon),
            ymn = min(pm25_clean$Lat), ymx = max(pm25_clean$Lat))
              
              #"+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")


pm25_ras_median <- rasterize(pm25_clean[,c("Lon", "Lat")], r, field = pm25_clean[,"pm25_median"],
                         fun = median)

pm25_ras_mean <- rasterize(pm25_clean[,c("Lon", "Lat")], r, field = pm25_clean[,"pm25_mean"],
                           fun = mean)


#get census block geometry
blockGroups <- tigris::block_groups(state = "08") %>% 
  st_transform(crs ="+proj=longlat +datum=WGS84" )


#get single value per block group

## clip to block group extent
pm25_co <- 
  crop(pm25_raster, extent(blockGroups))


#extract raster values per polygon using median value

blockGroups$pm25 <- extract(pm25_co,
                      blockGroups,
                      fun = median)

#check it out
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = blockGroups, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", pm25)(pm25))


#census tract -------------------------------------------

#now do census tract to compare to other file produced at tract level (below)
tract <- tigris::tracts(state = "CO") %>% 
  st_transform(crs ="+proj=longlat +datum=WGS84" )


## clip both rasters to block group extent
pm25_ras_median_tract <- 
  crop(pm25_ras_median, extent(tract))

pm25_ras_mean_tract <- 
  crop(pm25_ras_mean, extent(tract))


#extract raster values per polygon using median and mean value

tract$pm25_ras_median <- extract(pm25_ras_median_tract,
                            tract,
                            fun = median)

tract$pm25_ras_mean <- extract(pm25_ras_mean_tract,
                               tract,
                               fun = mean)


#check it out, compare mean and median
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", pm25_ras_median)(pm25_ras_median))


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", pm25_ras_mean)(pm25_ras_mean))

# census tract level -------------------------------------

#takes a while
# pm25_block <- read.table(gzfile("data/2017_pm25_daily_average.txt.gz"), header = TRUE,
#                          sep = ",") 
# 

#try vroom
pm25_tract <- vroom("data/2017_pm25_daily_average.txt.gz")


#clean data
pm25_tract_clean <- pm25_tract %>% mutate(Conc = as.numeric(`pm25_daily_average(ug/m3)`),
                                          Latitude = as.numeric(Latitude)) %>% 
  dplyr::select(Date, GEOID = FIPS, Longitude, Latitude, Conc) %>% 
  group_by(Longitude, Latitude) %>% 
  mutate(Conc = mean(Conc)) %>% distinct(Longitude, Latitude, .keep_all = TRUE)


pm25_tract_clean2 <- pm25_tract %>% mutate(Conc = as.numeric(`pm25_daily_average(ug/m3)`)) %>% 
  group_by(FIPS) %>% 
  mutate(pm25_tract_mean = mean(Conc), pm25_tract_median = median(Conc)) %>% distinct(FIPS, .keep_all = TRUE) %>% 
  dplyr::select(GEOID = FIPS, pm25_tract_mean, pm25_tract_median)



#join pm25 tract level data to tract geometry

tract <- tract %>% left_join(pm25_tract_clean2, by = "GEOID")


#save as data frame to compare values
pm25_comparison <- tract %>% dplyr::select(GEOID, pm25_ras_mean, pm25_tract_mean, 
                                           pm25_ras_median, pm25_tract_median)
st_geometry(pm25_comparison) <- NULL
write.csv(pm25_comparison, "data/pm25_comparison.csv")


#producing very different values....

# tract_pm25 <- tigris::tracts(state = "CO") %>% 
#   st_transform(crs ="+proj=longlat +datum=WGS84" )
#   left_join(pm25_tract_clean, by = "GEOID")


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", Conc)(Conc),
              popup = ~Conc)


  

#EJSCREEN calculates annual average for pm2.5 at tract level, block groups
#are assigned value of tract. Current ejscreen tool uses 2011 data (according to technical docs)
#EJSCREEN uses downscaling method (what created the tract level data)


