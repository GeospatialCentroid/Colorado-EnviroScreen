###
# process the pm2.5 data to geographic of interest
# 20211004 
# carverd@colostate.edu 
###

### testing
# filePath <- "data/epa_cmaq/2017_pm25_daily_average.txt.gz"


# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")

# tic()
# d2 <- getPM25(filePath = filePath, geometry = geometry)
# toc()
# View(d2)

getPM25 <- function(filePath, geometry){
  
  #read in and clean data
  d <- vroom(filePath) %>% 
    #filter just colorado tracts
    filter(str_starts(FIPS, "08")) %>%
    #concentration was read in as character
    mutate(Conc = as.numeric(`pm25_daily_average(ug/m3)`)) %>% 
    group_by(FIPS) %>% 
    summarise(pm25_mean = mean(Conc)) %>% 
    #rename as tract for calculation below
    dplyr::select(tract = FIPS, pm25_mean)
  
    
  #when geometry is tract or block level...
  if(nchar(geometry$GEOID[1]) >= nchar(d$tract[1])) {
    
    #add tract-level column to use as join then keep original geoid (tract or block)
    pm25 <- as.data.frame(geometry) %>% mutate(tract = str_sub(GEOID, start = 1, end = 11)) %>% 
      left_join(d, by = "tract") %>% dplyr::select(GEOID, pm25 = pm25_mean) 
    
  } else {
    
    # when geometry is county level.. just cut FIPS to county level and group by that
   pm25 <-  d %>% mutate(GEOID = str_sub(tract, start = 1, end = 5)) %>% 
      group_by(GEOID) %>% 
      summarise(pm25 = mean(pm25_mean))
    
  }
  
  return(pm25)
  
}




