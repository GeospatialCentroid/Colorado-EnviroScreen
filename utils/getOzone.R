###
# process the ozone data  
# carverd@colostate.edu
# 20210903
###

# path for testing the function 
# filePath <- "data/epa_cmaq/2017_ozone_daily_8hour_maximum.txt"
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
### 


getOzone <- function(filePath, geometry){
  
  x <- c("vroom", "sf","dplyr")
  lapply(x, require, character.only = TRUE)
  
  # read in dataset - fread for large files
  d1 <- vroom::vroom(filePath) %>%
    dplyr::filter(str_starts(FIPS, "08"))%>%
    dplyr::group_by(FIPS)%>%
    dplyr::mutate(Conc = as.numeric(`ozone_daily_8hour_maximum(ppb)`))%>%
    group_by(FIPS) %>% 
    summarise(ozone_mean = mean(Conc)) %>% 
    #rename as tract for calculation below
    dplyr::select(tract = FIPS, ozone_mean)
                    
                    
    #when geometry is tract or block level...
    if(nchar(geometry$GEOID[1]) >= nchar(d1$tract[1])) {
      #add tract-level column to use as join then keep original geoid (tract or block)
      ozone <- as.data.frame(geometry) %>% 
        dplyr::mutate(tract = str_sub(GEOID, start = 1, end = 11)) %>% 
        dplyr::left_join(d1, by = "tract") %>% 
        dplyr::select(GEOID, ozone = ozone_mean) 
    } else {
      # when geometry is county level.. just cut FIPS to county level and group by that
      ozone <-  d1 %>% 
        dplyr::mutate(GEOID = str_sub(tract, start = 1, end = 5)) %>% 
        dplyr::group_by(GEOID) %>% 
        dplyr::summarise(ozone = mean(ozone_mean))
    }
  return(ozone)
}
