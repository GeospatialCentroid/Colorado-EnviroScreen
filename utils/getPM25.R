
getPM25 <- function(filePath, geometry){
  
  #read in and clean data
  d1 <- vroom(filePath) %>% 
    #filter just colorado tracts
    filter(str_starts(FIPS, "08"))%>%
    #concentration was read in as character
    mutate(Conc = as.numeric(`pm25_daily_average(ug/m3)`)) %>% 
    group_by(FIPS) %>% 
    summarise(pm25_mean = mean(Conc)) %>% 
    #rename as tract for calculation below
    dplyr::select(tract = FIPS, pm25_mean)
  
    
  #when geometry is tract or block level...
  if(nchar(geometry$GEOID[1]) >= nchar(d1$tract[1])) {
    
    #add tract-level column to use as join then keep original geoid (tract or block)
    pm25 <- as.data.frame(geometry) %>% mutate(tract = str_sub(GEOID, start = 1, end = 11)) %>% 
      left_join(d1, by = "tract") %>% dplyr::select(GEOID, pm25 = pm25_mean) 
    
  } else {
    
    # when geometry is county level.. just cut FIPS to county level and group by that
   pm25 <-  d1 %>% mutate(GEOID = str_sub(tract, start = 1, end = 5)) %>% 
      group_by(GEOID) %>% 
      summarise(pm25 = mean(pm25_mean))
    
  }
  
  return(pm25)
  
}




