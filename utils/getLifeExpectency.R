###
# process life expectency data from the CDC
# 20210920 
# carverd@colostate.edu 
###

#testing 
# library(sf)
# library(dplyr)
# library(stringr)
# filePath <- "data/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# 
# t1 <- Sys.time()
# d2 <- getLifeExpectancy(filePath = filePath, geometry = geometry)
# t2 <- Sys.time() - t1
# t2

getLifeExpectancy <- function(filePath, geometry){
  ###
  # inputs life expectancy data from the CDC. Does some transformation to get it to standard format so 
  # processing can be complete in a similar fashion to other sources.
  # filePath : relative path of the csv 
  # geometry : sf object of one of the three geomerty levels
  ###
  
  g1 <- sf::st_read("data/county/coloradoCounties.geojson")

  # read in data, filter to colorado, and separate county character column to facilitate join 
  d1 <- readr::read_csv(filePath) ## read r keeps the leading zeros 
  colnames(d1)[1] <- "State"
  
  d1 <- d1 %>%
    dplyr::filter(`State` == "Colorado",!is.na(`Census Tract Number`))%>%
    tidyr::separate(County, c("County", NA), sep = ",")%>%
    # join on the county geom feature to grab GEOID to complete future joins 
    dplyr::left_join(y = g1, by = c("County" = "NAMELSAD"))%>%
    # select significant columns. 
    dplyr::select(County, `Census Tract Number`,`Life Expectancy`,
                  `GEOID`)%>%
    dplyr::mutate(`Census Tract Number` = sprintf("%.2f", as.numeric(`Census Tract Number`)))
  
  # Life Exp data is at the census tract level, need to join data to CT geometry to get full GEOID 
  g2 <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")%>%
    # create a second geoid to join on county level 
    dplyr::mutate(GEOID2 = stringr::str_sub(GEOID, 1,5),
                  NAME = sprintf("%.2f", as.numeric(NAME)))
  # Join data base on count match and census tract match, grab full geoid and output value
  ## this is because census tract number is unqiue to county only
  d1a <- dplyr::left_join(d1, g2, by= c("GEOID" = "GEOID2",
                                        "Census Tract Number" = "NAME"))%>%
    dplyr::select(GEOID = `GEOID.y`,`Life Expectancy`)
  
  
  #The data structure hear matchs most other datasets, so apply similar process
  ### select processing level by comparing length of GEOID between objects
  if(nchar(geometry$GEOID[1]) >= nchar(d1a$GEOID[1])){ 
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- as.data.frame(geometry) %>% 
      dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = 11)) %>% 
      dplyr::left_join(d1a, by = "GEOID") %>% 
      dplyr::select(GEOID, lifeExpectancy = `Life Expectancy`) 
  }else{
    # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1a %>% 
      dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = 5))%>% 
      dplyr::group_by(GEOID) %>% 
      dplyr::summarise(lifeExpectancy = mean(`Life Expectancy`, na.rm = TRUE)) 
  }
  # replace all NaN with NA 
  geom$lifeExpectancy[is.nan(geom$lifeExpectancy)] <- NA
  
  return(geom)
}  


