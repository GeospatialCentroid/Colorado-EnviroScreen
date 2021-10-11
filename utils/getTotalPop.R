###
# generate urban/rural based on popluation density 
# carverd@colostate.edu
# 20211006
###

# not done yet but only we get the population density information we should be 
# able to wrap this up 


### testing 
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson") 
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# 
# tic()
# d1 <- getTotalPop(geometry)
# toc()
# View(d1)

getTotalPop <- function(geometry){
  #condition to grab dataset if it doesn't exist 
  if(!file.exists("data/population/censusBlockPopulation.csv")){
    pop <- tidycensus::get_acs( 
      geography = "block group",
      variables = c("B01003_001"),
      state = "08",
      year = 2019
    )
    write.csv(pop, "data/population/censusBlockPopulation.csv")
  }else{
    pop <- vroom::vroom("data/population/censusBlockPopulation.csv")
  }
  # grab the length of the geoid for reference 
  idLength <- nchar(geometry$GEOID[1])
  
  # test for match on GEOID then aggregate 
  if(idLength == nchar(pop$GEOID[1])) {
    #add tract-level column to use as join then keep original geoid (tract or block)
    population <- geometry %>%
      dplyr::left_join(pop, by = "GEOID") %>% 
      dplyr::select(GEOID, population = estimate) 
  }else{
    # when geometry is not census block group cut FIPS to level and group by that
    p1 <-  pop %>% 
      dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = idLength)) %>% 
      dplyr::group_by(GEOID) %>% 
      dplyr::summarise(population = sum(estimate, na.rm = TRUE))
    # join back to geomtry to ensure that the match is present  
    population <- geometry %>% 
      dplyr::left_join(y = p1, by = "GEOID")%>%
      dplyr::select(GEOID,population)
  }
  # generate area measure 
  population$area <- st_area(population)
  # calculate sq mile area and population density  
  population2 <- population %>%
    sf::st_transform(crs = 5070) %>%
    dplyr::mutate(
      squareMiles = area * 0.000000386102,
      popDensity = population/squareMiles
    )%>%
    as.data.frame() %>%
    dplyr::select(GEOID, population, area, squareMiles, popDensity)
  
  ### what we want from this is a urban rural layer so it's not about population
  return(population2)
}
