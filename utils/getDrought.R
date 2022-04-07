


getDrought <- function(filePath, geometry, startYear, endYear){
  d1 <- read_csv(filePath)
  # fitler to specific 2016-2020 to ensure full year is present in data 
  d2 <- d1 %>%
    dplyr::filter(ValidStart  > as.Date(paste0(startYear,"-01-01")) & ValidStart  < as.Date(paste0(endYear,"-01-01")))%>%
    rowwise() %>%
    mutate(percentArea = sum(D2,D3,D4))
  # calculate average area in drought 
  d3 <- d2  %>%
    dplyr::select(FIPS,percentArea)  %>%
    dplyr::group_by(FIPS)%>%
    dplyr::summarise(averageAreaInDrought = mean(percentArea), sumAreaInDrought = sum(percentArea), totalWeeks = n())
  # calculate number of weeks with some drought 
  d4 <- d2 %>%
    dplyr::filter(percentArea != 0)%>%
    dplyr::select(FIPS,percentArea)  %>%
    dplyr::group_by(FIPS)%>%
    dplyr::summarise(weeksWithDrought = n())
  # sum values in cat d2 d3 adn d4
  d5 <- dplyr::left_join(d3, d4, by ="FIPS")%>%
    dplyr::mutate(percentTimeInDrought = (weeksWithDrought/totalWeeks)*100)
  
  #write_csv(x = d5, file = paste0("data/drought/droughtData_",startYear,"_",endYear,".csv"))
  
  # join to goemetry elements 
  geom <- st_drop_geometry(geometry)%>%
    dplyr::mutate("FIPS" = str_sub(GEOID, start = 1, end = 5))%>%
    dplyr::left_join(d5, by ="FIPS")%>%
    dplyr::select("GEOID", "averageAreaInDrought", "sumAreaInDrought", "weeksWithDrought", "percentTimeInDrought")
  
  # utilizing sum area in drought as the indicator 
  geom <- geom %>%
    dplyr::select(GEOID, drought = sumAreaInDrought)
  
  return(geom)
    
}