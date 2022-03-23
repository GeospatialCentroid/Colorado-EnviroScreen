


getJustice40 <- function(){
  # filter justice40 to colorado 
  d1 <- read_csv("data/justice40/Screening_Tool_Data/communities-2022-03-21-1359GMT.csv")%>%
    dplyr::filter(`State/Territory` == "Colorado")
  # join with spatial data 
  ct <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")%>%
    dplyr::left_join(d1, by = c("GEOID" = "Census tract ID"))%>%
    dplyr::select("Census tract ID","County Name" , "Total threshold criteria exceeded", "Identified as disadvantaged")%>%
    dplyr::filter(`Identified as disadvantaged` == TRUE)%>%
    rmapshaper::ms_simplify()
  # export result 
  st_write(obj = ct, "data/justice40/coloradoData.geojson")
}