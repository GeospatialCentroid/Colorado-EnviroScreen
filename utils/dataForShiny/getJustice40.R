


getJustice40 <- function(filePath,overWrite = FALSE){
  
  pathToData <- "data/justice40/justice40.rds"
  
  if(file.exists(pathToData) & overWrite == FALSE){
    return(paste0("The DI community spatial data exists and can be found ", pathToData))
  }else{
    # filter justice40 to colorado 
    d1 <- read_csv(filePath)%>%
      dplyr::filter(`State/Territory` == "Colorado")
    # join with spatial data 
    county <- sf::st_read("data/county/coloradoCounties.geojson")%>% 
      st_drop_geometry()%>%
      dplyr::select("County_Name" = "NAME", "COUNTYFP")
    
    ct <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")%>%
      dplyr::left_join(y = county, by = ("COUNTYFP")) %>%
      dplyr::left_join(d1, by = c("GEOID" = "Census tract ID"))%>%
      dplyr::select("GEOID","County_Name" , "Total threshold criteria exceeded", "Identified as disadvantaged")%>%
      dplyr::filter(`Identified as disadvantaged` == TRUE)%>%
      rmapshaper::ms_simplify()
    # export result 
    saveRDS(object = ct, pathToData)
  }
}