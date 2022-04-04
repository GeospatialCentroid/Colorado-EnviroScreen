# filePath  <- "data/noise/CONUS_L50dBA_sumDay_exi.tif"

getNoise <- function(filePath, geometry){
  # extract average value to each features
  # read in datasets 
  r1 <- terra::rast(filePath)
  g2 <- geometry %>%
    dplyr::select("GEOID")
  
  # convert to terra object 
  g3 <- vect(g2)%>%
    terra::project(r1)
  
  # grab values 
  r2 <- terra::extract(r1, g3, mean, na.rm = TRUE)
  
  # attached GEOID to datasets 
  geom <- dplyr::bind_cols(st_drop_geometry(g2), r2) %>%
    dplyr::select(GEOID, noiseLevel = CONUS_L50dBA_sumDay_exi)
  
  #output the object 
  return(geom)
}