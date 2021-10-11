###
# Grab all generic spatial data 
# carverd@colostate.edu
# 20210813
###


### 
# functions that pulls all layers from API  
# tests for existing layers 
# parameter for pulling new information 
# points to specific file location 
# reference function to call census API key 

pullGeometryDatasets <- function(fileFolder, pullNewData){
  ###
  # function that pull required spatial data to fileFolder location of interest
  # fileFolder : full path to folder which will store data as character 
  # pullNewData : overwrite existing datasets - TRUE/FALSE
  ### 
  require("tigris", "dplyr", "sf")
  
  
  # set file folder location 
  fileFolder <- fileFolder
  # State of Colorado layer 
  # Federal Information Processing Standards(FIPS) code for Colorado
  fips <- "08"
  
  if(isTRUE(pullNewData)){
    overwrite <- TRUE
  }else{
    overwrite <- FALSE
  }
  ### State Data
  # test for existing file folder structure 
  path1 <- paste0(fileFolder, "/state")

  if(!dir.exists(path1)){
    dir.create(path1)
  }
  print("Downloading and saving state level spatial data")
  if(!file.exists(paste0(path1,"/colorado.geojson")) | overwrite == TRUE){
    # call all state data and filter to colorado 
    states <- tigris::states() %>% 
      dplyr::filter(STATEFP  == fips)
    # write object as a geojson
    sf::st_write(obj = states,
                 dsn = paste0(path1,"/colorado.geojson"),
                 driver = "GeoJSON", 
                 delete_layer = overwrite)
  }else{
        print("File exist, change pullNewData to TRUE to force Download")
      }

  ### County Data 
  # test for existing file folder structure 
  path2 <- paste0(fileFolder, "/county")
  
  if(!dir.exists(path2)){
    dir.create(path2)
  }
  
  print("Downloading and saving county level spatial data")
  if(!file.exists(paste0(path2,"/coloradoCounties.geojson")) | overwrite == TRUE){
      
    # Counties within the state of Colorado 
    counties <- tigris::counties(state = fips, year = 2019)
    # write out spatial data
    sf::st_write(obj = counties,
                 dsn = paste0(path2,"/coloradoCounties.geojson"),
                 drive = "GeoJSON",
                 delete_layer = overwrite)
  }else{
    print("File exist, change pullNewData to TRUE to force Download")
  }
  
  ### Census Tract
  # test for existing file folder structure 
  path3 <- paste0(fileFolder, "/censusTract")
  
  if(!dir.exists(path3)){
    dir.create(path3)
  }
  
  cat("Downloading and saving census tract level spatial data")
  if(!file.exists(paste0(path3,"/coloradoCensusTracts.geojson")) | overwrite == TRUE){
    # Census Tracts within the state of Colorado 
    tracts <- tigris::tracts(state = fips, year = 2019)
    # writing census tract data 
    sf::st_write(obj = tracts,
                 dsn = paste0(path3,"/coloradoCensusTracts.geojson"),
                 drive = "GeoJSON",
                 delete_layer = overwrite)
  }else{
    print("File exist, change pullNewData to TRUE to force Download")
  }
  

  
  ### Census Block Group 
  # test for existing file folder structure 
  path4 <- paste0(fileFolder, "/censusBlockGroup")
  
  if(!dir.exists(path4)){
    dir.create(path4)
  }
  
  cat("Downloading and saving census block group level spatial data")
  if(!file.exists(paste0(path4,"/coloradoCensusBlockGroups.geojson")) | overwrite == TRUE){
    # census block groups within the state of Colorado 
    blockGroups <- tigris::block_groups(state = fips, year = 2019)
    sf::st_write(obj = blockGroups,
                 dsn = paste0(path4,"/coloradoCensusBlockGroups.geojson"),
                 drive = "GeoJSON",
                 delete_layer = overwrite)
  }else{
    print("File exist, change pullNewData to TRUE to force Download")
    
  }
 
  cat("All files have been downloaded.")
}



