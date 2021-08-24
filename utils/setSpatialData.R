###
# grabs the spatial data of interest based on input paramter 
# carverd@colostate.edu
# 20210816
###

setSpatialData <- function(dataFolder, scale){
  ###
  # grabs spatial object of interest based on user defined scale
  # dataFolder : full path to file folder where data is held
  # scale: one of three options "censusBlockGroup", "censusTract", "county"
  ### 
  
  if(scale == "censusBlockGroup"){
    data <- sf::st_read(paste0(dataFolder,"/censusBlockGroup/coloradoCensusBlockGroups.geojson"))
  }
  if(scale == "censusTract"){
    data <- sf::st_read(paste0(dataFolder,"/censusTract/coloradoCensusTracts.geojson"))
  }
  if(scale == "county"){
    data <- sf::st_read(paste0(dataFolder,"/county/coloradoCounties.geojson"))
  }
  if(class(data)[1]== "sf"){
    return(data)
  }else{
   stop("No spatial object was created. Please check your input parameter")
  }
}


