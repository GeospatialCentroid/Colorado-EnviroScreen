###
# primary script for running all data processing steps
# carverd@colostate.edu
# 20210816
###

# load required libraries
# install.packages("pacman")
pacman::p_load(
  tigris, # pulling spatial geometries
  tidycensus, # pulling census data
  dplyr, # data manipulation
  sf, # processing vector spatial data
  stringr, # string manipulation
  tictoc, # running processing time checks
  vroom, # loading large datasets
  terra, # processing rasters
  #tmap, # visualize spatial data
  arcpullr, # pull objects from ESRI REST api,
  purrr, # joining and other iterative processes
  # leaflet, # mapping features
  # leaflet.extras, # search functionality
  tidyr, 
  rmapshaper,
  readr,
  lubridate
)

# source functions; this is verbose, so temp object is created then removed
### if we can find a way to pass parameters to the source function within the
### lapply we can get around this.
source("utils/helperFunctions/loadFunctions.R")
loadFunctions()
# download data
### leave all relative paths as teminal "no '/'" this will be account for if a
### larger path is constructed
pullGeometryDatasets(
  fileFolder = "data",
  pullNewData = FALSE
)


# Version -----------------------------------------------------------------
version <- "4"


# tidycensus key ----------------------------------------------------------
# key <- 
# getCensusAPIKey(key)

overWriteComponentScores <- FALSE

for(val1 in c( "county","censusTract","censusBlockGroup")){ 
  processingLevel <- val1
  # call in spatial object at give extent
  geometry <- setSpatialData(dataFolder = "data/", scale = processingLevel)

  ### EJScreen and ACS data contributes to multiple components run it here then split out
  ejscreen <- getEjScreen2021(geometry = geometry ,processingLevel = processingLevel)
  ### add condition to test for the existence of a specific file based on geom
  acsData <- getACS(processingLevel = processingLevel, year = 2019)


# Exposures ---------------------------------------------------------------
  f1 <- paste0("data/envScreenScores/",processingLevel,"/envExposures_",version,".csv")
  # if(file.exists(f1) & overWriteComponentScores == FALSE){
  #   envExposures <- read_csv(f1)
  # }else{
    envExposures <- enviromentalExposures(geometry = geometry, ejscreen = ejscreen,processingLevel = processingLevel)
    write_csv(x = envExposures, f1 )
  # }

# Environmental Effects ---------------------------------------------------
  f2 <- paste0("data/envScreenScores/",processingLevel,"/envEffects_",version,".csv")
  if(file.exists(f2) & overWriteComponentScores == FALSE){
    envEffects <- read_csv(f2)
  }else{
    envEffects <- enviromentalEffects(geometry = geometry,processingLevel = processingLevel,ejscreen = ejscreen)
    write_csv(x = envEffects, f2 )
  }


# Climate Impacts ---------------------------------------------------------
  f3 <- paste0("data/envScreenScores/",processingLevel,"/climate_",version,".csv")
  if(file.exists(f3) & overWriteComponentScores == FALSE){
    climateData <- read_csv(f3)
  }else{
    climateData <- climate(geometry)
    write_csv(x = climateData, f3)
  }
  
# Socioeconomic Factors ---------------------------------------------------
  f4 <- paste0("data/envScreenScores/",processingLevel,"/senPop_",version,".csv")
  if(file.exists(f4) & overWriteComponentScores == FALSE){
    senPop <- read_csv(f4)
  }else{
    senPop <- sensitivePopulations(geometry = geometry, ejscreen = ejscreen)
    write_csv(x = senPop, f4)
  }

# Socioeconomic Factors ---------------------------------------------------
  f5 <- paste0("data/envScreenScores/",processingLevel,"/socEco_",version,".csv")
  if(file.exists(f5) & overWriteComponentScores == FALSE){
    socEco <- read_csv(f5)
  }else{
    socEco <- socioEconomicFactors(geometry,ejscreen, acsData, processingLevel = processingLevel)
    write_csv(x = socEco, f5)
  }
  
  # merge all datasets on geoid
  # apply function across all features
  dataframes <- list(envExposures,envEffects,climateData,senPop,socEco)
  
  # compile final component scores 
  df <- finalComponentScore(dataframes)

  # write output ------------------------------------------------------------
  write_csv(df,path = paste0("data/envScreenScores/",processingLevel,"_",version,".csv"))
}


# Stand alone map Elements ------------------------------------------------
### DI communities
getDI(overWrite = TRUE)
### Oil and gas community,
getOilGas()
### Coal power plant community,
getCoal()
### urban/rural
getRural()
### justice40 layer 
getJustice40(filePath = "data/justice40/Screening_Tool_Data/communities-2022-03-21-1359GMT.csv", overWrite = FALSE)


# generate dataset for shiny input ----------------------------------------
generateDataForShiny(removeNativeLand = TRUE,version = version)
