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
  tmap, # visualize spatial data
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
version <- "2"


# tidycensus key ----------------------------------------------------------
# key <- 
# getCensusAPIKey(key)


for(val1 in c( "county","censusTract","censusBlockGroup")){ 
  processingLevel <- val1
  # call in spatial object at give extent
  geometry <- setSpatialData(dataFolder = "data/", scale = processingLevel)

  ### EJScreen and ACS data contributes to multiple components run it here then split out
  ejscreen <- getEjScreen2021(geometry = geometry ,processingLevel = processingLevel)
  ### add condition to test for the existence of a specific file based on geom
  acsData <- getACS(processingLevel = processingLevel, year = 2019)


# Exposures ---------------------------------------------------------------
  tic()
  envExposures <- enviromentalExposures(geometry = geometry, ejscreen = ejscreen,processingLevel = processingLevel)
  toc()
  # county : 86.62 sec elapsed
  # censusTract : 66.48 sec elapsed
  # censusBlockGroups : 135.71 sec elapsed



# Environmental Effects ---------------------------------------------------
  tic()
  envEffects <- enviromentalEffects(geometry = geometry,
                                    processingLevel = processingLevel,
                                    ejscreen = ejscreen)
  toc()
  # county : 257.29 sec elapsed
  # censusTract : 1.11 sec elapsed
  # censusBlockGroups : 1.17 sec elapsed



# Climate Impacts ---------------------------------------------------------
  tic()
  climateData <- climate(geometry)
  toc()
  # county : 38.95 sec elapsed
  # censusTract :37.2 sec elapsed
  # censusBlockGroups : 66.16 sec elapsed

# Socioeconomic Factors ---------------------------------------------------
  tic()
  senPop <- sensitivePopulations(geometry = geometry, ejscreen = ejscreen)
  toc()
  # county : 7.47 sec elapsed
  # censusTract : 7.78 sec elapsed
  # censusBlockGroups : 8.75 sec elapsed

# Socioeconomic Factors ---------------------------------------------------
  tic()
  socEco <- socioEconomicFactors(geometry,ejscreen, acsData, processingLevel = processingLevel)
  toc()
  # county : 0.66 sec elapsed
  # censusTract : 2.8 sec elapsed
  # censusBlockGroups : 4 sec elapsed



  # merge all datasets on geoid
  # apply function across all features
  dataframes <- list(envExposures,envEffects,climateData,senPop,socEco)
  
  # compile final component scores 
  df <- finalComponentScore(dataframes)

  # write output ------------------------------------------------------------
  write_csv(df,path = paste0("data/envScreenScores/",processingLevel,"_",version,".csv"))
}



# generate dataset for shiny input ----------------------------------------
generateDataForShiny(removeNativeLand = TRUE,version = version)


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

