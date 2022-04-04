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
  leaflet, # mapping features
  leaflet.extras, # search functionality
  tidyr, 
  rmapshaper,
  readr,
  lubridate
)

# source functions; this is verbose, so temp object is created then removed
### if we can find a way to pass parameters to the source function within the
### lapply we can get around this.
source("utils/loadFunctions.R")
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


for(val1 in c( "county","censusTract","censusBlockGroup")){ 
  processingLevel <- val1
  # call in spatial object at give extent
  geometry <- setSpatialData(dataFolder = "data/", scale = processingLevel)

  ### EJScreen and ACS data contributes to multiple components run it here then split out
  ejscreen <- getEjScreen2021(geometry = geometry ,processingLevel = processingLevel)
  ### add condition to test for the existence of a specific file based on geom
  acsData <- getACS(processingLevel = processingLevel, year = 2019)

  ### add condition to test for the existence of a specific file based on geom

  ####
  # Exposures
  ####
  print("exposures")
  tic()
  envExposures <- enviromentalExposures(geometry = geometry, ejscreen = ejscreen,processingLevel = processingLevel)
  toc()
  # county : 86.62 sec elapsed
  # censusTract : 66.48 sec elapsed
  # censusBlockGroups : 135.71 sec elapsed



  ####
  # Environmental Effects
  ####
  print("effects")  ### failing due to memory errors... need to rework the function 
  tic()
  envEffects <- enviromentalEffects(geometry = geometry,
                                    processingLevel = processingLevel,
                                    ejscreen = ejscreen)
  toc()
  # county : 257.29 sec elapsed
  # censusTract : 1.11 sec elapsed
  # censusBlockGroups : 1.17 sec elapsed



  ####
  # Climate Impacts
  ####
  print("climate")
  tic()
  climateData <- climate(geometry)
  toc()
  # county : 38.95 sec elapsed
  # censusTract :37.2 sec elapsed
  # censusBlockGroups : 66.16 sec elapsed




  ####
  # Sensitive Populations
  ####
  
  print("pop")
  tic()
  senPop <- sensitivePopulations(geometry = geometry, ejscreen = ejscreen)
  toc()
  # county : 7.47 sec elapsed
  # censusTract : 7.78 sec elapsed
  # censusBlockGroups : 8.75 sec elapsed



  ####
  # Socioeconomic Factors
  ####
  print("soc")
  tic()
  socEco <- socioEconomicFactors(geometry,ejscreen, acsData, processingLevel = processingLevel)
  toc()
  # county : 0.66 sec elapsed
  # censusTract : 2.8 sec elapsed
  # censusBlockGroups : 4 sec elapsed



  # merge all datasets on geoid
  # apply function across all features
  dataframes <- list(envExposures,envEffects,climateData,senPop,socEco)
  df <- joinDataFrames(componentName = "all", dataframes = dataframes)%>%
    dplyr::select(-component)

  ###
  # generate the component scores
  ###
  df <- df %>%
    rowwise() %>%
    dplyr::mutate(
      pollClimBurden =  sum(envExp, (envEff * 0.5) , (climate *0.5),na.rm=TRUE)/3,
      popCharacteristic = sum(senPop, socEco, na.rm = TRUE)/2,
      finalScore = sum(pollClimBurden, `popCharacteristic`, na.rm = TRUE)
    )
  ### Error ###
  # percent_rank was returning NaN values within the mutate... Pulled out for short fix
  df$envExp_Pctl <- cume_dist(df$envExp)*100
  df$envEff_Pctl <- cume_dist(df$envEff)*100
  df$climate_Pctl <- cume_dist(df$climate)*100
  df$senPop_Pctl <- cume_dist(df$senPop)*100
  df$socEco_Pctl <- cume_dist(df$socEco)*100
  df$pollClimBurden_Pctl <- cume_dist(df$pollClimBurden) *100
  df$popCharacteristic_Pctl <- cume_dist(df$popCharacteristic) *100
  df$finalScore_Pctl <- cume_dist(df$finalScore) *100

  ###
  # write the output if wanted
  ###
  write_csv(df,path = paste0("data/envScreenScores/",processingLevel,"_",version,".csv"))
}



# generate dataset for shiny input ----------------------------------------
generateDataForShiny(removeNativeLand = TRUE,version = version)

###
# Stand alone map Elements
###

### DI communities

getDI(overWrite = TRUE)
### Oil and gas community,
getOilGas()
### Coal power plant community,
getCoal()
### urban/rural
getRural()
### justice40 layer 
getJustice40()

