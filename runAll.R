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
  feather,
  tidyr, 
  rmapshaper,
  readr
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

# set processing level
### "censusBlockGroup", "censusTract", "county"

for(val1 in c("censusBlockGroup", "censusTract", "county")){
  processingLevel <- val1
  # call in spatial object at give extent
  geometry <- setSpatialData(dataFolder = "data/", scale = processingLevel)

  ### EJScreen and ACS data contributes to multiple components run it here then split out
  ejscreen <- getEJScreen(
    filePath = "data/EJScreen/EJSCREEN_2020_StatePctile.csv",
    geometry = geometry
  )
  ### add condition to test for the existence of a specific file based on geom
  acsData <- getACS(processingLevel = processingLevel, year = 2019)

  ### add condition to test for the existence of a specific file based on geom

  ####
  # Exposures
  ####
  tic()
  envExposures <- enviromentalExposures(geometry = geometry, ejscreen = ejscreen)
  toc()
  # county : 51.28 sec elapsed
  # censusTract : 66.48 sec elapsed
  # censusBlockGroups : 77.48 sec elapsed



  ####
  # Environmental Effects
  ####
  tic()
  envEffects <- enviromentalEffects(geometry = geometry, ejscreen = ejscreen)
  toc()
  # county : 1.58 sec elapsed
  # censusTract : 1.11 sec elapsed
  # censusBlockGroups : 0.42 sec elapsed



  ####
  # Climate Impacts
  ####
  tic()
  climateData <- climate(geometry)
  toc()
  # county : 22.89 sec elapsed
  # censusTract :37.2 sec elapsed
  # censusBlockGroups : 50.84 sec elapsed




  ####
  # Sensitive Populations
  ####
  tic()
  senPop <- sensitivePopulations(geometry = geometry, ejscreen = ejscreen)
  toc()
  # county : 4.61 sec elapsed
  # censusTract : 4.1 sec elapsed
  # censusBlockGroups : 4.19 sec elapsed



  ####
  # Socioeconomic Factors
  ####
  tic()
  socEco <- socioEconomicFactors(geometry,ejscreen, acsData)
  toc()
  # county : 0.84 sec elapsed
  # censusTract : 0.6 sec elapsed
  # censusBlockGroups : 0.52 sec elapsed



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
  feather::write_feather(df,path = paste0("data/envScreenScores/",processingLevel,"_1.feather"))

}



# generate dataset for shiny input ----------------------------------------
generateDataForShiny(removeNativeLand = TRUE)

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

