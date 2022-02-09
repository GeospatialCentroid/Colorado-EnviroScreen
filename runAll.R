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
  tidyr
)

# source functions; this is verbose, so temp object is created then removed 
### if we can find a way to pass parameters to the source function within the 
### lapply we can get around this. 
functions <- list.files("utils/", full.names = TRUE, recursive = TRUE)
for (i in seq_along(functions)) {
  print(i)
  source(file = functions[i], echo = FALSE)
}
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
  ### Ozone,
  ### PM2.5, 
  ### Pre-1960 housing (lead risk), -ejscreen
  ### Traffic proximity and volume (DOT 2017),-ejscreen
  ### Diesel PM, -ejscreen
  ### Air Toxics Emissions (Hazardous Air Pollutants or "HAPs"),
  ### Other Criteria Pollutants (SOx, NOx, CO, PB, PM10),
  ### Proximity to oil and gas facilities,
  ### Drinking Water quality Public water system sample results,
  ### Impaired surface waters,
  ### Solar radiation,
  ### Radon,
  ### Other radiation exposure sources,
  ### Groundwater vulnerability, Other remediation sites (non-Superfund),
  ### Tree cover (alternate indicators: vegetation?, heat surface data?),
  ### PFAS risk,
  ### Permit violations Air and water violations
  
  ### compile all available data 
  tic()
  envExposures <- enviromentalExposures(geometry = geometry, ejscreen = ejscreen)
  toc()
  # county : 51.28 sec elapsed
  # censusTract : 66.48 sec elapsed
  # censusBlockGroups : 77.48 sec elapsed
  
  
  
  ####
  # Environmental Effects
  ####
  ### includes 
  ### Proximity to National Priorities List (NPL) sites, 
  ### Proximity to Risk Management Plan (RMP) sites
  ### Wastewater Discharge Indicator (Stream Proximity and Toxic Concentration),
  ### Proximity to Hazardous Waste Facilities,
  ### Mining and smelter locations (historical, current),
  ### Solid Waste facilities, Underground Injection Control (UIC) wells, 
  ### Underground Storage Tanks & Leaking Underground Storage Tanks
  
  tic()
  envEffects <- enviromentalEffects(geometry = geometry, ejscreen = ejscreen)
  toc()
  # county : 1.58 sec elapsed
  # censusTract : 1.11 sec elapsed
  # censusBlockGroups : 0.42 sec elapsed
  
  
  
  ####
  # Climate Impacts
  ####
  
  ### includes 
  ### Wildfire risk, Flood plains, Projected heat days, Percent impervious surface,
  ### Projected precipitation
  tic()
  climateData <- climate(geometry)
  toc()
  # county : 22.89 sec elapsed
  # censusTract :37.2 sec elapsed
  # censusBlockGroups : 50.84 sec elapsed
  
  
  
  
  ####
  # Sensitive Populations
  ####
  
  ### includes
  ### Asthma hospitalization rate (CDPHE/CHA, 2013-2017),
  ### Heart disease in adults (CDPHE/BRFSS 2014-2017),
  ### Low weight birth rate (CDPHE - Vital Statistics,
  ### 2013-2017, Housing Cost-Burdened Communities, 
  ### life expectancy,
  ### pregnancy, 
  
  tic()
  senPop <- sensitivePopulations(geometry = geometry, ejscreen = ejscreen)
  toc()
  # county : 4.61 sec elapsed
  # censusTract : 4.1 sec elapsed
  # censusBlockGroups : 4.19 sec elapsed
  
  
  
  ####
  # Socioeconomic Factors
  ####
  
  ### includes 
  ### Percent people of color,
  ### Percent low income,
  ### Percent linguistic isolation, 
  ### Percent less than high school education,
  ### Percent disability,
  
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
      pollClimBurden =  sum(envExp, envEff, na.rm = TRUE)/2 + (climate/2),
      popCharacteristic = sum(senPop, socEco, na.rm = TRUE)/2,
      finalScore = sum(pollClimBurden, `popCharacteristic`, na.rm = TRUE)
    )
  ### Error ###
  # percent_rank was returning NaN values within the mutate... Pulled out for short fix 
  df$envExp_Pctl <- percent_rank(df$envExp)*100
  df$envEff_Pctl <- percent_rank(df$envEff)*100
  df$climate_Pctl <- percent_rank(df$climate)*100
  df$senPop_Pctl <- percent_rank(df$senPop)*100
  df$socEco_Pctl <- percent_rank(df$socEco)*100
  df$pollClimBurden_Pctl <- percent_rank(df$pollClimBurden) *100
  df$popCharacteristic_Pctl <- percent_rank(df$popCharacteristic) *100
  df$finalScore_Pctl <- percent_rank(df$finalScore) *100
  
  ###
  # write the output if wanted 
  ###
  feather::write_feather(df,path = paste0("data/envScreenScores/",processingLevel,"_1.feather"))
  
}





# # combine all output datasets 
# list1 <- list()
# proLevel <- c("county", "censusTract", "censusBlockGroup")
# for(i in seq_along(proLevel)){
#   temp1 <- feather::read_feather(paste0("data/envScreenScores/",proLevel[i],".feather"))
#   list1[[i]] <- temp1
# }
# 



###
# compile the three features into single object and join with spatial data
# output sfarrow, geojson, and rdata 
###
generateDataForShiny()







###
# Stand alone map Elements 
### 

### includes 
### DI communities 
getDI(overWrite = TRUE)
### Oil and gas community,
### Coal power plant community,

### urban/rural 
g1 <- getTotalPop(geometry)





       