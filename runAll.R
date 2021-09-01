###
# primary script for running all data processing steps 
# carverd@colostate.edu
# 20210816
### 

# load required libraries 
pacman::p_load(tigris, dplyr, sf, tidycensus, tidyr)

# source functions; this is verbose, so temp object is created then removed 
### if we can find a way to pass parameters to the source function within the lapply we can get around this. 
temp <- list.files("utils/", full.names = TRUE) %>% lapply(source)
rm(temp)

### download data 
###  leave all relative paths as teminal "no '/'" this will be account for if a larger path is constructed 
pullGeometryDatasets(fileFolder = "data",
                     pullNewData = FALSE)

### set processing level 
# "censusBlockGroup", "censusTract", "county"
processingLevel <- "county"
# call in spatial object at give extent 
geometry <- setSpatialData(dataFolder = "data/",scale = processingLevel)


### call function that loops over all inputs data and processes the dataset. 

### we might want to break this out by Component (exposures, effects, climate,  population, socioeconomic)

####
# Exposures
####
### create the storage df that combines all features 
exposures <- as.data.frame(geometry) %>% 
  dplyr::select(GEOID)

#! probably want to call the processing of all this set of data as a function
#! to keep this script clean 

### generate some data 
d1 <- processHAPS(filePath = "data/haps/APENs 8_24_2021.csv",
                  geometry = geometry)
# join datasets 
### consider writing this into function as GEOID will be consistent
exposures <- dplyr::left_join(x = exposures, y = d1, by = "GEOID")






####
# Environmental Effects
####
envEffect <- as.data.frame(geometry) %>% 
  dplyr::select(GEOID)
####
# Climate Impacts
####
cliImapcts <-  as.data.frame(geometry) %>% 
  dplyr::select(GEOID)

####
# Sensitive Populations
####
# gather and derive the social demographic elements from 
# use character to describe the spatial scale at which data is gathered.
#  
acsData <- getACS(processingLevel = processingLevel, year = 2019)

####
# Socioeconomic Factors
####

acsData <- getACS(processingLevel = processingLevel,
                  year = 2019)



####
# Pollution and Climate Burden
####
pcb <- dplyr::bind_cols(exposures, envEffect, cliImapcts)

####
# Population Characteristics 
####


####
# Environmental Health Score 
###






       