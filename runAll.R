###
# primary script for running all data processing steps 
# carverd@colostate.edu
# 20210816
### 

# load required libraries 
pacman::p_load(tigris, dplyr, sf)

# set base directory 
baseDir <- "F:/geoSpatialCentroid/coEnvrioScreen"
  

#source function 
### this prints the function elements, which I don't want. so creating a temp var then removing it. Maybe there is a verbose 
### setting to adjust within the source function? 
temp1 <- list.files(path = paste0(baseDir,"/src/Colorado-EnviroScreen/dataProcessing/functions/"), full.names = TRUE,
                        pattern = "*.R") %>%
  lapply(source)
rm(temp1)


### download data 
pullGeometryDatasets(fileFolder = paste0(baseDir,"/data"),
                     pullNewData = FALSE)

### set processing level 
# "censusBlockGroup", "censusTract", "county"
processingLevel <- "county"
# call in spatial object at give extent 
spatialData <- setSpatialData(dataFolder = paste0(baseDir,"/data"),scale = processingLevel)


### call function that loops over all inputs data and processes the dataset. 
### much of the checks and overwrite parameters will be the same for all features. 
### use pullGeometryDatasets as a reference.

       