###
# get epa EJScreen data 
# carverd@colostate.edu
# 20210921  
### 


### 

#testing
# library(sf)
# library(dplyr)
# library(stringr)
# library(vroom)
# filePath <- "data/EJScreen/EJSCREEN_2020_StatePctile.csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# 
# tic()
# d2 <- getEJScreen(filePath = filePath, geometry = geometry)
# toc()

getEJScreen <- function(filePath, geometry, overWrite=FALSE){
  ###
  # pulls multiple variables from the EJScreen data source 
  # filePath : relative path to file location 
  # geometry : sf object representing census(tract/block group) or county
  ###
  
  x <- c("sf","dplyr", "stringr", "vroom")
  lapply(x, require, character.only = TRUE)
  ###gather 
  # MINORPCT	% people of color  -redundant
  # LOWINCPCT	% low-income  -redundant
  # LESSHSPCT	% less than high school -redundant
  # LINGISOPCT	% of households (interpreted as individuals) in linguistic isolation  -redundant
  # UNDER5PCT	% under age 5  - -redundant
  # OVER64PCT	% over age 64 - -redundant
  ### environmental Exposures 
  # PRE1960PCT	% pre-1960 housing (lead paint indicator)
  # DSLPM	Diesel particulate matter level in air
  # PTRAF	Traffic proximity and volume
  # PWDIS	Indicator for major direct dischargers to water
  # PNPL	Proximity to National Priorities List (NPL) sites
  # PRMP	Proximity to Risk Management Plan (RMP) facilities
  # PTSDF	Proximity to Treatment Storage and Disposal (TSDF) facilities
  #
  pathToData <- paste0("data/EJScreen/ejscreen_",processingLevel,".csv")
  if(file.exists(pathToData) & overWrite == FALSE){
    geom <- vroom::vroom(pathToData)
    return(geom)
  }else{
    d1 <- vroom::vroom("data/EJScreen/EJSCREEN_2020_StatePctile.csv")%>%
      dplyr::filter(STATE_NAME == "Colorado") %>%
      dplyr::select(
        GEOID = ID,
        peopleOfColor = MINORPCT,
        lowIncome = LOWINCPCT,
        highSchool = LESSHSPCT,
        linguisticIsolation = LINGISOPCT,
        under5 = UNDER5PCT,
        over64 = OVER64PCT,
        leadPaint = PRE1960PCT,
        deiselPM = DSLPM, 
        trafficeProx = PTRAF,
        waterDischarge = PWDIS,
        nplProx = PNPL,
        rmpProx = PRMP, 
        tsdfProx = PTSDF
      )
    
    # process based on geometry level 
    if(nchar(geometry$GEOID[1])==5){
      i <- 5
    }
    if(nchar(geometry$GEOID[1])==11){
      i <- 11
    }
    if(nchar(geometry$GEOID[1])==12){
      i <- 12
    }
    # processing based on the length of GEOID object
    geom <-  d1 %>%
      dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = i)) %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarise(
        peopleOfColor = mean(peopleOfColor,na.rm=TRUE),
        lowIncome = mean(lowIncome,na.rm=TRUE),
        highSchool = mean(highSchool,na.rm=TRUE),
        linguisticIsolation = mean(linguisticIsolation,na.rm=TRUE),
        under5 = mean(under5,na.rm=TRUE),
        over64 = mean(over64,na.rm=TRUE),
        leadPaint = mean(leadPaint,na.rm=TRUE),
        deiselPM = mean(deiselPM,na.rm=TRUE), 
        trafficeProx = mean(trafficeProx,na.rm=TRUE),
        waterDischarge = mean(waterDischarge,na.rm=TRUE),
        nplProx = mean(nplProx,na.rm=TRUE),
        rmpProx = mean(rmpProx,na.rm=TRUE), 
        tsdfProx = mean(tsdfProx,na.rm=TRUE)
      )
    write.csv(x = geom, file = pathToData)
    return(geom)
  }
}
  
  