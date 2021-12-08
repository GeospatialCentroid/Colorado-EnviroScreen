###
# compile multiple geography levels into single file for upload to the shiny app
# 20211129
# carverd@colostate.edu 
###
library(dplyr)
library(sf)
library(purrr)


generateDataForShiny <- function(){
  ###
  # takes outputs from the enviroscreen scoring process and combines and renames 
  # for use in the shiny app
  ### 
  
  # read in spatial data  
  county <- sf::st_read("data/county/coloradoCounties.geojson")%>%
    dplyr::mutate(area = "County")%>%
    dplyr::select(GEOID, area)
  censusTract <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")%>%
    dplyr::mutate(area = "Census Tract")%>%
    dplyr::select(GEOID, area)
  censusBlockGroup <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")%>%
    dplyr::mutate(area = "Census Block Group")%>%
    dplyr::select(GEOID, area)
  # read in feather files
  c_data <- feather::read_feather("data/envScreenScores/county.feather")%>%
    dplyr::left_join(county, by = "GEOID")
  ct_data <- feather::read_feather("data/envScreenScores/censusTract.feather")%>%
    dplyr::left_join(censusTract, by = "GEOID")
  cbg_data <- feather::read_feather("data/envScreenScores/censusBlockGroup.feather")%>%
    dplyr::left_join(censusBlockGroup, by = "GEOID")
  
  # join all features 
  df <- dplyr::bind_rows(c_data, ct_data, cbg_data)
  
  # select to define order and rename features 
  df <- df %>% 
    dplyr::select(
      "GEOID"
      ,"Ozone"="ozone"
      ,"Ozone_pcntl"="ozone_pcntl"
      ,"Particulate Matter 2.5"="pm25"
      ,"Particulate Matter 2.5_pcntl"="pm25_pcntl"
      ,"Lead Paint in Homes"="leadPaint"
      ,"Lead Paint in Homes_pcntl"="leadPaint_pcntl"
      ,"Diesel Particulate Matter"="deiselPM"
      ,"Diesel Particulate Matter_pcntl"="deiselPM_pcntl"
      ,"Traffic Density"="trafficeProx"
      ,"Traffic Density_pcntl"="trafficeProx_pcntl" 
      ,"Hazardous Air Emission"="HAPS"
      ,"Hazardous Air Emission_pcntl"="HAPS_pcntl"
      ,"Environmental Exposures"="envExp"
      ,"Waste Water Discharge"="waterDischarge"
      ,"Waste Water Discharge_pcntl"="waterDischarge_pcntl"
      ,"Proximity to Superfund Sites"="nplProx"
      ,"Proximity to Superfund Sites_pcntl"="nplProx_pcntl"
      ,"Proximity to Risk Management Plan Sites"="rmpProx"
      ,"Proximity to Risk Management Plan Sites_pcntl"="rmpProx_pcntl" 
      ,"Proximity to Treatment, Storage and Disposal Facilities" ="tsdfProx"          
      ,"Proximity to Treatment, Storage and Disposal Facilities_pcntl"="tsdfProx_pcntl"
      ,"Environmental Effects"="envEff"
      ,"Wildfire Risk"="wildfire"
      ,"Wildfire Risk_pcntl"="wildfire_pcntl"
      ,"Flood Plain Area"="floodplainPercent"
      ,"Flood Plain Area_pcntl"="floodplainPercent_pcntl"
      ,"Climate"="climate"
      ,"Population Under Five"="under5"
      ,"Population Under Five_pcntl"="under5_pcntl"
      ,"Population Over Sixity Four"=  "over64" 
      ,"Population Over Sixity Four_pcntl"= "over64_pcntl"            
      ,"Heart Disease"="heartDisease"
      ,"Heart Disease_pcntl"="heartDisease_pcntl"
      ,"Asthma"="asthma"
      ,"Asthma_pcntl"="asthma_pcntl"
      ,"Life Expectancy"="lifeExpectancy" 
      ,"Life Expectancy_pcntl"="lifeExpectancy_pcntl"
      ,"Low Birth Weight"="lowBirthWeight"
      ,"Low Birth Weight_pcntl"="lowBirthWeight_pcntl"
      ,"Sensitive Populations"="senPop"
      ,"People of Color"="peopleOfColor" 
      ,"People of Color_pcntl" = "peopleOfColor_pcntl"
      ,"Educational Attainment"="highSchool"
      ,"Educational Attainment_pcntl"=       "highSchool_pcntl"   
      ,"Low Income"="percent_lowincome" 
      ,"Low Income_pcntl"="percent_lowincome_pcntl" 
      ,"Linguistic Isolation"= "percent_lingiso"
      ,"Linguistic Isolation_pcntl"="percent_lingiso_pcntl"   
      ,"Disability"="percent_disability" 
      ,"Disability_pcntl"="percent_disability_pcntl"
      ,"Socioeconomic"="socEco" 
      ,"Climate Burden"="pollClimBurden"
      ,"Climate Burden_pcntl"= "pollClimBurden_Pctl" 
      ,"Pollution Burden"="popCharacteristic"  
      ,"Pollution Burden_pcntl"="popCharacteristic_Pctl" 
      ,"Colorado Enviroscreen Score"      =  "finalScore"      
      ,"Colorado Enviroscreen Score_pcntl"="finalScore_Pctl"  
      ,"area"
      ,"geometry"  
    )
  
  # convert to an sf object 
  df <- df %>%
    sf::st_as_sf()
  # export as a single document 
  sfarrow::st_write_feather(df, dsn = "data/envScreenScores/allScores.feather")
    }

