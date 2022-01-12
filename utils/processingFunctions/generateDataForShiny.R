###
# compile multiple geography levels into single file for upload to the shiny app
# 20211129
# carverd@colostate.edu
###

generatgeneratgenerateDataForShiny <- function(){
  ###
  # takes outputs from the enviroscreen scoring process and combines and renames
  # for use in the shiny app
  ###

  # read in spatial data
  county <- sf::st_read("data/county/coloradoCounties.geojson")%>%
    dplyr::mutate(area = "County",
                name = paste0(NAME, " County"))%>%
    dplyr::select(GEOID, "cNAME" = NAME,name, area)
  # grap county name to attached to each lower geometry 
  countyName <- county %>% st_drop_geometry() %>% dplyr::select(GEOID, cNAME)
  county <- dplyr::select(county, -"cNAME")
  # census tract 
  censusTract <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")%>%
    dplyr::mutate(area = "Census Tract", geoid2 =stringr::str_sub(GEOID,start = 1, end = 5))%>%
    dplyr::left_join( y = countyName, by = c("geoid2" = "GEOID"))%>%
    dplyr::mutate(name = paste0("Census Tract: ",GEOID," in ", cNAME, " County"))%>%
    dplyr::select(GEOID, name, area)
    # census block group 
  censusBlockGroup <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")%>%
    dplyr::mutate(area = "Census Block Group",geoid2 =stringr::str_sub(GEOID,start = 1, end = 5))%>%
    dplyr::left_join( y = countyName, by = c("geoid2" = "GEOID"))%>%
    dplyr::mutate(name = paste0("Census Block Group: ",GEOID," in ", cNAME, " County"))%>%
    dplyr::select(GEOID, name, area)
  ### compile names based on the county relationship 
  
  
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
      ,"Colorado Enviroscreen Score"      =  "finalScore"
      ,"Colorado Enviroscreen Score_pcntl"="finalScore_Pctl"
      ,"Pollution and Climate Burden"="pollClimBurden"
      ,"Pollution and Climate Burden_pcntl"= "pollClimBurden_Pctl"
      ,"Population Burden"="popCharacteristic"
      ,"Population Burden_pcntl"="popCharacteristic_Pctl"
      ,"Environmental Exposures"="envExp"
      ,"Environmental Effects"="envEff"
      ,"Climate"="climate"
      ,"Sensitive Populations"="senPop"
      ,"Socioeconomic"="socEco"
      ,"Environmental Exposures_pcntl"="envExp_Pctl"
      ,"Environmental Effects_pcntl"="envEff_Pctl"
      ,"Climate_pcntl"="climate_Pctl"
      ,"Sensitive Populations_pcntl"="senPop_Pctl"
      ,"Socioeconomic_pcntl"="socEco_Pctl"
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
      ,"Waste Water Discharge"="waterDischarge"
      ,"Waste Water Discharge_pcntl"="waterDischarge_pcntl"
      ,"Proximity to Superfund Sites"="nplProx"
      ,"Proximity to Superfund Sites_pcntl"="nplProx_pcntl"
      ,"Proximity to Risk Management Plan Sites"="rmpProx"
      ,"Proximity to Risk Management Plan Sites_pcntl"="rmpProx_pcntl"
      ,"Proximity to Treatment, Storage and Disposal Facilities" ="tsdfProx"
      ,"Proximity to Treatment, Storage and Disposal Facilities_pcntl"="tsdfProx_pcntl"
      ,"Wildfire Risk"="wildfire"
      ,"Wildfire Risk_pcntl"="wildfire_pcntl"
      ,"Flood Plain Area"="floodplainPercent"
      ,"Flood Plain Area_pcntl"="floodplainPercent_pcntl"
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
      ,"name"
      ,"area"
      ,"geometry"
    )

  # convert to an sf object
  df <- df %>%
    mutate(across(is.numeric, round, digits=1))%>%
    sf::st_as_sf()
  # round all values 
  
  
  # export as a single document
  ## sfarrow
  # sfarrow::st_write_feather(df, dsn = "data/envScreenScores/allScores.feather")
  # # sf object
  # sf::st_write(obj = df, dsn = "data/envScreenScores/allScores.geojson", delete_dsn  = TRUE)
  # rdata delete_dsn 
  saveRDS(df, file = "data/envScreenScores/allScores.rda")
}
