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
    dplyr::mutate(name = paste0(cNAME, " County"))%>%
    dplyr::select(GEOID, name, area)
    # census block group 
  censusBlockGroup <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")%>%
    dplyr::mutate(area = "Census Block Group",geoid2 =stringr::str_sub(GEOID,start = 1, end = 5))%>%
    dplyr::left_join( y = countyName, by = c("geoid2" = "GEOID"))%>%
    dplyr::mutate(name = paste0( cNAME, " County"))%>%
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
      ,"Socioeconomics & Demographics"="popCharacteristic"
      ,"Socioeconomics & Demographics_pcntl"="popCharacteristic_Pctl"
      ,"Environmental exposures"="envExp"
      ,"Environmental effects"="envEff"
      ,"Climate Vulnerability"="climate"
      ,"Sensitive population"="senPop"
      ,"Demographics"="socEco"
      ,"Environmental Exposures_pcntl"="envExp_Pctl"
      ,"Environmental Effects_pcntl"="envEff_Pctl"
      ,"Climate_pcntl"="climate_Pctl"
      ,"Sensitive Populations_pcntl"="senPop_Pctl"
      ,"Socioeconomic_pcntl"="socEco_Pctl"
      ,"Ozone"="ozone"
      ,"Ozone_pcntl"="ozone_pcntl"
      ,"Particles"="pm25"
      ,"Particles_pcntl"="pm25_pcntl"
      ,"Lead exposure risk"="leadPaint"
      ,"Lead exposure risk_pcntl"="leadPaint_pcntl"
      ,"Diesel PM"="deiselPM"
      ,"Diesel PM_pcntl"="deiselPM_pcntl"
      ,"Traffic proximity and volume"="trafficeProx"
      ,"Traffic proximity and volume_pcntl"="trafficeProx_pcntl"
      ,"Air Toxics Emissions"="HAPS"
      ,"Air Toxics Emissions_pcntl"="HAPS_pcntl"
      ,"Wastewater Discharge Indicator"="waterDischarge"
      ,"Wastewater Discharge Indicator_pcntl"="waterDischarge_pcntl"
      ,"Proximity to National Priorities List (NPL) sites"="nplProx"
      ,"Proximity to National Priorities List (NPL) sites_pcntl"="nplProx_pcntl"
      ,"Proximity to RMP Sites"="rmpProx"
      ,"Proximity to RMP Sites_pcntl"="rmpProx_pcntl"
      ,"Proximity to Hazardous Waste Facilities" ="tsdfProx"
      ,"Proximity to Hazardous Waste Facilities_pcntl"="tsdfProx_pcntl"
      ,"Wildfire Risk"="wildfire"
      ,"Wildfire Risk_pcntl"="wildfire_pcntl"
      ,"Flood Plains"="floodplainPercent"
      ,"Flood Plains_pcntl"="floodplainPercent_pcntl"
      ,"Population under 5"="under5"
      ,"Population under 5_pcntl"="under5_pcntl"
      ,"Population over 64"=  "over64"
      ,"Population over 64_pcntl"= "over64_pcntl"
      ,"Heart disease in adults"="heartDisease"
      ,"Heart disease in adults_pcntl"="heartDisease_pcntl"
      ,"Asthma hospitalization rate"="asthma"
      ,"Asthma hospitalization rate_pcntl"="asthma_pcntl"
      ,"Life Expectancy"="lifeExpectancy"
      ,"Life Expectancy_pcntl"="lifeExpectancy_pcntl"
      ,"Low weight birth rate"="lowBirthWeight"
      ,"Low weight birth rate_pcntl"="lowBirthWeight_pcntl"
      ,"Percent people of color"="peopleOfColor"
      ,"Percent people of color_pcntl" = "peopleOfColor_pcntl"
      ,"Percent less than high school education"="highSchool"
      ,"Percent less than high school education_pcntl"=       "highSchool_pcntl"
      ,"Percent low income"="percent_lowincome"
      ,"Percent low income_pcntl"="percent_lowincome_pcntl"
      ,"Percent linguistic isolation"= "percent_lingiso"
      ,"Percent linguistic isolation_pcntl"="percent_lingiso_pcntl"
      ,"Percent disability"="percent_disability"
      ,"Percent disability_pcntl"="percent_disability_pcntl"
      ,"County Name" = "name"
      ,"area"
      ,"geometry"
    )

  # convert to an sf object
  df <- df %>%
    mutate(across(where(is.numeric), round, digits=1))%>%
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
