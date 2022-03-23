###
# compile multiple geography levels into single file for upload to the shiny app
# 20211129
# carverd@colostate.edu
###

generateDataForShiny <- function(removeNativeLand){
  ###
  # takes outputs from the enviroscreen scoring process and combines and renames
  # for use in the shiny app
  ###

  # read in spatial data
  county <- sf::st_read("data/county/coloradoCounties.geojson")%>%
    dplyr::mutate(area = "County",
                name = paste0(NAME, " County"))%>%
    dplyr::select(GEOID, "cNAME" = NAME,name, area)%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()
  
  
  # grap county name to attached to each lower geometry 
  countyName <- county %>% st_drop_geometry() %>% dplyr::select(GEOID, cNAME)
  county <- dplyr::select(county, -"cNAME")
  # census tract 
  censusTract <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")%>%
    dplyr::mutate(area = "Census Tract", geoid2 =stringr::str_sub(GEOID,start = 1, end = 5))%>%
    dplyr::left_join( y = countyName, by = c("geoid2" = "GEOID"))%>%
    dplyr::mutate(name = paste0(cNAME, " County"))%>%
    dplyr::select(GEOID, name, area)%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()

  
    # census block group 
  censusBlockGroup <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")%>%
    dplyr::mutate(area = "Census Block Group",geoid2 =stringr::str_sub(GEOID,start = 1, end = 5))%>%
    dplyr::left_join( y = countyName, by = c("geoid2" = "GEOID"))%>%
    dplyr::mutate(name = paste0( cNAME, " County"))%>%
    dplyr::select(GEOID, name, area)%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()
  
  ### compile names based on the county relationship 
  
  # read in feather files
  c_data <- feather::read_feather("data/envScreenScores/county_1.feather")%>%
    dplyr::left_join(county, by = "GEOID")
  ct_data <- feather::read_feather("data/envScreenScores/censusTract_1.feather")%>%
    dplyr::left_join(censusTract, by = "GEOID")
  cbg_data <- feather::read_feather("data/envScreenScores/censusBlockGroup_1.feather")%>%
    dplyr::left_join(censusBlockGroup, by = "GEOID")

  # join all features
  df <- dplyr::bind_rows(c_data, ct_data, cbg_data)
  if(removeNativeLand == TRUE){
    censusTractsNative <- c("08083941100","08067940400", "08067940300", "08007940400")
    features <- c()
    for(i in seq_along(censusTractsNative)){
      features <- c(features, grep(pattern =  censusTractsNative[i], x = df$GEOID))
    }
   df <- df[-features, ]
  }
  
  
  # select to define order and rename features
  df <- df %>%
    dplyr::select(
      "GEOID"
      ,"Colorado EnviroScreen Score"=  "finalScore"
      ,"Colorado EnviroScreen Score Percentile"="finalScore_Pctl"
      ,"Pollution & Climate Burden"="pollClimBurden"
      ,"Pollution & Climate Burden Percentile"= "pollClimBurden_Pctl"
      ,"Socioeconomics & Demographics"="popCharacteristic"
      ,"Socioeconomics & Demographics Percentile"="popCharacteristic_Pctl"
      ,"Environmental exposures"="envExp"
      ,"Environmental effects"="envEff"
      ,"Climate vulnerability"="climate"
      ,"Sensitive population"="senPop"
      ,"Demographics"="socEco"
      ,"Environmental exposures Percentile"="envExp_Pctl"
      ,"Environmental effects Percentile"="envEff_Pctl"
      ,"Climate vulnerability Percentile"="climate_Pctl"
      ,"Sensitive population Percentile"="senPop_Pctl"
      ,"Demographics Percentile"="socEco_Pctl"
      ,"Ozone"="ozone"
      ,"Ozone Percentile"="ozone_pcntl"
      ,"Particles"="pm25"
      ,"Particles Percentile"="pm25_pcntl"
      ,"Lead exposure risk"="leadPaint"
      ,"Lead exposure risk Percentile"="leadPaint_pcntl"
      ,"Diesel PM"="deiselPM"
      ,"Diesel PM Percentile"="deiselPM_pcntl"
      ,"Traffic proximity & volume"="trafficeProx"
      ,"Traffic proximity & volume Percentile"="trafficeProx_pcntl"
      ,"Air toxics emissions"="HAPS"
      ,"Air toxics emissions Percentile"="HAPS_pcntl"
      ,"Wastewater discharge indicator"="waterDischarge"
      ,"Wastewater discharge indicator Percentile"="waterDischarge_pcntl"
      ,"Proximity to National Priorities List (NPL) sites"="nplProx"
      ,"Proximity to National Priorities List (NPL) sites Percentile"="nplProx_pcntl"
      ,"Proximity to RMP sites"="rmpProx"
      ,"Proximity to RMP sites Percentile"="rmpProx_pcntl"
      ,"Proximity to hazardous waste facilities" ="tsdfProx"
      ,"Proximity to hazardous waste facilities Percentile"="tsdfProx_pcntl"
      ,"Wildfire risk"="wildfire"
      ,"Wildfire risk Percentile"="wildfire_pcntl"
      ,"Floodplains"="floodplainPercent"
      ,"Floodplains Percentile"="floodplainPercent_pcntl"
      ,"Population under 5"="under5"
      ,"Population under 5 Percentile"="under5_pcntl"
      ,"Population over 64"=  "over64"
      ,"Population over 64 Percentile"= "over64_pcntl"
      ,"Heart disease in adults"="heartDisease"
      ,"Heart disease in adults Percentile"="heartDisease_pcntl"
      ,"Asthma hospitalization rate"="asthma"
      ,"Asthma hospitalization rate Percentile"="asthma_pcntl"
      ,"Life expectancy"="lifeExpectancy"
      ,"Life expectancy Percentile"="lifeExpectancy_pcntl"
      ,"Low weight birth rate"="lowBirthWeight"
      ,"Low weight birth rate Percentile"="lowBirthWeight_pcntl"
      ,"Percent people of color"="peopleOfColor"
      ,"Percent people of color Percentile" = "peopleOfColor_pcntl"
      ,"Percent less than high school education"="highSchool"
      ,"Percent less than high school education Percentile"= "highSchool_pcntl"
      ,"Percent low income"="percent_lowincome"
      ,"Percent low income Percentile"="percent_lowincome_pcntl"
      ,"Percent linguistic isolation"= "percent_lingiso"
      ,"Percent linguistic isolation Percentile"="percent_lingiso_pcntl"
      ,"Percent disability"="percent_disability"
      ,"Percent disability Percentile"="percent_disability_pcntl"
      ,"County Name" = "name"
      ,"area"
      ,"geometry"
    )

  # convert to an sf object
  df <- df %>%
    mutate(across(where(is.numeric), round, digits=1))%>%
    sf::st_as_sf()

  # add label for Coal, oil/gas, rural  -------------------------------------
  coal <- readRDS("data/coalCommunities/coalCommunities.rda")%>%
    dplyr::select("GEOID","coal")%>%
    st_drop_geometry()
  og <- readRDS("data/oilgasCommunities/oilgasCommunities.rda")%>%
    dplyr::select("GEOID","oilGas")%>%
    st_drop_geometry()
  rural <- readRDS("data/ruralCommunities/ruralCommunities.rda")%>%
    dplyr::select("GEOID","rural")%>%
    st_drop_geometry()
  
  df$GEOID2 <- str_sub(df$GEOID, 1,5)
  df <- dplyr::left_join(x = df ,y = coal, by = c("GEOID2" = "GEOID"))%>%
    dplyr::left_join(y = og, by = c("GEOID2" = "GEOID"))%>%
    dplyr::left_join(y = rural, by = c("GEOID2" = "GEOID"))%>%
    dplyr::select(-"GEOID2")
  
  # rdata delete_dsn 
  saveRDS(df, file = "data/envScreenScores/allScores.rda")
}

# export feature ----------------------------------------------------------

