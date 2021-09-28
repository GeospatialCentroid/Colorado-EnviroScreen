###
# precessFACE dataset from CDPHE
# carverd@colostate.edu
# 20210915
###

library(dplyr)


# #testing
# library(sf)
# library(dplyr)
# library(stringr)
# filePath <- "data/face/county_level_eads.csv"
# filePath2 <- "data/face/FACE_results_population_scenarios1.csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
#
# tic()
# d2 <- getHeartDisease(filePath = filePath, geometry = geometry)
# toc()


getFACE <- function(filePath, filePath2, geometry){
  ###
  #
  #
  #
  ###
  x <- c("sf","dplyr", "stringr")
  lapply(x, require, character.only = TRUE)

  # read in and join datasets
  d1 <- dplyr::merge(
    read.csv(filePath),
    read.csv(filePath2),
    by = "NAME")

  # select features of interest
  d1 <- d1 %>%
    # select features of interest
    dplyr::select(GEOID = ï..GEOID.x,
           NAME,
           High, #population estimate in the high population growth scenario
           Drought_Crops=D_AG_CRO_C2_P3,
           Drought_Raft=D_RE_BOA_C2_P3,
           Drought_Ski=D_RE_SKI_C2_P3,
           Drought_Cattle=D_AG_LIV_C2_P3,
           Flood_Bridge=F_IN_BRI_C2_P3,
           Flood_Building=F_IN_RES_C2_P3,
           Wildfire_Sup=W_EC_SUP_C2_P3,
           Wildfire_Buildings=W_IN_RES_C2_P3) %>%
    # set geoid and generete percapita norms
    dplyr::mutate(GEOID = paste0("0", as.character(GEOID)),
           dplyr::across(where(is.numeric),
                  .fns = list(PerCapita= ~./High),
                  .names = "{col}_{fn}"))%>%
    # select necessay columns
    dplyr::select(
      GEOID,Flood_Bridge_PerCapita, Drought_Crops_PerCapita,
       Drought_Raft_PerCapita, Drought_Ski_PerCapita,
      Drought_Cattle_PerCapita, Flood_Building_PerCapita,
       Wildfire_Sup_PerCapita, Wildfire_Buildings_PerCapita
    )%>%
    # aggregate at three distinct groups -- Might change 20210916
    dplyr::mutate(
      flood = rowMeans(
        dplyr::select(
        .,Flood_Bridge_PerCapita,
        Flood_Building_PerCapita
      ), na.rm = TRUE),
      drought = rowMeans(
        dplyr::select(
        .,Drought_Crops_PerCapita,
        Drought_Raft_PerCapita,
        Drought_Ski_PerCapita,
        Drought_Cattle_PerCapita
      ), na.rm = TRUE),
      wildfire = rowMeans(
        dplyr::select(
        .,Wildfire_Sup_PerCapita,
        Wildfire_Buildings_PerCapita
      ), na.rm = TRUE)
    )
  ### not sure what to capture at this point two options
  # all layers individually
  op1 <- d1 %>%
    dplyr::select(
      GEOID,Flood_Bridge_PerCapita, Drought_Crops_PerCapita,
      Drought_Raft_PerCapita, Drought_Ski_PerCapita,Drought_Cattle_PerCapita    , Flood_Building_PerCapita,
      Wildfire_Sup_PerCapita, Wildfire_Buildings_PerCapita
    )
  # aggregated layers
  op2 <- d1 %>%
    dplyr::select(
      GEOID, flood, drought,wildfire
    )


  # Apply to geom features
  # country level processing
  if(nchar(geometry$GEOID[1])==5){
    return(op1)
  }else{
    geom <- as.data.frame(geometry) %>%
      dplyr::mutate(GEO2 = stringr::str_sub(GEOID, 1,5))%>%
      # dplyr::select(GEO2)%>%
      dplyr::left_join(y = op1, by = c("GEO2" = "GEOID"))%>%
      dplyr::select(names(op1))
    return(geom)
  }
}
