###
# process the haps dataset for nox,sox,etc
# carverd@colostate.edu
# 20211001
###



# subset data from the Other Criteria Pollutants (SOx, NOx, CO, PB, PM10)
# headers sox : site_so2_estim, nox : c(site_no2_estim, site_nox_estim), co : site_co_estim
# pm10 : site_pm10_estim

### testing
###
# filePath <- "data/haps/APENs 8_24_2021.csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
### 



getOtherHAPS <- function(filePath, geometry){
  ### Processes the HAPS dataset establishing a volume weighted score by geometry
  # filePath : relative path to csv of HAPS data
  # geometry : sf object of the census block group, census track, or county
  # return : a dataframe with geoid and haps score 
  ###

  x <- c("sf","dplyr")
  lapply(x, require, character.only = TRUE)
  
  #call in normalize function 
  source("utils/processingFunctions/normalizeVector.R")
  
  # read in dataset and drop locations with no coordinates 
  d1 <- read.csv(filePath)%>%
    dplyr::filter(SITE_X_COORDINATE != 0)
  
  ### Generate 5 year median values for all pollutants 
  # select columns of interest 
  d2 <- d1 %>% 
    dplyr::select(
      APCD_SITE_ID, SITE_SO2_ESTIM, SITE_NO2_ESTIM, 
                  SITE_NOX_ESTIM, SITE_CO_ESTIM, SITE_PM10_ESTIM
    )%>%
    dplyr::group_by(APCD_SITE_ID)%>%
    dplyr::summarise_all(median ,na.rm = TRUE)
  
  ### normalize data based on volume of emission 
  d2[,2:6] <- apply(d2[,2:6], MARGIN = 2, FUN = normalizeVector)
  ### calculate total 
  d2$total <- rowSums(d2[,c(-1)], na.rm = TRUE)
  ### drop all non poluting sites 
  d2 <- d2[d2$total != 0, ]
  
  ### create spatial feature based on sites of interest 
  sp1 <- d1 %>%
    dplyr::filter(APCD_SITE_ID %in% d2$APCD_SITE_ID)%>%
    distinct(APCD_SITE_ID, .keep_all = TRUE)%>%
    dplyr::select("APCD_SITE_ID",
                  "SITE_X_COORDINATE",
                  "SITE_Y_COORDINATE")%>%
    st_as_sf(.,coords=c("SITE_X_COORDINATE","SITE_Y_COORDINATE"),crs=4269)
  
  ### attached GEOID to all points 
  geom <- geometry %>%
    dplyr::select(GEOID)
  sp1 <-  sp1 %>% 
    sf::st_intersection(geom)
  
  ####
  # currently this is just doing a direct intersect, we will want to change the method to match point buffer define in the cal enviroscreen process.
  ###
  
  
  ### attach and summarize haps score 
  temp1 <- sp1 %>%
    dplyr::left_join(d2, by = "APCD_SITE_ID")%>%
    dplyr::group_by(GEOID)%>%
    dplyr::summarize(sum(total))%>%
    dplyr::rename(HAPS = `sum(total)`)%>%
    as.data.frame()%>%
    dplyr::select(GEOID, HAPS_Other = HAPS)
  
  return(temp1)
}
