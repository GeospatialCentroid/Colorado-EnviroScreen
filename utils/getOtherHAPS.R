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
 filePath <- "data/haps/APENs 8_24_2021.csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
 geometry <- sf::st_read("data/county/coloradoCounties.geojson")
### 



getOtherHAPS <- function(filePath, geometry, processingLevel,overWrite=FALSE){
  ### Processes the HAPS dataset establishing a volume weighted score by geometry
  # filePath : relative path to csv of HAPS data
  # geometry : sf object of the census block group, census track, or county
  # return : a dataframe with geoid and haps score 
  ###
  
  file <- paste0("data/haps/",processingLevel,"_otherHAPS.csv")
  if(file.exists(file) & isFALSE(overWrite)){
    geom <- read_csv(file)
  }else{
  
  # read in dataset and drop locations with no coordinates 
  d1 <- read_csv(filePath)%>%
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
  
  # Intersection and buffer process -----------------------------------------
  geom2 <- st_transform(geometry, crs = st_crs(5070))%>% select(GEOID)
  d5 <- st_transform(sp1, crs = st_crs(5070))
  # running buffering process. 
  b1 <- bufferObjects(bufferFeature = d5, 
                      geometry = geom2,
                      dist = seq(250, 1000, by = 250),
                      weight = c(1, 0.5, 0.2, 0.1)
  )
  # select the top score value only, combine with site score, and summarize
  geom <- b1 %>%
    st_drop_geometry()%>%
    arrange(desc(weight)) %>% # ensures highest score is kept
    dplyr::distinct(GEOID, APCD_SITE_ID , .keep_all = TRUE)%>%
    group_by(GEOID) %>% 
    summarise(bufferFeature_score = sum(weight, na.rm = TRUE)) %>%
    dplyr::select(GEOID, otherHAPS = bufferFeature_score) 
  
  geom <- left_join(st_drop_geometry(geom2), geom, by = "GEOID")%>%
    dplyr::mutate(
      otherHAPS = case_when(
        is.na(otherHAPS) ~ 0,
        TRUE ~ otherHAPS
      )
    )
  
  write_csv(geom, file = file )
  }
  return(geom)
}
