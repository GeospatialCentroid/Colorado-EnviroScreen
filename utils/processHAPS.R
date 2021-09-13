###
# process the haps dataset
# carverd@colostate.edu
# 20210826
###

# ### testing 
filePath <- "data/haps/APENs 8_24_2021.csv"
geometry <- sf::st_read("F:/geoSpatialCentroid/coEnvrioScreen/data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# ###

processHAPS <- function(filePath, geometry){
  ### Processes the HAPS dataset establishing a volume weighted score by geometry
  # filePath : relative path to csv of HAPS data
  # geometry : sf object of the census block group, census track, or county
  # return : a dataframe with geoid and haps score 
  ###
  require(dplyr, sf)
  ### create function to generate the relative rank of emission from all facilities 
  normalizeVolume <- function(x){
    max <- max(x, na.rm = TRUE)
    return(x / max)
  }
  
  # read in dataset and drop locations with no coordinates 
  d1 <- read.csv(filePath)%>%
    dplyr::filter(SITE_X_COORDINATE != 0)
  
  ### Generate 5 year median values for all pollutants 
  # select columns of interest 
  d2 <- d1 %>% 
    dplyr::select(APCD_SITE_ID                ,SITE_100414_ESTIM
                  ,SITE_106467_ESTIM                ,SITE_106990_ESTIM
                  ,SITE_18540299_ESTIM                ,SITE_50000_ESTIM
                  ,SITE_56235_ESTIM                ,SITE_71432_ESTIM
                  ,SITE_75070_ESTIM                ,SITE_75218_ESTIM
                  ,SITE_7782505_ESTIM                ,SITE_822060_ESTIM
                  ,SITE_91203_ESTIM                ,SITE_ASC_ESTIM
                  ,SITE_CE_ESTIM
                  )%>%
    dplyr::group_by(APCD_SITE_ID)%>%
    dplyr::summarise_all(median ,na.rm = TRUE)
  
  ### normalize data based on volume of emission 
  d2[,2:15] <- apply(d2[,2:15], MARGIN = 2, FUN = normalizeVolume)
  ### calculate total 
  d2$total <- rowSums(d2[,c(-1)], na.rm = TRUE)
  ### drop all non poluting sites 
  d2 <- d2[d2$total != 0, ]
  
  ### temp 20210913
  # generate count of all non na values 
  #d3 <- colSums(x = !is.na(d2))
  #View(d3)
  
  
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
    dplyr::select(GEOID, HAPS)
  
  return(temp1)
}
