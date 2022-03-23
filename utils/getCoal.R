


getCoal <- function(){
  # defination for what counties meet conditions  ---------------------------
  coal <- str_to_title(c("MOFFAT","ROUTT","MORGAN","EL PASO","PUEBLO"))
  # add condition to spatial object  ----------------------------------------
  geometry <- setSpatialData(dataFolder = "data/", scale = "county")%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()
  # define rural features  
  geometry$coal = ifelse(geometry$NAME %in% coal, "Yes", "No")
  # select columns of interest 
  geom <- geometry %>%
    dplyr::select(
      "GEOID","NAME","NAMELSAD" ,"LSAD" ,"geometry" ,"coal"  
    )
  
  geom2 <- geom %>% 
    dplyr::filter(coal == "Yes")%>%
    patternLayer(pattern = "vertical",mode = "sfc", density = 4)
  
  # export 
  saveRDS(object = geom, file = "data/coalCommunities/coalCommunities.rda")
  saveRDS(object = geom2, file = "data/coalCommunities/coalVis.rda")
  return(paste0("The Coal community spatial data was writen to data/coalCommunities/coalCommunities.rda"))
}