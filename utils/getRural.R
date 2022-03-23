


getRural <- function(){
  # defination for what counties meet conditions  ---------------------------
  urban <- str_to_title(c("ADAMS","ARAPAHOE","BOULDER","BROOMFIELD","DENVER","DOUGLAS",
                          "EL PASO","JEFFERSON","LARIMER","MESA","PUEBLO","WELD"))
  # add condition to spatial object  ----------------------------------------
  geometry <- setSpatialData(dataFolder = "data/", scale = "county")%>%
    st_transform(crs = st_crs(4326))%>%
    rmapshaper::ms_simplify()
  # define rural features  
  geometry$rural = ifelse(geometry$NAME %in% urban, "No", "Yes")
  # select columns of interest 
  geom <- geometry %>%
    dplyr::select(
      "GEOID","NAME","NAMELSAD" ,"LSAD" ,"geometry" ,"rural"  
    )
  
  geom2 <- geom %>%
    dplyr::filter( rural == "Yes")%>%
    patternLayer(pattern = "left2right",mode = "sfc", density = 4)
  
  print(geom2)
  # export 
  saveRDS(object = geom, file = "data/ruralCommunities/ruralCommunities.rda")
  saveRDS(object = geom2, file = "data/ruralCommunities/ruralVis.rda")
  return(paste0("The rural community spatial data was writen to data/ruralCommunities/ruralCommunities.rda"))
}