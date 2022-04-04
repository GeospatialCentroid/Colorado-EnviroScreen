

getMines <- function(geometry, processingLevel, overWrite = FALSE){
  file <- paste0("data/mines/",processingLevel,"_mines.csv")
  if(file.exists(file) & isFALSE(overWrite)){
    geom <- read_csv(file)
  }else{
    ### generate the file dataset 
    # active coal 
    d1 <- st_read("data/mines/Active_Coal_Permit.shp")%>%
      dplyr::filter(StatusDesc == "Active")%>%
      dplyr::mutate(class = "coal", id = PermitID)%>%
      dplyr::select(class, id)
    # active hardrock 
    d2 <- st_read("data/mines/ActiveMining.shp")%>%
      dplyr::filter(StatusDesc == "Active")%>%
      dplyr::mutate(class = "hardrock", id = PermitID)%>%
      dplyr::select(class, id)
    # active construction
    d3 <- st_read("data/mines/Active_Construction_Permit.shp")%>%
      dplyr::filter(StatusDesc == "Active")%>%
      dplyr::mutate(class = "construction", id = PermitID)%>%
      dplyr::select(class, id)
    # combine features 
    d4  <- bind_rows(d1,d2,d3)
    
    # Intersection and buffer process -----------------------------------------
    geom2 <- st_transform(geometry, crs = st_crs(5070))%>% select(GEOID)
    d5 <- st_transform(d4, crs = st_crs(5070))
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
      dplyr::distinct(GEOID, id, .keep_all = TRUE)%>%
      group_by(GEOID) %>% 
      summarise(bufferFeature_score = sum(weight, na.rm = TRUE)) %>%
      dplyr::select(GEOID, mining = bufferFeature_score) 
    
    geom <- left_join(st_drop_geometry(geom2), geom, by = "GEOID")%>%
      dplyr::mutate(
        mining = case_when(
          is.na(mining) ~ 0,
          TRUE ~ mining
        )
      )
    
    write_csv(geom, file = file )
    
  }
  # 
  return(geom)
}


