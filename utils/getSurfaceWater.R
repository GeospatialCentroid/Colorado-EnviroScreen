

getSurfaceWater <- function(filePath,processingLevel, geometry, overWrite = FALSE, compile = FALSE){
  file <- paste0("data/sufaceWater/",processingLevel,"_surfaceWater.csv")
  #compile results 
  if(isTRUE(compile)){
    files <- list.files(path = "data/sufaceWater/outputTemp", pattern = processingLevel,full.names = TRUE) %>%
      lapply(read_csv)%>%
      bind_rows()
    write_csv(files, file = file )
    
  }
  # run the process 
  if(file.exists(file) & isFALSE(overWrite)){
    geom <- read_csv(file)
  }else{
  ### Import data
  # 303(d) stream data layer
  streams <- st_read(filePath) %>%
    filter(substr(AUID, 1, 2) == "CO") # removing 4 "Lake" obs
  
  ### Data wrangling ----
  #   - create new variables for use assignments, assessment status, 
  #     impairment status, fully supported status, etc.
  
  stream_uses <- streams %>%
    mutate( # Uses
      AgUse = ifelse(Ag == "NA", 0, 1),
      AQLifeUse = ifelse(AQLife == "NA", 0, 1),
      RecUse = ifelse(Rec == "NA", 0, 1),
      WSUse = ifelse(WS == "NA", 0, 1),
      TotalUses = AgUse+AQLifeUse+RecUse+WSUse,
      # Impairment
      ImpairedUse = ifelse(X303d_Uses_ > 0, 1, 0),
      ImpairedUse_char = as.character(ImpairedUse),
      PercentUsesImpaired = 100*X303d_Uses_/TotalUses,
      # Assessment status
      AgAssessed = ifelse(Ag == "X"| Ag == "NA", 0, 1), 
      AQLifeAssessed = ifelse(AQLife == "X"| AQLife == "NA", 0, 1), 
      RecAssessed = ifelse(Rec == "X"| Rec == "NA", 0, 1), 
      WSAssessed = ifelse(WS == "X"| WS == "NA", 0, 1), 
      TotalAssessed = AgAssessed+AQLifeAssessed+RecAssessed+WSAssessed,
      Assessed = ifelse(TotalAssessed > 0, 1, 0),
      Assessed_char = as.character(Assessed)) 
  
  #### Overlay streams and geographic boundaries ----
  geometry <- geometry %>% st_transform(crs = st_crs(stream_uses))
  
  
  geom <- data.frame(matrix(nrow = nrow(geometry), ncol = 2))
  names(geom) <- c("GEOID", "surfaceWater")
  for(i in seq_along(geometry$STATEFP)){
    print(i)
    g1 <- geometry[i, ]
    geom$GEOID[i] <- g1$GEOID
    
    overlay <- st_intersection(stream_uses, g1) # very slow 
    if(nrow(overlay)==0){
      geom$surfaceWater[i] <- NA
    }else{
      overlay$seglength <- st_length(overlay) 
      geom$surfaceWater[i] <- overlay %>%
        st_drop_geometry() %>% # drop stream segment geometry for faster processing.
        mutate(
          #convert segment length in meters to miles
          stream_mi = as.numeric(seglength)*0.000621, 
          
          # Calculate the numerator for average percent impaired:
          # Stream segment length multiplied by the percent of uses impaired.
          # These will be added together for the entire county in the
          # "summarise" step below. 
          numerator_impaired = stream_mi*PercentUsesImpaired,
          
          # Calculate the numerator for percent unassessed
          # Stream segment length for completely unassessed streams. 
          # These will be added together for the entire county in the 
          # "summarise" step below.
          numerator_completelyunassessed = ifelse(Assessed == 0, stream_mi, 0))%>%
        group_by(GEOID) %>% # calculate measures for each county
        summarise(TotalStreamLengthMi = sum(stream_mi),
                  numerator_impaired = sum(numerator_impaired),
                  numerator_completelyunassessed = sum(numerator_completelyunassessed)) %>%
        ungroup() %>%
        mutate(AvgPercentImpaired = numerator_impaired/TotalStreamLengthMi) %>%
        # Add county spatial boundaries to dataframe
        dplyr::select(surfaceWater = AvgPercentImpaired)
    }
    
    if(i %% 10 ==0){
      df2 <- geom[(i-10):i, ] %>% dplyr::mutate(surfaceWater = as.character(surfaceWater))
      write_csv(x = df2,
              file = paste0("data/sufaceWater/outputTemp/surfaceWater",i - 10, "_",i,"_",processingLevel,".csv")
      )
    }
  }
  ### need to determine if we want to consider no stream a NA or a zero 
  # geom <- left_join(st_drop_geometry(geometry), geom, by = "GEOID")%>%
  #   dplyr::mutate(
  #     surfaceWater = case_when(
  #       is.na(surfaceWater) ~ 0,
  #       TRUE ~ surfaceWater
  #     )
  #   )
  write_csv(geom, file = file )
  }
  return(geom)
}