

getHousingBurden <- function(processingLevel, geometry, overWrite = FALSE){
  pathToData <- paste0("data/housingBurden/housingBurden_",processingLevel,".csv")
  
  if(file.exists(pathToData) & overWrite == FALSE){
    return(read_csv(pathToData))
  }else{
    # change the geometry character to match requirements in tidy census
    if(processingLevel == "censusBlockGroup"){
      processingLevel <- "block group"
    }
    if(processingLevel == "county"){
      processingLevel <- "county"
    }
    if(processingLevel == "censusTract"){
      processingLevel <- "tract"
    }
    
    d1 <- get_acs(geography = processingLevel,
                     variables = c("B25070_001", # Total Renters
                                   "B25070_007", # 30 to 34.9%
                                   "B25070_008", # 35 to 39.9%
                                   "B25070_009", # 40 to 49.9%
                                   "B25070_010", # 50% or more
                                   "B25091_001", # total owner-occupied,
                                   # "B25003_002", # confirmation of previous var - total owner occupied,
                                   "B25091_008", # 30 to 34.9% - mortgaged
                                   "B25091_009", # 35 to 39.9% - mortgaged
                                   "B25091_010", # 40 to 49.9% - mortgaged
                                   "B25091_011", # 50% or more - mortgaged
                                   "B25091_019", # 30 to 34.9% - not mortgaged
                                   "B25091_020", # 35 to 39.9% - not mortgaged
                                   "B25091_021", # 40 to 49.9% - not mortgaged
                                   "B25091_022" ), # 50% or more - not mortgaged
                     state = "08",
                     year = 2019)%>%
      select(-moe) %>%
      spread(key = variable, value = estimate)%>%
      mutate(
             HHUnits = B25070_001+B25091_001, # renter total + owner total
             HH_Burdened = B25070_007+B25070_008+B25070_009+B25070_010+
               B25091_008+B25091_009+B25091_010+B25091_011+
               B25091_019+B25091_020+B25091_021+B25091_022, # >30% renters, mortgaged, nonmortgaged
             HH_Burdened_Pct = HH_Burdened/HHUnits)%>%
      dplyr::select(GEOID,HH_Burdened_Pct)
    write_csv(d1, file = pathToData)
  }
}