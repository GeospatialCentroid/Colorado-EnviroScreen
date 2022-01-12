###
# define areas of specific environmental health burden
# carverd@colostate.edu
# 20211018
# adapted from work by margaret.horton@state.co.us
###

# STATUTE SAYS CENSUS BLOCK GROUPS WITH...
# >40% HOUSEHOLDS ARE LOW INCOME (200% OF FEDERAL POVERTY GUIDELINE OR LESS)
# >40% HOUSEHOLDS ARE MINORITY
# >40% HOUSEHOLDS THAT ARE "HOUSING COST-BURDENED" (30% OR MORE OF HOUSEHOLD INCOME SPENT ON HOUSING)



getDI <- function(overWrite = FALSE){
  # overWrite defines if you want to force the file to be recreated.

  pathToData <- "data/diCommunities/diCommunities.shp"

  if(file.exists(pathToData) & overWrite == FALSE){
    return(paste0("The DI community spatial data exists and can be found ", pathToData))
  }else{
    bg_co <- get_acs(geography = "block group",
                     variables = c("B01001_001", # total pop, age&sex
                                   "C17002_001", # total pop whose income to poverty ratio was determined
                                   "C17002_002", # total pop under 200% FPL (next few rows)
                                   "C17002_003",
                                   "C17002_004",
                                   "C17002_005",
                                   "C17002_006",
                                   "C17002_007",
                                   "C17002_008", # total pop at or above 200% FLP
                                   "B03002_001", # total pop, race
                                   "B03002_003", # population - white alone, non-Hispanic
                                   "B25070_001", # Total Renters
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
                                   "B25091_022", # 50% or more - not mortgaged
                                   "B25002_002"), # total occupied units

                     state = "08",
                     year = 2019) %>%
      select(-moe) %>%
      spread(key = variable, value = estimate)%>%
      mutate(TotalPop = B01001_001,
             WhitePop = B03002_003, # White, non-Hispanic population
             MinPop = TotalPop-WhitePop, # all people that are not white, non-Hispanic
             Pov_PCT =  (C17002_001 - C17002_008)/C17002_001, # % of pop < 200% FPL (whose income:poverty could be determined)
             Min_PCT = MinPop/TotalPop, # percent of population that are non-white
             Min_FLAG = ifelse(Min_PCT > .4, 1, 0), # DI community flags
             FLP_FLAG = ifelse(Pov_PCT > .4, 1, 0),
             HHUnits = B25070_001+B25091_001, # renter total + owner total
             HH_Burdened = B25070_007+B25070_008+B25070_009+B25070_010+
               B25091_008+B25091_009+B25091_010+B25091_011+
               B25091_019+B25091_020+B25091_021+B25091_022, # >30% renters, mortgaged, nonmortgaged
             HH_Burdened_Pct = HH_Burdened/HHUnits,
             Burdened_FLAG = ifelse(HH_Burdened_Pct > .4, 1, 0),
             COUNTYFIPS = substr(GEOID, 1,5)
      )%>%
      dplyr::select(GEOID,Min_PCT,Min_FLAG,Pov_PCT,FLP_FLAG,HH_Burdened_Pct,Burdened_FLAG)%>%
      dplyr::rowwise()%>%
      dplyr::mutate(
        DI_communityCount = sum(c(Min_FLAG,
                                  FLP_FLAG,
                                  Burdened_FLAG)),
        DI_community = case_when(
          DI_communityCount != 0 ~ 1,
          TRUE ~ 0
        )
      )

    # read in geometry for census block groups
    geom <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")%>%
      dplyr::select(GEOID)%>%
      dplyr::left_join(bg_co, by = "GEOID")

    #write feature
    sf::st_write(obj = geom, dsn = file,delete_dsn = TRUE)
    # save as rda
    di <- sf::st_read(pathToData) %>%
      dplyr::filter(Mn_FLAG != 0 | FLP_FLA != 0 | Br_FLAG !=0 )
    saveRDS(object = di, file = "data/diCommunities/diCommunities.rda")
    return(paste0("The DI community spatial data was writen ", file))
  }
}
