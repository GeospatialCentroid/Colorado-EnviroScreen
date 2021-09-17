###
# precessFACE dataset from CDPHE 
# carverd@colostate.edu
# 20210915
### 

# temp 
d1 <- read.csv("data/face/FACE_results_population_scenarios1.csv")
d2 <- read.csv("data/face/county_level_eads.csv")

climate <- merge(d2, d1, by = "NAME")

climate <- climate %>%
  select(ï..GEOID.x,
         NAME,
         High, #population estimate in the high population growth scenario
         Flood_Bridge=F_IN_BRI_C2_P3,
         Drought_Crops=D_AG_CRO_C2_P3,
         Drought_Raft=D_RE_BOA_C2_P3,
         Drought_Ski=D_RE_SKI_C2_P3,
         Drought_Cattle=D_AG_LIV_C2_P3,
         Flood_Building=F_IN_RES_C2_P3,
         Wildfire_Sup=W_EC_SUP_C2_P3,
         Wildfire_Buildings=W_IN_RES_C2_P3) %>%
  mutate(CountyFIPS = paste0("0", as.character(ï..GEOID.x)),
         across(where(is.numeric),
                .fns = list(PerCapita= ~./High),
                .names = "{col}_{fn}"))


# need to determine connection to geoid and how to summarize inputs 

